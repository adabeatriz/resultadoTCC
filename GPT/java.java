package org.nbgit;

// Importações necessárias
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.nbgit.util.GitUtils;
import org.netbeans.modules.versioning.spi.VersioningSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.RequestProcessor;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Repository;

public class Git {

    // Constantes usadas para eventos e identificação
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName");
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged";
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged";
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged";
    
    // Logger para saída de mensagens de log
    public static final Logger LOG = Logger.getLogger("org.nbgit");

    // Máscara de bits que representa os estados em que um arquivo pode ser comparado (diffable)
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;

    // Suporte para notificações de mudanças de propriedades (observers)
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);

    // Cache de status de arquivos versionados
    private final StatusCache statusCache = new StatusCache(this);

    // Mapeamento entre URLs e processadores de requisições
    private HashMap<String, RequestProcessor> processorsToUrl;

    // Armazena repositórios Git associados a diretórios
    private final Map<File, Repository> repos = new HashMap<File, Repository>();

    // Instância única da classe (singleton)
    private static Git instance;

    // Construtor privado para implementar o padrão singleton
    private Git() {
    }

    // Método para obter a instância única da classe Git
    public static synchronized Git getInstance() {
        if (instance == null) {
            instance = new Git();
        }
        return instance;
    }

    // Retorna o repositório Git associado ao diretório raiz informado
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);
        if (repo == null) {
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                repo = new Repository(gitDir);
                repos.put(root, repo);
            } catch (IOException ex) {
                // Falha ao criar o repositório
            }
        }
        return repo;
    }

    // Retorna o cache de status
    public StatusCache getStatusCache() {
        return statusCache;
    }

    // Verifica se o diretório é administrativo (.git)
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    // Verifica se o nome do arquivo é ".git"
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git");
    }

    // Verifica se o arquivo é gerenciado pelo controle de versão Git
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    // Retorna o diretório mais externo que é gerenciado pelo Git
    public File getTopmostManagedParent(File file) {
        if (GitUtils.isPartOfGitMetadata(file)) {
            for (; file != null; file = file.getParentFile()) {
                if (isAdministrative(file)) {
                    file = file.getParentFile();
                    break;
                }
            }
        }

        File topmost = null;
        for (; file != null; file = file.getParentFile()) {
            if (org.netbeans.modules.versioning.util.Utils.isScanForbidden(file)) {
                break;
            }
            if (new File(file, ".git").canWrite()) {
                topmost = file;
                break;
            }
        }
        return topmost;
    }

    // Retorna o tipo MIME de um arquivo, levando em consideração se é binário ou texto
    public String getMimeType(File file) {
        FileObject fo = FileUtil.toFileObject(file);
        String foMime;

        if (fo == null) {
            foMime = "content/unknown";
        } else {
            foMime = fo.getMIMEType();
            if ("content/unknown".equals(foMime)) {
                foMime = "text/plain";
            }
        }

        if ((statusCache.getStatus(file).getStatus() & StatusInfo.STATUS_VERSIONED) == 0) {
            return GitUtils.isFileContentBinary(file) ? "application/octet-stream" : foMime;
        } else {
            return foMime;
        }
    }

    // Dispara evento informando que arquivos versionados foram alterados
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    // Dispara evento para atualizar anotações visuais
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    // Dispara evento indicando que o changeset do repositório mudou
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    // Adiciona um ouvinte para mudanças de propriedade
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    // Remove um ouvinte de mudanças de propriedade
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    // Obtém a cópia original de um arquivo modificado localmente
    public void getOriginalFile(File workingCopy, File originalFile) {
        StatusInfo info = statusCache.getStatus(workingCopy);
        LOG.log(Level.FINE, "getOriginalFile: {0} {1}", new Object[]{workingCopy, info});
        
        if ((info.getStatus() & STATUS_DIFFABLE) == 0) {
            return;        
        }

        try {
            File original = GitUtils.getFileRevision(workingCopy, GitRepository.REVISION_BASE);
            if (original == null) {
                return;
            }
            // Copia o conteúdo da versão original para o arquivo de saída
            org.netbeans.modules.versioning.util.Utils.copyStreamsCloseAll(
                new FileOutputStream(originalFile), new FileInputStream(original));
            original.delete(); // Exclui o arquivo temporário original
        } catch (IOException e) {
            Logger.getLogger(Git.class.getName()).log(Level.INFO, "Unable to get original file", e);
        }
    }

    // Obtém um processador de requisições (thread separada)
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    // Sobrecarga que aceita um arquivo como base para gerar o processador
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    // Obtém um processador de requisições associado a uma URL ou cria um novo se não existir
    public RequestProcessor getRequestProcessor(String url) {
        if (processorsToUrl == null) {
            processorsToUrl = new HashMap<String, RequestProcessor>();
        }

        String key = (url != null) ? url : "ANY_URL";

        RequestProcessor rp = processorsToUrl.get(key);
        if (rp == null) {
            rp = new RequestProcessor("Git - " + key, 1, true); // Cria com 1 thread
            processorsToUrl.put(key, rp);
        }

        return rp;
    }

    // Limpa o processador associado a uma URL específica
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null & url != null) {
            processorsToUrl.remove(url);
        }
    }
}