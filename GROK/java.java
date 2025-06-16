package org.nbgit;

// Importações necessárias para manipulação de arquivos, eventos, logging e integração com Git e NetBeans
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

// Classe principal para integração do Git com o NetBeans
public class Git {

    // Título da aba de saída do Git no NetBeans, obtido do recurso de internacionalização
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName");
    
    // Constantes para eventos de mudança de estado
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged"; // Evento para mudanças em anotações
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged"; // Evento para mudanças em arquivos versionados
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged"; // Evento para mudanças no changeset
    
    // Logger para registro de eventos e erros
    public static final Logger LOG = Logger.getLogger("org.nbgit");
    
    // Máscara de bits para status de arquivos que podem ser comparados (diff)
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;
    
    // Suporte para notificação de mudanças de propriedades
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);
    
    // Cache de status dos arquivos versionados
    private final StatusCache statusCache = new StatusCache(this);
    
    // Mapeamento de URLs para processadores de requisições
    private HashMap<String, RequestProcessor> processorsToUrl;
    
    // Mapeamento de diretórios para repositórios Git
    private final Map<File, Repository> repos = new HashMap<File, Repository>();
    
    // Instância singleton da classe Git
    private static Git instance;

    // Construtor privado para implementar o padrão Singleton
    private Git() {
    }

    // Obtém a instância singleton da classe Git
    public static synchronized Git getInstance() {
        if (instance == null) {
            instance = new Git();
        }
        return instance;
    }

    // Obtém ou cria um repositório Git para o diretório raiz especificado
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);

        if (repo == null) {
            // Cria um objeto File para o diretório .git
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                // Inicializa o repositório Git
                repo = new Repository(gitDir);
                // Armazena no cache de repositórios
                repos.put(root, repo);
            } catch (IOException ex) {
                // Exceção tratada silenciosamente, retorna null se falhar
            }
        }

        return repo;
    }

    // Retorna o cache de status dos arquivos
    public StatusCache getStatusCache() {
        return statusCache;
    }

    // Verifica se o arquivo é um diretório administrativo do Git (ex.: .git)
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    // Verifica se o nome do arquivo corresponde a um diretório administrativo (.git)
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git"); 
    }

    // Verifica se o arquivo está sob controle de versão do Git
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    // Encontra o diretório raiz mais alto que está sob controle de versão Git
    public File getTopmostManagedParent(File file) {
        // Ignora arquivos que fazem parte dos metadados do Git
        if (GitUtils.isPartOfGitMetadata(file)) {
            for (; file != null; file = file.getParentFile()) {
                if (isAdministrative(file)) {
                    file = file.getParentFile();
                    break;
                }
            }
        }
        File topmost = null;
        // Percorre a hierarquia de diretórios até encontrar um com .git
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

    // Determina o tipo MIME do arquivo
    public String getMimeType(File file) {
        // Converte o arquivo para um FileObject do NetBeans
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
        // Verifica se o arquivo não está versionado
        if ((statusCache.getStatus(file).getStatus() & StatusInfo.STATUS_VERSIONED) == 0) {
            // Determina se o arquivo é binário ou texto
            return GitUtils.isFileContentBinary(file) ? "application/octet-stream" : foMime;
        } else {
            return foMime;
        }
    }

    // Dispara evento de mudança nos arquivos versionados
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    // Dispara evento de atualização de todas as anotações
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    // Dispara evento de mudança no changeset de um repositório
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    // Adiciona um ouvinte para eventos de mudança de propriedades
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    // Remove um ouvinte de eventos de mudança de propriedades
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    // Obtém a versão original de um arquivo versionado
    public void getOriginalFile(File workingCopy, File originalFile) {
        StatusInfo info = statusCache.getStatus(workingCopy);
        // Registra informações de depuração
        LOG.log(Level.FINE, "getOriginalFile: {0} {1}", new Object[]{workingCopy, info});
        // Verifica se o arquivo é elegível para comparação
        if ((info.getStatus() & STATUS_DIFFABLE) == 0) {
            return;
        }
        try {
            // Obtém a revisão base do arquivo
            File original = GitUtils.getFileRevision(workingCopy, GitRepository.REVISION_BASE);
            if (original == null) {
                return;
            }
            // Copia o conteúdo do arquivo original para o destino
            org.netbeans.modules.versioning.util.Utils.copyStreamsCloseAll(new FileOutputStream(originalFile), new FileInputStream(original));
            // Remove o arquivo temporário original
            original.delete();
        } catch (IOException e) {
            // Registra erro ao obter o arquivo original
            Logger.getLogger(Git.class.getName()).log(Level.INFO, "Unable to get original file", e);
        }
    }

    // Obtém um processador de requisições genérico
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    // Obtém um processador de requisições para um arquivo específico
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    // Obtém ou cria um processador de requisições para uma URL
    public RequestProcessor getRequestProcessor(String url) {
        if (processorsToUrl == null) {
            processorsToUrl = new HashMap<String, RequestProcessor>();
        }
        String key;
        if (url != null) {
            key = url;
        } else {
            key = "ANY_URL";
        }
        RequestProcessor rp = processorsToUrl.get(key);
        if (rp == null) {
            // Cria um novo processador de requisições para a chave especificada
            rp = new RequestProcessor("Git - " + key, 1, true); 
            processorsToUrl.put(key, rp);
        }
        return rp;
    }

    // Remove um processador de requisições associado a uma URL
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null && url != null) {
            processorsToUrl.remove(url);
        }
    }
}