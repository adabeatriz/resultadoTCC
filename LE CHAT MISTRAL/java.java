package org.nbgit;

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

// Classe principal para manipulação de operações Git
public class Git {

    // Título da aba de saída do Git
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName");

    // Constantes para propriedades de mudança
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged";
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged";
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged";

    // Logger para registrar eventos e erros
    public static final Logger LOG = Logger.getLogger("org.nbgit");

    // Máscara de status para arquivos que podem ser comparados (diff)
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;

    // Suporte para notificação de mudanças de propriedades
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);

    // Cache para armazenar o status dos arquivos
    private final StatusCache statusCache = new StatusCache(this);

    // Mapa para armazenar processadores de requisições associados a URLs
    private HashMap<String, RequestProcessor> processorsToUrl;

    // Mapa para armazenar repositórios Git
    private final Map<File, Repository> repos = new HashMap<File, Repository>();

    // Instância singleton da classe Git
    private static Git instance;

    // Construtor privado para garantir o padrão singleton
    private Git() {
    }

    // Método para obter a instância singleton da classe Git
    public static synchronized Git getInstance() {
        if (instance == null) {
            instance = new Git();
        }
        return instance;
    }

    // Método para obter o repositório Git associado a um diretório raiz
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);

        if (repo == null) {
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                repo = new Repository(gitDir);
                repos.put(root, repo);
            } catch (IOException ex) {
                // Tratar exceção conforme necessário
            }
        }

        return repo;
    }

    // Método para obter o cache de status
    public StatusCache getStatusCache() {
        return statusCache;
    }

    // Método para verificar se um arquivo é administrativo (diretório .git)
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    // Método para verificar se um nome de arquivo é administrativo
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git"); 
    }

    // Método para verificar se um arquivo é gerenciado pelo Git
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    // Método para obter o diretório pai mais alto gerenciado pelo Git
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

    // Método para obter o tipo MIME de um arquivo
    public String getMimeType(File file) {
        FileObject fo = FileUtil.toFileObject(file);
        String foMime;
        if (fo == null) {
            foMime = "content/unknown";
        } else {
            foMime = fo.getMIMEType();
            if ("content/unknown".equals(foMime)) 
            {
                foMime = "text/plain";
            }
        }
        if ((statusCache.getStatus(file).getStatus() & StatusInfo.STATUS_VERSIONED) == 0) {
            return GitUtils.isFileContentBinary(file) ? "application/octet-stream" : foMime;
        } else {
            return foMime;
        }
    }

    // Método para notificar que os arquivos versionados foram alterados
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    // Método para atualizar todas as anotações
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    // Método para notificar que o conjunto de alterações foi modificado
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    // Método para adicionar um ouvinte de mudança de propriedade
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    // Método para remover um ouvinte de mudança de propriedade
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    // Método para obter o arquivo original de uma cópia de trabalho
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
            org.netbeans.modules.versioning.util.Utils.copyStreamsCloseAll(new FileOutputStream(originalFile), new FileInputStream(original));
            original.delete();
        } catch (IOException e) {
            Logger.getLogger(Git.class.getName()).log(Level.INFO, "Unable to get original file", e);
        }
    }

    // Método para obter um processador de requisições
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    // Método para obter um processador de requisições associado a um arquivo
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    // Método para obter um processador de requisições associado a uma URL
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
            rp = new RequestProcessor("Git - " + key, 1, true); 
            processorsToUrl.put(key, rp);
        }
        return rp;
    }

    // Método para limpar o processador de requisições associado a uma URL
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null & url != null) {
            processorsToUrl.remove(url);
        }
    }
}
