/**
 * Classe principal para integração com o Git.
 */
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

public class Git {

    /**
     * Título da aba de saída do Git.
     */
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName");

    /**
     * Propriedade para mudanças nas anotações.
     */
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged";

    /**
     * Propriedade para mudanças nos arquivos versionados.
     */
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged";

    /**
     * Propriedade para mudanças no changeset.
     */
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged";

    /**
     * Logger para a classe Git.
     */
    public static final Logger LOG = Logger.getLogger("org.nbgit");

    /**
     * Máscara para status de arquivos que podem ser diffados.
     */
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;

    /**
     * Suporte para mudanças de propriedades.
     */
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);

    /**
     * Cache de status de arquivos.
     */
    private final StatusCache statusCache = new StatusCache(this);

    /**
     * Mapa de processadores de requisições por URL.
     */
    private HashMap<String, RequestProcessor> processorsToUrl;

    /**
     * Mapa de repositórios Git por raiz.
     */
    private final Map<File, Repository> repos = new HashMap<File, Repository>();

    /**
     * Instância única da classe Git.
     */
    private static Git instance;

    /**
     * Construtor privado para evitar instanciação direta.
     */
    private Git() {
    }

    /**
     * Obtém a instância única da classe Git.
     * 
     * @return a instância única da classe Git
     */
    public static synchronized Git getInstance() {
        if (instance == null) {
            instance = new Git();
        }
        return instance;
    }

    /**
     * Obtém o repositório Git para uma raiz específica.
     * 
     * @param root a raiz do repositório
     * @return o repositório Git
     */
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);

        if (repo == null) {
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                repo = new Repository(gitDir);
                repos.put(root, repo);
            } catch (IOException ex) {
                // Erro ao criar o repositório
            }
        }

        return repo;
    }

    /**
     * Obtém o cache de status de arquivos.
     * 
     * @return o cache de status de arquivos
     */
    public StatusCache getStatusCache() {
        return statusCache;
    }

    /**
     * Verifica se um arquivo é administrativo (diretório .git).
     * 
     * @param file o arquivo a verificar
     * @return true se o arquivo é administrativo, false caso contrário
     */
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    /**
     * Verifica se um arquivo é administrativo (nome .git).
     * 
     * @param fileName o nome do arquivo
     * @return true se o arquivo é administrativo, false caso contrário
     */
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git"); 
    }

    /**
     * Verifica se um arquivo é gerenciado pelo Git.
     * 
     * @param file o arquivo a verificar
     * @return true se o arquivo é gerenciado, false caso contrário
     */
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    /**
     * Obtém o pai mais alto gerenciado para um arquivo.
     * 
     * @param file o arquivo
     * @return o pai mais alto gerenciado
     */
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

    /**
     * Obtém o tipo MIME de um arquivo.
     * 
     * @param file o arquivo
     * @return o tipo MIME
     */
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

    /**
     * Notifica que os arquivos versionados mudaram.
     */
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    /**
     * Atualiza todas as anotações.
     */
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    /**
     * Notifica que o changeset mudou.
     * 
     * @param repository o repositório afetado
     */
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    /**
     * Adiciona um listener para mudanças de propriedades.
     * 
     * @param listener o listener
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    /**
     * Remove um listener para mudanças de propriedades.
     * 
     * @param listener o listener
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    /**
     * Obtém o arquivo original para um arquivo de trabalho.
     * 
     * @param workingCopy o arquivo de trabalho
     * @param originalFile o arquivo original
     */
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

    /**
     * Obtém um processador de requisições.
     * 
     * @return o processador de requisições
     */
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    /**
     * Obtém um processador de requisições para um arquivo específico.
     * 
     * @param file o arquivo
     * @return o processador de requisições
     */
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    /**
     * Obtém um processador de requisições para uma URL específica.
     * 
     * @param url a URL
     * @return o processador de requisições
     */
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

    /**
     * Limpa o processador de requisições para uma URL específica.
     * 
     * @param url a URL
     */
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null & url != null) {
            processorsToUrl.remove(url);
        }
    }
}
