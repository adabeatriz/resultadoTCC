/**
 * Classe principal que gerencia operações relacionadas ao Git no NetBeans.
 * Fornece funcionalidades para gerenciar repositórios, status de arquivos e notificações de mudanças.
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

    // Constantes públicas
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName"); 
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged"; // Nome da propriedade para mudanças em anotações
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged"; // Nome da propriedade para mudanças em arquivos versionados
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged"; // Nome da propriedade para mudanças no conjunto de alterações
    public static final Logger LOG = Logger.getLogger("org.nbgit"); // Logger para esta classe
    
    // Máscara de bits para status que permitem diff
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;
    
    private final PropertyChangeSupport support = new PropertyChangeSupport(this); // Suporte para notificação de mudanças
    private final StatusCache statusCache = new StatusCache(this); // Cache de status dos arquivos
    private HashMap<String, RequestProcessor> processorsToUrl; // Mapeamento de processadores por URL
    private final Map<File, Repository> repos = new HashMap<File, Repository>(); // Mapeamento de repositórios Git
    private static Git instance; // Instância singleton desta classe

    /**
     * Construtor privado para implementação do padrão Singleton.
     */
    private Git() {
    }

    /**
     * Obtém a instância singleton da classe Git.
     * @return A instância única da classe Git
     */
    public static synchronized Git getInstance() {
        if (instance == null) {
            instance = new Git();
        }
        return instance;
    }

    /**
     * Obtém o repositório Git associado ao diretório raiz especificado.
     * @param root Diretório raiz do repositório
     * @return O objeto Repository correspondente ou null se não existir
     */
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);

        if (repo == null) {
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                repo = new Repository(gitDir);
                repos.put(root, repo);
            } catch (IOException ex) {
                // Ignora exceção se não conseguir criar o repositório
            }
        }

        return repo;
    }

    /**
     * Obtém o cache de status dos arquivos.
     * @return O objeto StatusCache
     */
    public StatusCache getStatusCache() {
        return statusCache;
    }

    /**
     * Verifica se um arquivo/diretório é administrativo do Git.
     * @param file Arquivo/diretório a ser verificado
     * @return true se for um diretório administrativo do Git
     */
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    /**
     * Verifica se um nome de arquivo é administrativo do Git.
     * @param fileName Nome do arquivo a ser verificado
     * @return true se for um nome administrativo do Git (.git)
     */
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git"); 
    }

    /**
     * Verifica se um arquivo está sob controle de versão do Git.
     * @param file Arquivo a ser verificado
     * @return true se o arquivo estiver versionado e não for parte dos metadados do Git
     */
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    /**
     * Obtém o diretório pai mais alto que está sob controle de versão do Git.
     * @param file Arquivo/diretório para encontrar o pai versionado
     * @return O diretório pai mais alto versionado ou null se não encontrado
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
     * Obtém o tipo MIME de um arquivo, considerando seu status no Git.
     * @param file Arquivo para obter o tipo MIME
     * @return O tipo MIME do arquivo
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
     * Notifica que houve mudanças nos arquivos versionados.
     */
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    /**
     * Atualiza todas as anotações do controle de versão.
     */
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    /**
     * Notifica que houve mudanças no conjunto de alterações de um repositório.
     * @param repository Repositório que teve mudanças
     */
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    /**
     * Adiciona um listener para mudanças de propriedades.
     * @param listener Listener a ser adicionado
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    /**
     * Remove um listener de mudanças de propriedades.
     * @param listener Listener a ser removido
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    /**
     * Obtém a versão original de um arquivo no repositório.
     * @param workingCopy Arquivo de trabalho local
     * @param originalFile Arquivo de destino onde será gravado o conteúdo original
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
     * Obtém um RequestProcessor padrão para operações assíncronas.
     * @return RequestProcessor padrão
     */
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    /**
     * Obtém um RequestProcessor associado a um arquivo específico.
     * @param file Arquivo para associar ao RequestProcessor
     * @return RequestProcessor associado ao arquivo
     */
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    /**
     * Obtém um RequestProcessor associado a uma URL específica.
     * @param url URL para associar ao RequestProcessor
     * @return RequestProcessor associado à URL
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
     * Remove o RequestProcessor associado a uma URL específica.
     * @param url URL do RequestProcessor a ser removido
     */
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null & url != null) {
            processorsToUrl.remove(url);
        }
    }
}