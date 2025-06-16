package org.nbgit;

// Importações necessárias para funcionalidades de beans, I/O, logging e Git
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

/**
 * Classe principal que gerencia a integração do Git com o NetBeans IDE.
 * Implementa o padrão Singleton para garantir uma única instância global.
 * Responsável por gerenciar repositórios, cache de status e operações Git.
 */
public class Git {

    // Constantes para identificação de propriedades e configurações
    /** Título da aba de saída do Git no NetBeans */
    public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName"); 
    
    /** Propriedade que indica mudanças nas anotações dos arquivos */
    public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged"; 
    
    /** Propriedade que indica mudanças nos arquivos versionados */
    public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged"; 
    
    /** Propriedade que indica mudanças no changeset do repositório */
    public static final String PROP_CHANGESET_CHANGED = "changesetChanged"; 
    
    /** Logger para registrar eventos e debug da integração Git */
    public static final Logger LOG = Logger.getLogger("org.nbgit");
    
    /**
     * Máscara de bits que define quais status de arquivo permitem comparação (diff).
     * Inclui arquivos atualizados, modificados localmente, em conflito, etc.
     */
    private static final int STATUS_DIFFABLE =
            StatusInfo.STATUS_VERSIONED_UPTODATE |
            StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_CONFLICT |
            StatusInfo.STATUS_VERSIONED_MERGE |
            StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY |
            StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY;

    // Campos da instância
    /** Suporte para notificação de mudanças de propriedades */
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);
    
    /** Cache para armazenar status dos arquivos e otimizar consultas */
    private final StatusCache statusCache = new StatusCache(this);
    
    /** Mapa que associa URLs a seus respectivos processadores de requisições */
    private HashMap<String, RequestProcessor> processorsToUrl;
    
    /** Cache de repositórios Git indexados por diretório raiz */
    private final Map<File, Repository> repos = new HashMap<File, Repository>();
    
    /** Instância única da classe (padrão Singleton) */
    private static Git instance;

    /**
     * Construtor privado para implementar o padrão Singleton.
     * Impede a criação direta de instâncias da classe.
     */
    private Git() {
    }

    /**
     * Retorna a instância única da classe Git (padrão Singleton).
     * Cria a instância na primeira chamada e a reutiliza nas demais.
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
     * Obtém ou cria um repositório Git associado ao diretório raiz especificado.
     * Utiliza cache para evitar recriar repositórios já existentes.
     * 
     * @param root diretório raiz do repositório Git
     * @return objeto Repository do JGit ou null se houver erro
     */
    public Repository getRepository(File root) {
        Repository repo = repos.get(root);

        // Se o repositório não está no cache, tenta criá-lo
        if (repo == null) {
            final File gitDir = new File(root, Constants.DOT_GIT);
            try {
                repo = new Repository(gitDir);
                repos.put(root, repo); // Adiciona ao cache
            } catch (IOException ex) {
                // Falha silenciosa - retorna null se não conseguir criar
            }
        }

        return repo;
    }

    /**
     * Retorna o cache de status dos arquivos.
     * 
     * @return instância do StatusCache
     */
    public StatusCache getStatusCache() {
        return statusCache;
    }

    /**
     * Verifica se um arquivo é administrativo do Git (diretório .git).
     * Um arquivo é considerado administrativo se for um diretório com nome específico.
     * 
     * @param file arquivo a ser verificado
     * @return true se for um diretório administrativo do Git
     */
    public boolean isAdministrative(File file) {
        String name = file.getName();
        return isAdministrative(name) && file.isDirectory();
    }

    /**
     * Verifica se um nome de arquivo corresponde a um diretório administrativo do Git.
     * 
     * @param fileName nome do arquivo a ser verificado
     * @return true se o nome for ".git"
     */
    public boolean isAdministrative(String fileName) {
        return fileName.equals(".git"); 
    }

    /**
     * Verifica se um arquivo é gerenciado pelo sistema de versionamento Git.
     * Um arquivo é gerenciado se pertence a um repositório Git e não faz parte dos metadados.
     * 
     * @param file arquivo a ser verificado
     * @return true se o arquivo for gerenciado pelo Git
     */
    public boolean isManaged(File file) {
        return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
    }

    /**
     * Encontra o diretório pai mais alto que é gerenciado pelo Git.
     * Sobe na hierarquia de diretórios até encontrar a raiz do repositório Git.
     * 
     * @param file arquivo de partida para a busca
     * @return diretório raiz do repositório Git ou null se não encontrado
     */
    public File getTopmostManagedParent(File file) {
        // Se o arquivo faz parte dos metadados do Git, sobe até sair dessa área
        if (GitUtils.isPartOfGitMetadata(file)) {
            for (; file != null; file = file.getParentFile()) {
                if (isAdministrative(file)) {
                    file = file.getParentFile();
                    break;
                }
            }
        }
        
        // Procura o diretório raiz do repositório Git
        File topmost = null;
        for (; file != null; file = file.getParentFile()) {
            // Interrompe se encontrar um diretório proibido para scan
            if (org.netbeans.modules.versioning.util.Utils.isScanForbidden(file)) {
                break;
            }
            // Verifica se existe um diretório .git com permissão de escrita
            if (new File(file, ".git").canWrite()) { 
                topmost = file;
                break;
            }
        }
        return topmost;
    }

    /**
     * Determina o tipo MIME de um arquivo baseado em seu status no Git e conteúdo.
     * Para arquivos não versionados, verifica se o conteúdo é binário.
     * 
     * @param file arquivo para determinar o tipo MIME
     * @return string representando o tipo MIME do arquivo
     */
    public String getMimeType(File file) {
        FileObject fo = FileUtil.toFileObject(file);
        String foMime;
        
        // Obtém o tipo MIME do arquivo
        if (fo == null) {
            foMime = "content/unknown";
        } else {
            foMime = fo.getMIMEType();
            // Se o tipo for desconhecido, assume texto simples
            if ("content/unknown".equals(foMime)) 
            {
                foMime = "text/plain";
            }
        }
        
        // Para arquivos não versionados, verifica se é binário
        if ((statusCache.getStatus(file).getStatus() & StatusInfo.STATUS_VERSIONED) == 0) {
            return GitUtils.isFileContentBinary(file) ? "application/octet-stream" : foMime;
        } else {
            return foMime;
        }
    }

    /**
     * Notifica que houve mudanças nos arquivos versionados.
     * Dispara evento para atualizar a interface do usuário.
     */
    public void versionedFilesChanged() {
        support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
    }

    /**
     * Força a atualização de todas as anotações de arquivos.
     * Dispara evento para recarregar anotações na interface.
     */
    public void refreshAllAnnotations() {
        support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
    }

    /**
     * Notifica que houve mudanças no changeset de um repositório.
     * 
     * @param repository diretório do repositório que sofreu mudanças
     */
    public void changesetChanged(File repository) {
        support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
    }

    /**
     * Adiciona um listener para mudanças de propriedades.
     * 
     * @param listener objeto que será notificado sobre mudanças
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    /**
     * Remove um listener de mudanças de propriedades.
     * 
     * @param listener objeto que não deve mais receber notificações
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    /**
     * Obtém a versão original (base) de um arquivo do repositório Git.
     * Copia o conteúdo da versão base para o arquivo de destino especificado.
     * 
     * @param workingCopy arquivo na cópia de trabalho
     * @param originalFile arquivo onde será salva a versão original
     */
    public void getOriginalFile(File workingCopy, File originalFile) {
        StatusInfo info = statusCache.getStatus(workingCopy);
        LOG.log(Level.FINE, "getOriginalFile: {0} {1}", new Object[]{workingCopy, info}); 
        
        // Verifica se o arquivo permite comparação (diff)
        if ((info.getStatus() & STATUS_DIFFABLE) == 0) {
            return;        
        }
        
        try {
            // Obtém a versão base do arquivo do repositório
            File original = GitUtils.getFileRevision(workingCopy, GitRepository.REVISION_BASE);
            if (original == null) {
                return;
            }
            
            // Copia o conteúdo da versão original para o arquivo de destino
            org.netbeans.modules.versioning.util.Utils.copyStreamsCloseAll(
                new FileOutputStream(originalFile), 
                new FileInputStream(original)
            );
            
            // Remove o arquivo temporário
            original.delete();
        } catch (IOException e) {
            Logger.getLogger(Git.class.getName()).log(Level.INFO, "Unable to get original file", e); 
        }
    }

    /**
     * Obtém um processador de requisições genérico (sem URL específica).
     * 
     * @return instância de RequestProcessor
     */
    public RequestProcessor getRequestProcessor() {
        return getRequestProcessor((String) null);
    }

    /**
     * Obtém um processador de requisições associado a um arquivo específico.
     * 
     * @param file arquivo para o qual obter o processador
     * @return instância de RequestProcessor
     */
    public RequestProcessor getRequestProcessor(File file) {
        return getRequestProcessor(file.getAbsolutePath());
    }

    /**
     * Obtém ou cria um processador de requisições para uma URL específica.
     * Utiliza cache para reutilizar processadores existentes e otimizar performance.
     * 
     * @param url URL para a qual obter o processador (pode ser null)
     * @return instância de RequestProcessor associada à URL
     */
    public RequestProcessor getRequestProcessor(String url) {
        // Inicializa o mapa de processadores se necessário
        if (processorsToUrl == null) {
            processorsToUrl = new HashMap<String, RequestProcessor>();
        }
        
        // Define a chave para o cache
        String key;
        if (url != null) {
            key = url;
        } else {
            key = "ANY_URL";
        }
        
        // Busca processador existente no cache
        RequestProcessor rp = processorsToUrl.get(key);
        if (rp == null) {
            // Cria novo processador com thread única e daemon
            rp = new RequestProcessor("Git - " + key, 1, true); 
            processorsToUrl.put(key, rp);
        }
        return rp;
    }

    /**
     * Remove um processador de requisições do cache para uma URL específica.
     * Útil para limpeza de recursos quando uma URL não é mais necessária.
     * 
     * @param url URL cujo processador deve ser removido
     */
    public void clearRequestProcessor(String url) {
        if (processorsToUrl != null & url != null) {
            processorsToUrl.remove(url);
        }
    }
}