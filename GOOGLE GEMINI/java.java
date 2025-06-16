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
 
 /**
  * Classe principal que representa o plugin Git para o NetBeans.
  * Implementa o padrão Singleton para garantir uma única instância.
  */
 public class Git {
 
	 // Constante para o título da aba de saída do Git
	 public static final String GIT_OUTPUT_TAB_TITLE = org.openide.util.NbBundle.getMessage(Git.class, "CTL_Git_DisplayName"); 
	 // Propriedade para notificar mudanças nas anotações
	 public static final String PROP_ANNOTATIONS_CHANGED = "annotationsChanged"; 
	 // Propriedade para notificar mudanças em arquivos versionados
	 public static final String PROP_VERSIONED_FILES_CHANGED = "versionedFilesChanged"; 
	 // Propriedade para notificar mudanças em conjuntos de alterações (changesets)
	 public static final String PROP_CHANGESET_CHANGED = "changesetChanged"; 
	 // Logger para registrar mensagens da classe
	 public static final Logger LOG = Logger.getLogger("org.nbgit");
	 
	 // Máscara de bits para determinar se um arquivo é "diffable" (pode ser comparado)
	 private static final int STATUS_DIFFABLE =
			 StatusInfo.STATUS_VERSIONED_UPTODATE | // Arquivo versionado e atualizado
			 StatusInfo.STATUS_VERSIONED_MODIFIEDLOCALLY | // Arquivo versionado e modificado localmente
			 StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY | // Arquivo versionado e modificado no repositório
			 StatusInfo.STATUS_VERSIONED_CONFLICT | // Arquivo versionado e em conflito
			 StatusInfo.STATUS_VERSIONED_MERGE | // Arquivo versionado e em processo de merge
			 StatusInfo.STATUS_VERSIONED_REMOVEDINREPOSITORY | // Arquivo versionado e removido no repositório
			 StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY | // Arquivo versionado e modificado no repositório (duplicado intencionalmente?)
			 StatusInfo.STATUS_VERSIONED_MODIFIEDINREPOSITORY; // Arquivo versionado e modificado no repositório (triplicado intencionalmente?)
	 
	 // Suporte para notificação de mudanças de propriedades
	 private final PropertyChangeSupport support = new PropertyChangeSupport(this);
	 // Cache de informações de status dos arquivos
	 private final StatusCache statusCache = new StatusCache(this);
	 // Mapa de processadores de requisições por URL (para operações assíncronas)
	 private HashMap<String, RequestProcessor> processorsToUrl;
	 // Mapa de repositórios Git, onde a chave é o diretório raiz do repositório
	 private final Map<File, Repository> repos = new HashMap<File, Repository>();
	 // Instância única do Singleton Git
	 private static Git instance;
 
	 /**
	  * Construtor privado para implementar o padrão Singleton.
	  */
	 private Git() {
	 }
 
	 /**
	  * Retorna a instância única da classe Git (Singleton).
	  * @return A instância de Git.
	  */
	 public static synchronized Git getInstance() {
		 if (instance == null) {
			 instance = new Git();
		 }
		 return instance;
	 }
 
	 /**
	  * Obtém o objeto {@link Repository} para um determinado diretório raiz.
	  * Se o repositório ainda não estiver em cache, ele é criado e adicionado ao cache.
	  * @param root O diretório raiz do repositório Git.
	  * @return O objeto {@link Repository} correspondente.
	  */
	 public Repository getRepository(File root) {
		 Repository repo = repos.get(root);
 
		 if (repo == null) {
			 // Constrói o caminho para o diretório .git
			 final File gitDir = new File(root, Constants.DOT_GIT);
			 try {
				 // Cria um novo objeto Repository
				 repo = new Repository(gitDir);
				 // Armazena o repositório em cache
				 repos.put(root, repo);
			 } catch (IOException ex) {
				 // Ignora a exceção se não for possível criar o repositório
				 // O log pode ser adicionado aqui para depuração: LOG.log(Level.SEVERE, null, ex);
			 }
		 }
 
		 return repo;
	 }
 
	 /**
	  * Retorna o cache de status dos arquivos.
	  * @return O objeto StatusCache.
	  */
	 public StatusCache getStatusCache() {
		 return statusCache;
	 }
 
	 /**
	  * Verifica se um arquivo é um diretório administrativo do Git (.git).
	  * @param file O arquivo a ser verificado.
	  * @return true se o arquivo for um diretório administrativo do Git e for de fato um diretório, false caso contrário.
	  */
	 public boolean isAdministrative(File file) {
		 String name = file.getName();
		 return isAdministrative(name) && file.isDirectory();
	 }
 
	 /**
	  * Verifica se um nome de arquivo corresponde ao diretório administrativo do Git.
	  * @param fileName O nome do arquivo a ser verificado.
	  * @return true se o nome do arquivo for ".git", false caso contrário.
	  */
	 public boolean isAdministrative(String fileName) {
		 return fileName.equals(".git"); 
	 }
 
	 /**
	  * Verifica se um arquivo é gerenciado pelo Git.
	  * @param file O arquivo a ser verificado.
	  * @return true se o arquivo for gerenciado pelo Git e não for parte dos metadados internos do Git, false caso contrário.
	  */
	 public boolean isManaged(File file) {
		 return VersioningSupport.getOwner(file) instanceof GitVCS && !GitUtils.isPartOfGitMetadata(file);
	 }
 
	 /**
	  * Encontra o diretório pai gerenciado pelo Git mais acima na hierarquia de arquivos.
	  * @param file O arquivo a partir do qual a busca será iniciada.
	  * @return O diretório pai mais alto gerenciado pelo Git, ou null se nenhum for encontrado.
	  */
	 public File getTopmostManagedParent(File file) {
		 // Se o arquivo for parte dos metadados do Git, sobe na hierarquia até encontrar o diretório .git
		 if (GitUtils.isPartOfGitMetadata(file)) {
			 for (; file != null; file = file.getParentFile()) {
				 if (isAdministrative(file)) {
					 file = file.getParentFile(); // Pega o pai do diretório .git
					 break;
				 }
			 }
		 }
		 File topmost = null;
		 // Continua subindo na hierarquia para encontrar o diretório raiz do repositório Git
		 for (; file != null; file = file.getParentFile()) {
			 // Se a varredura for proibida para este arquivo, para a busca
			 if (org.netbeans.modules.versioning.util.Utils.isScanForbidden(file)) {
				 break;
			 }
			 // Se o diretório contiver um diretório .git gravável, é o diretório raiz
			 if (new File(file, ".git").canWrite()) { 
				 topmost = file;
				 break;
			 }
		 }
		 return topmost;
	 }
 
	 /**
	  * Obtém o tipo MIME de um arquivo.
	  * @param file O arquivo para o qual obter o tipo MIME.
	  * @return O tipo MIME do arquivo.
	  */
	 public String getMimeType(File file) {
		 FileObject fo = FileUtil.toFileObject(file);
		 String foMime;
		 if (fo == null) {
			 foMime = "content/unknown"; // Tipo MIME desconhecido se FileObject for nulo
		 } else {
			 foMime = fo.getMIMEType();
			 if ("content/unknown".equals(foMime)) 
			 {
				 foMime = "text/plain"; // Assume texto simples se o tipo MIME for desconhecido
			 }
		 }
		 // Se o arquivo não estiver versionado, verifica se é binário
		 if ((statusCache.getStatus(file).getStatus() & StatusInfo.STATUS_VERSIONED) == 0) {
			 return GitUtils.isFileContentBinary(file) ? "application/octet-stream" : foMime;
		 } else {
			 return foMime;
		 }
	 }
 
	 /**
	  * Notifica os ouvintes que os arquivos versionados foram alterados.
	  */
	 public void versionedFilesChanged() {
		 support.firePropertyChange(PROP_VERSIONED_FILES_CHANGED, null, null);
	 }
 
	 /**
	  * Força a atualização de todas as anotações (por exemplo, status de arquivos).
	  */
	 public void refreshAllAnnotations() {
		 support.firePropertyChange(PROP_ANNOTATIONS_CHANGED, null, null);
	 }
 
	 /**
	  * Notifica os ouvintes que um conjunto de alterações (changeset) foi alterado em um repositório específico.
	  * @param repository O diretório raiz do repositório onde o changeset foi alterado.
	  */
	 public void changesetChanged(File repository) {
		 support.firePropertyChange(PROP_CHANGESET_CHANGED, repository, null);
	 }
 
	 /**
	  * Adiciona um ouvinte para mudanças de propriedade.
	  * @param listener O ouvinte a ser adicionado.
	  */
	 public void addPropertyChangeListener(PropertyChangeListener listener) {
		 support.addPropertyChangeListener(listener);
	 }
 
	 /**
	  * Remove um ouvinte de mudanças de propriedade.
	  * @param listener O ouvinte a ser removido.
	  */
	 public void removePropertyChangeListener(PropertyChangeListener listener) {
		 support.removePropertyChangeListener(listener);
	 }
 
	 /**
	  * Obtém o conteúdo original de um arquivo de trabalho e o grava em um arquivo de destino.
	  * @param workingCopy O arquivo de trabalho atual.
	  * @param originalFile O arquivo de destino onde o conteúdo original será gravado.
	  */
	 public void getOriginalFile(File workingCopy, File originalFile) {
		 StatusInfo info = statusCache.getStatus(workingCopy);
		 LOG.log(Level.FINE, "getOriginalFile: {0} {1}", new Object[]{workingCopy, info}); 
		 // Se o arquivo não puder ser "diffado", retorna.
		 if ((info.getStatus() & STATUS_DIFFABLE) == 0) {
			 return;         
		 }
		 try {
			 // Obtém a revisão base do arquivo
			 File original = GitUtils.getFileRevision(workingCopy, GitRepository.REVISION_BASE);
			 if (original == null) {
				 return;
			 }
			 // Copia o conteúdo da revisão original para o arquivo de destino
			 org.netbeans.modules.versioning.util.Utils.copyStreamsCloseAll(new FileOutputStream(originalFile), new FileInputStream(original));
			 // Exclui o arquivo temporário da revisão original
			 original.delete();
		 } catch (IOException e) {
			 Logger.getLogger(Git.class.getName()).log(Level.INFO, "Unable to get original file", e); 
		 }
 
	 }
 
	 /**
	  * Retorna um RequestProcessor genérico.
	  * @return Um RequestProcessor.
	  */
	 public RequestProcessor getRequestProcessor() {
		 return getRequestProcessor((String) null);
	 }
 
	 /**
	  * Retorna um RequestProcessor associado a um arquivo específico.
	  * @param file O arquivo para o qual obter o RequestProcessor.
	  * @return Um RequestProcessor.
	  */
	 public RequestProcessor getRequestProcessor(File file) {
		 return getRequestProcessor(file.getAbsolutePath());
	 }
 
	 /**
	  * Retorna um RequestProcessor associado a uma URL (ou uma chave genérica).
	  * Se um RequestProcessor para a chave não existir, um novo é criado.
	  * @param url A URL ou chave para associar o RequestProcessor. Pode ser null para um RequestProcessor genérico.
	  * @return Um RequestProcessor.
	  */
	 public RequestProcessor getRequestProcessor(String url) {
		 if (processorsToUrl == null) {
			 processorsToUrl = new HashMap<String, RequestProcessor>();
		 }
		 String key;
		 if (url != null) {
			 key = url;
		 } else {
			 key = "ANY_URL"; // Chave para um RequestProcessor genérico
		 }
		 RequestProcessor rp = processorsToUrl.get(key);
		 if (rp == null) {
			 // Cria um novo RequestProcessor com um único thread
			 rp = new RequestProcessor("Git - " + key, 1, true); 
			 processorsToUrl.put(key, rp);
		 }
		 return rp;
	 }
 
	 /**
	  * Limpa (remove) um RequestProcessor associado a uma URL específica.
	  * @param url A URL do RequestProcessor a ser limpo.
	  */
	 public void clearRequestProcessor(String url) {
		 if (processorsToUrl != null & url != null) {
			 processorsToUrl.remove(url);
		 }
	 }
 }