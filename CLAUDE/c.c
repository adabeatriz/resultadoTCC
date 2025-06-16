// Inclusão dos arquivos de cabeçalho necessários para o sistema
#include "../param.h"  // Parâmetros do sistema
#include "../user.h"   // Estruturas e definições do usuário
#include "../systm.h"  // Definições do sistema
#include "../proc.h"   // Estruturas e definições de processos
#include "../text.h"   // Definições de segmentos de texto
#include "../inode.h"  // Estruturas e definições de inodes
#include "../seg.h"    // Definições de segmentação de memória

// Definições dos endereços dos relógios do sistema (clock)
#define	CLOCK1	0177546  // Endereço do primeiro relógio
#define	CLOCK2	0172540  // Endereço do segundo relógio (alternativo)

// Código inicial que será executado pelo primeiro processo do usuário
// Este array contém instruções em linguagem de máquina PDP-11
int	icode[]
{
	0104413,	// Instrução: sys exec
	0000014,	// Argumento: ponteiro para nome do programa
	0000010,	// Argumento: ponteiro para array de argumentos
	0000777,	// Instrução: br . (loop infinito se exec falhar)
	0000014,	// String: "/etc/init" (nome do programa a executar)
	0000000,	// Terminador da string
	0062457,	// Caracteres ASCII da string "/etc/init"
	0061564,
	0064457,
	0064556,
	0000164,
};

// Função principal do sistema - inicialização do kernel
main()
{
	extern schar;        // Variável externa (início da área de swap)
	register i, *p;      // Variáveis locais para iteração e ponteiro
	
	updlock = 0;         // Inicializa o lock de atualização como desbloqueado
	
	// Calcula o endereço inicial da memória disponível
	// *ka6 contém o endereço base do kernel, USIZE é o tamanho da área do usuário
	i = *ka6 + USIZE;
	
	// Configura o registrador de descrição do segmento do usuário
	// 077406 = modo de leitura/escrita, tamanho 64 palavras (USIZE)
	UISD->r[0] = 077406;
	
	// Loop para descobrir toda a memória física disponível
	for(;;) {
		UISA->r[0] = i;          // Define o endereço do segmento para teste
		if(fuibyte(0) < 0)       // Tenta ler um byte do endereço virtual 0
			break;               // Se falhar, não há mais memória
		clearseg(i);             // Limpa o segmento de memória
		maxmem++;                // Incrementa contador de memória máxima
		mfree(coremap, 1, i);    // Adiciona o bloco ao mapa de memória livre
		i++;                     // Próximo bloco de memória
	}
	
	// Se for um PDP-11/70, inicializa o mapa de barramentos UNIBUS
	if(cputype == 70)
	for(i=0; i<62; i=+2) {       // Percorre registradores do mapa UNIBUS
		UBMAP->r[i] = i<<12;     // Mapeia endereço físico
		UBMAP->r[i+1] = 0;       // Zera o registrador superior
	}
	
	// Imprime quantidade de memória disponível em K-words
	// Converte de blocos de 64 palavras para K-words (5/16 é a razão de conversão)
	printf("mem = %l\n", maxmem*5/16);
	
	// Limita a memória máxima ao valor definido pelo sistema
	maxmem = min(maxmem, MAXMEM);
	
	// Inicializa o mapa de área de swap como livre
	mfree(swapmap, nswap, swplo);
	
	// Configura o último segmento do usuário para apontar para o kernel
	UISA->r[7] = ka6[1];     // Endereço do segmento do kernel
	UISD->r[7] = 077406;     // Modo de leitura/escrita
	
	// Detecta qual relógio está disponível no sistema
	lks = CLOCK1;                    // Tenta o primeiro relógio
	if(fuiword(lks) == -1) {         // Se não conseguir ler
		lks = CLOCK2;                // Tenta o segundo relógio
		if(fuiword(lks) == -1)       // Se também falhar
			panic("no clock");        // Para o sistema com erro
	}
	
	// Inicializa o processo 0 (processo do kernel)
	proc[0].p_addr = *ka6;           // Endereço do processo
	proc[0].p_size = USIZE;          // Tamanho do processo
	proc[0].p_stat = SRUN;           // Estado: executando
	proc[0].p_flag =| SLOAD|SSYS;    // Flags: carregado na memória + sistema
	u.u_procp = &proc[0];            // Aponta o usuário atual para o processo 0
	
	// Configura o relógio para gerar interrupções
	// 0115 = habilita interrupções + taxa de interrupção
	*lks = 0115;
	
	// Inicializa os diversos subsistemas
	cinit();  // Inicializa cache de caracteres
	binit();  // Inicializa buffers de E/S
	iinit();  // Inicializa tabela de inodes
	
	// Configura o diretório raiz do sistema de arquivos
	rootdir = iget(rootdev, ROOTINO);    // Obtém o inode raiz
	rootdir->i_flag =& ~ILOCK;           // Remove o lock do inode raiz
	
	// Configura o diretório atual do usuário como raiz
	u.u_cdir = iget(rootdev, ROOTINO);   // Obtém novamente o inode raiz
	u.u_cdir->i_flag =& ~ILOCK;          // Remove o lock
	
	// Cria o primeiro processo de usuário
	if(newproc()) {                      // Se este for o processo filho
		expand(USIZE+1);                 // Expande o espaço de memória
		estabur(0, 1, 0, 0);            // Estabelece os registradores do usuário
		// Copia o código inicial para o espaço do usuário
		copyout(icode, 0, sizeof icode);
		return;                          // Retorna para executar o código
	}
	
	// Se este for o processo pai (kernel), inicia o escalonador
	sched();
}

// Função para configurar os registradores de segmentação do usuário
sureg()
{
	register *up, *rp, a;  // Ponteiros e endereço base
	
	a = u.u_procp->p_addr;      // Obtém o endereço base do processo atual
	up = &u.u_uisa[16];         // Ponteiro para registradores de endereço do usuário
	rp = &UISA->r[16];          // Ponteiro para registradores de hardware
	
	// Para PDP-11/40, usa apenas 8 registradores em vez de 16
	if(cputype == 40) {
		up =- 8;  // Ajusta ponteiros para 8 registradores
		rp =- 8;
	}
	
	// Copia registradores de endereço do usuário para o hardware
	// Adiciona o endereço base do processo a cada registrador
	while(rp > &UISA->r[0])
		*--rp = *--up + a;
	
	// Se o processo tem um segmento de texto compartilhado
	if((up=u.u_procp->p_textp) != NULL)
		a =- up->x_caddr;  // Ajusta o endereço base
	
	up = &u.u_uisd[16];    // Ponteiro para registradores de descrição do usuário
	rp = &UISD->r[16];     // Ponteiro para registradores de hardware
	
	// Ajusta para PDP-11/40
	if(cputype == 40) {
		up =- 8;
		rp =- 8;
	}
	
	// Copia registradores de descrição e ajusta endereços se necessário
	while(rp > &UISD->r[0]) {
		*--rp = *--up;                    // Copia o descritor
		if((*rp & WO) == 0)               // Se não é write-only
			rp[(UISA-UISD)/2] =- a;       // Ajusta o endereço correspondente
	}
}

// Função para estabelecer os registradores do usuário
// nt = tamanho do texto, nd = tamanho dos dados, ns = tamanho da pilha, sep = separação I&D
estabur(nt, nd, ns, sep)
{
	register a, *ap, *dp;  // Endereço atual e ponteiros para registradores
	
	// Verifica se a configuração é válida para separação I&D
	if(sep) {
		if(cputype == 40)               // PDP-11/40 não suporta separação I&D
			goto err;
		// Verifica se cabe nos registradores disponíveis
		if(nseg(nt) > 8 || nseg(nd)+nseg(ns) > 8)
			goto err;
	} else
		// Para espaço unificado, verifica se tudo cabe em 8 segmentos
		if(nseg(nt)+nseg(nd)+nseg(ns) > 8)
			goto err;
	
	// Verifica se o total não excede a memória disponível
	if(nt+nd+ns+USIZE > maxmem)
		goto err;
	
	a = 0;                      // Endereço inicial
	ap = &u.u_uisa[0];         // Ponteiro para registradores de endereço
	dp = &u.u_uisd[0];         // Ponteiro para registradores de descrição
	
	// Configura os segmentos de texto (somente leitura)
	while(nt >= 128) {          // Para segmentos de 128 blocos (tamanho máximo)
		*dp++ = (127<<8) | RO;  // Descritor: 127 blocos, somente leitura
		*ap++ = a;              // Endereço do segmento
		a =+ 128;               // Próximo endereço
		nt =- 128;              // Reduz o tamanho restante
	}
	if(nt) {                    // Se sobrou texto
		*dp++ = ((nt-1)<<8) | RO;  // Descritor do último segmento de texto
		*ap++ = a;                  // Seu endereço
	}
	
	// Se há separação I&D, preenche os registradores restantes com zeros
	if(sep)
	while(ap < &u.u_uisa[8]) {
		*ap++ = 0;
		*dp++ = 0;
	}
	
	a = USIZE;  // Inicia os dados após a área do usuário
	
	// Configura os segmentos de dados (leitura/escrita)
	while(nd >= 128) {
		*dp++ = (127<<8) | RW;  // Descritor: 127 blocos, leitura/escrita
		*ap++ = a;              // Endereço do segmento
		a =+ 128;
		nd =- 128;
	}
	if(nd) {
		*dp++ = ((nd-1)<<8) | RW;  // Último segmento de dados
		*ap++ = a;
		a =+ nd;
	}
	
	// Preenche registradores não utilizados
	while(ap < &u.u_uisa[8]) {
		*dp++ = 0;
		*ap++ = 0;
	}
	
	// Para separação I&D, zera os registradores do espaço D
	if(sep)
	while(ap < &u.u_uisa[16]) {
		*dp++ = 0;
		*ap++ = 0;
	}
	
	// Configura os segmentos de pilha (crescem para baixo)
	a =+ ns;  // Endereço final da pilha
	while(ns >= 128) {
		a =- 128;
		ns =- 128;
		*--dp = (127<<8) | RW;    // Descritor do segmento de pilha
		*--ap = a;                // Endereço do segmento
	}
	if(ns) {
		// Último segmento da pilha, com flag ED (expand down)
		*--dp = ((128-ns)<<8) | RW | ED;
		*--ap = a-128;
	}
	
	// Se não há separação I&D, copia os registradores para o espaço D
	if(!sep) {
		ap = &u.u_uisa[0];
		dp = &u.u_uisa[8];
		while(ap < &u.u_uisa[8])  // Copia endereços
			*dp++ = *ap++;
		ap = &u.u_uisd[0];
		dp = &u.u_uisd[8];
		while(ap < &u.u_uisd[8])  // Copia descritores
			*dp++ = *ap++;
	}
	
	sureg();    // Atualiza os registradores de hardware
	return(0);  // Sucesso
	
err:
	u.u_error = ENOMEM;  // Define erro de falta de memória
	return(-1);          // Retorna erro
}

// Função para calcular quantos segmentos são necessários para n blocos
// Cada segmento pode conter no máximo 128 blocos (8K palavras)
nseg(n)
{
	return((n+127)>>7);  // Divide por 128, arredondando para cima
}