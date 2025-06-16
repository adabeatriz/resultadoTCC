// Inclusão dos cabeçalhos necessários para parâmetros, estruturas e funções do sistema operacional
#include "../param.h"
#include "../user.h"
#include "../systm.h"
#include "../proc.h"
#include "../text.h"
#include "../inode.h"
#include "../seg.h"

// Definições dos endereços dos relógios do sistema
#define	CLOCK1	0177546
#define	CLOCK2	0172540

// Código de inicialização usado pelo processo init (primeiro processo do usuário)
int icode[] {
	0104413,	// instruções em linguagem de máquina (montagem)
	0000014,
	0000010,
	0000777,	
	0000014,	
	0000000,
	0062457,	
	0061564,
	0064457,
	0064556,
	0000164,
};

// Função principal que inicializa o sistema operacional
main()
{
	extern schar;
	register i, *p;

	updlock = 0; // Desbloqueia atualizações de disco
	i = *ka6 + USIZE; // Começa no final da área do núcleo (memória de usuário)
	UISD->r[0] = 077406; // Define permissões de segmento para leitura/gravação

	// Loop para identificar e liberar blocos de memória disponíveis
	for(;;) {
		UISA->r[0] = i; // Define o endereço da memória
		if(fuibyte(0) < 0) // Verifica se a memória está acessível
			break;
		clearseg(i); // Limpa o segmento de memória
		maxmem++; // Conta os blocos de memória livre
		mfree(coremap, 1, i); // Libera o bloco de memória para o mapa de memória principal
		i++;
	}

	// Configuração do mapeamento de memória para a CPU 70
	if(cputype == 70)
	for(i=0; i<62; i=+2) {
		UBMAP->r[i] = i<<12; // Define endereço físico
		UBMAP->r[i+1] = 0;   // Define endereço lógico
	}

	// Exibe a quantidade total de memória detectada
	printf("mem = %l\n", maxmem*5/16);

	// Ajusta a memória máxima disponível para o sistema
	maxmem = min(maxmem, MAXMEM);
	mfree(swapmap, nswap, swplo); // Inicializa o mapa de troca (swap)

	// Configura registradores de segmento para o processo 0 (núcleo)
	UISA->r[7] = ka6[1]; 
	UISD->r[7] = 077406;

	// Tenta localizar um relógio funcional
	lks = CLOCK1;
	if(fuiword(lks) == -1) {
		lks = CLOCK2;
		if(fuiword(lks) == -1)
			panic("no clock"); // Falha crítica: sem relógio disponível
	}

	// Inicializa o processo 0 (núcleo)
	proc[0].p_addr = *ka6;
	proc[0].p_size = USIZE;
	proc[0].p_stat = SRUN; // Estado: em execução
	proc[0].p_flag =| SLOAD|SSYS; // Flags: carregado na memória e processo do sistema
	u.u_procp = &proc[0]; // Ponteiro para o processo atual
	*lks = 0115; // Inicia o relógio

	// Inicializa subsistemas do núcleo
	cinit(); // Inicializa filas de comunicação
	binit(); // Inicializa buffers de E/S
	iinit(); // Inicializa o sistema de arquivos

	// Define o diretório raiz
	rootdir = iget(rootdev, ROOTINO);
	rootdir->i_flag =& ~ILOCK; // Libera bloqueio de inode

	u.u_cdir = iget(rootdev, ROOTINO); // Define diretório atual
	u.u_cdir->i_flag =& ~ILOCK;

	// Cria um novo processo (init)
	if(newproc()) {
		expand(USIZE+1); // Expande memória do processo
		estabur(0, 1, 0, 0); // Estabelece segmentos de memória
		copyout(icode, 0, sizeof icode); // Copia o código de inicialização para o espaço do usuário
		return;
	}

	// Processo 0 entra no escalonador
	sched();
}

// Atualiza os registradores de segmento do processo atual
sureg()
{
	register *up, *rp, a;

	a = u.u_procp->p_addr;
	up = &u.u_uisa[16];
	rp = &UISA->r[16];

	// Ajustes para CPUs mais antigas (PDP-11/40)
	if(cputype == 40) {
		up =- 8;
		rp =- 8;
	}

	// Atualiza registradores UISA com deslocamento de endereço base do processo
	while(rp > &UISA->r[0])
		*--rp = *--up + a;

	// Ajusta endereço base se o processo tiver texto compartilhado
	if((up=u.u_procp->p_textp) != NULL)
		a =- up->x_caddr;

	up = &u.u_uisd[16];
	rp = &UISD->r[16];

	if(cputype == 40) {
		up =- 8;
		rp =- 8;
	}

	// Atualiza registradores de descrição de segmento (UISD)
	while(rp > &UISD->r[0]) {
		*--rp = *--up;
		if((*rp & WO) == 0) // Se não for segmento somente leitura
			rp[(UISA-UISD)/2] =- a;
	}
}

// Estabelece a configuração dos segmentos de memória do processo
estabur(nt, nd, ns, sep)
{
	register a, *ap, *dp;

	// Verificações de limite para segmentação separada e total
	if(sep) {
		if(cputype == 40)
			goto err;
		if(nseg(nt) > 8 || nseg(nd)+nseg(ns) > 8)
			goto err;
	} else
		if(nseg(nt)+nseg(nd)+nseg(ns) > 8)
			goto err;

	if(nt+nd+ns+USIZE > maxmem)
		goto err;

	a = 0;
	ap = &u.u_uisa[0];
	dp = &u.u_uisd[0];

	// Configura segmento de texto (read-only)
	while(nt >= 128) {
		*dp++ = (127<<8) | RO;
		*ap++ = a;
		a =+ 128;
		nt =- 128;
	}
	if(nt) {
		*dp++ = ((nt-1)<<8) | RO;
		*ap++ = a;
	}

	// Zera espaço restante se segmentação separada
	if(sep)
	while(ap < &u.u_uisa[8]) {
		*ap++ = 0;
		*dp++ = 0;
	}

	a = USIZE;

	// Configura segmento de dados (read-write)
	while(nd >= 128) {
		*dp++ = (127<<8) | RW;
		*ap++ = a;
		a =+ 128;
		nd =- 128;
	}
	if(nd) {
		*dp++ = ((nd-1)<<8) | RW;
		*ap++ = a;
		a =+ nd;
	}

	while(ap < &u.u_uisa[8]) {
		*dp++ = 0;
		*ap++ = 0;
	}

	if(sep)
	while(ap < &u.u_uisa[16]) {
		*dp++ = 0;
		*ap++ = 0;
	}

	// Configura segmento de stack
	a =+ ns;
	while(ns >= 128) {
		a =- 128;
		ns =- 128;
		*--dp = (127<<8) | RW;
		*--ap = a;
	}
	if(ns) {
		*--dp = ((128-ns)<<8) | RW | ED; // Flag ED indica crescimento reverso da pilha
		*--ap = a-128;
	}

	// Copia configuração para a segunda metade dos registradores, se não segmentado
	if(!sep) {
		ap = &u.u_uisa[0];
		dp = &u.u_uisa[8];
		while(ap < &u.u_uisa[8])
			*dp++ = *ap++;
		ap = &u.u_uisd[0];
		dp = &u.u_uisd[8];
		while(ap < &u.u_uisd[8])
			*dp++ = *ap++;
	}

	sureg(); // Atualiza registradores com nova configuração
	return(0);

err:
	u.u_error = ENOMEM; // Erro: sem memória suficiente
	return(-1);
}

// Calcula o número de segmentos necessários para 'n' blocos de 128 palavras
nseg(n)
{
	return((n+127)>>7);
}

