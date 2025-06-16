// Inclui os arquivos de cabeçalho necessários para o funcionamento do programa
#include "../param.h"
#include "../user.h"
#include "../systm.h"
#include "../proc.h"
#include "../text.h"
#include "../inode.h"
#include "../seg.h"

// Define endereços de memória para dois relógios
#define	CLOCK1	0177546
#define	CLOCK2	0172540

// Código inicial em linguagem de máquina para ser copiado para a memória
int	icode[]
{
	0104413,
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

main()
{
	extern schar;
	register i, *p;

	// Inicializa o bloqueio de atualização como 0
	updlock = 0;
	// Calcula o endereço inicial da memória do usuário
	i = *ka6 + USIZE;
	UISD->r[0] = 077406;
	for(;;) {
		// Define o endereço inicial do segmento de instruções do usuário
		UISA->r[0] = i;
		// Verifica se o byte na posição 0 é acessível
		if(fuibyte(0) < 0)
			break;
		// Limpa o segmento de memória
		clearseg(i);
		// Incrementa o contador de memória máxima
		maxmem++;
		// Libera o segmento de memória no mapa de memória principal
		mfree(coremap, 1, i);
		i++;
	}
	// Configura o mapeamento de memória para o tipo de CPU 70
	if(cputype == 70)
	for(i=0; i<62; i=+2) {
		UBMAP->r[i] = i<<12;
		UBMAP->r[i+1] = 0;
	}
	// Imprime a quantidade de memória disponível
	printf("mem = %l\n", maxmem*5/16);
	// Define o limite máximo de memória
	maxmem = min(maxmem, MAXMEM);
	// Libera o mapa de swap
	mfree(swapmap, nswap, swplo);

	// Configura os registradores de endereço e dados do usuário
	UISA->r[7] = ka6[1];
	UISD->r[7] = 077406;
	// Define o endereço do relógio
	lks = CLOCK1;
	// Verifica se o relógio está acessível
	if(fuiword(lks) == -1) {
		lks = CLOCK2;
		if(fuiword(lks) == -1)
			panic("no clock");
	}
	// Configura o processo 0
	proc[0].p_addr = *ka6;
	proc[0].p_size = USIZE;
	proc[0].p_stat = SRUN;
	proc[0].p_flag =| SLOAD|SSYS;
	u.u_procp = &proc[0];
	*lks = 0115;
	// Inicializa os subsistemas
	cinit();
	binit();
	iinit();
	// Obtém o inode do diretório raiz
	rootdir = iget(rootdev, ROOTINO);
	rootdir->i_flag =& ~ILOCK;
	u.u_cdir = iget(rootdev, ROOTINO);
	u.u_cdir->i_flag =& ~ILOCK;

	// Cria um novo processo
	if(newproc()) {
		// Expande a memória do usuário
		expand(USIZE+1);
		// Estabelece os segmentos de memória do usuário
		estabur(0, 1, 0, 0);
		// Copia o código inicial para a memória do usuário
		copyout(icode, 0, sizeof icode);
		return;
	}
	// Agenda o próximo processo
	sched();
}

// Função para configurar os registradores do usuário
sureg()
{
	register *up, *rp, a;

	// Obtém o endereço do processo atual
	a = u.u_procp->p_addr;
	up = &u.u_uisa[16];
	rp = &UISA->r[16];
	// Ajusta os ponteiros para o tipo de CPU 40
	if(cputype == 40) {
		up =- 8;
		rp =- 8;
	}
	// Configura os registradores de endereço do usuário
	while(rp > &UISA->r[0])
		*--rp = *--up + a;
	// Obtém o endereço do texto do processo atual
	if((up=u.u_procp->p_textp) != NULL)
		a =- up->x_caddr;
	up = &u.u_uisd[16];
	rp = &UISD->r[16];
	// Ajusta os ponteiros para o tipo de CPU 40
	if(cputype == 40) {
		up =- 8;
		rp =- 8;
	}
	// Configura os registradores de dados do usuário
	while(rp > &UISD->r[0]) {
		*--rp = *--up;
		if((*rp & WO) == 0)
			rp[(UISA-UISD)/2] =- a;
	}
}

// Função para estabelecer os segmentos de memória do usuário
estabur(nt, nd, ns, sep)
{
	register a, *ap, *dp;

	// Verifica se a quantidade de segmentos é válida
	if(sep) {
		if(cputype == 40)
			goto err;
		if(nseg(nt) > 8 || nseg(nd)+nseg(ns) > 8)
			goto err;
	} else
		if(nseg(nt)+nseg(nd)+nseg(ns) > 8)
			goto err;
	// Verifica se a quantidade total de memória é válida
	if(nt+nd+ns+USIZE > maxmem)
		goto err;
	a = 0;
	ap = &u.u_uisa[0];
	dp = &u.u_uisd[0];
	// Configura os segmentos de texto
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
	// Configura os segmentos de dados
	if(sep)
	while(ap < &u.u_uisa[8]) {
		*ap++ = 0;
		*dp++ = 0;
	}
	a = USIZE;
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
	// Configura os segmentos de pilha
	if(sep)
	while(ap < &u.u_uisa[16]) {
		*dp++ = 0;
		*ap++ = 0;
	}
	a =+ ns;
	while(ns >= 128) {
		a =- 128;
		ns =- 128;
		*--dp = (127<<8) | RW;
		*--ap = a;
	}
	if(ns) {
		*--dp = ((128-ns)<<8) | RW | ED;
		*--ap = a-128;
	}
	// Configura os segmentos de memória do usuário
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
	// Configura os registradores do usuário
	sureg();
	return(0);

err:
	// Define o erro de memória insuficiente
	u.u_error = ENOMEM;
	return(-1);
}

// Função para calcular o número de segmentos necessários
nseg(n)
{

	return((n+127)>>7);
}
