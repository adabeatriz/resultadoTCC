#include "../param.h"       // Inclusão dos parâmetros do sistema
#include "../user.h"       // Definições relacionadas ao usuário
#include "../systm.h"      // Definições gerais do sistema
#include "../proc.h"       // Definições de processos
#include "../text.h"       // Definições de segmentos de texto
#include "../inode.h"      // Definições de i-nodes
#include "../seg.h"        // Definições de segmentos de memória

#define CLOCK1  0177546    // Endereço do primeiro relógio
#define CLOCK2  0172540    // Endereço alternativo do relógio

// Código binário inicial para um novo processo
int icode[] {
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

/*
 * Função principal de inicialização do sistema
 * Responsável por configurar a memória, processos e dispositivos
 */
main()
{
    extern schar;          // Declaração externa (não definida no código)
    register i, *p;        // Registradores para otimização

    updlock = 0;           // Inicializa lock de atualização
    
    // Configuração inicial da memória
    i = *ka6 + USIZE;      // Calcula endereço inicial
    UISD->r[0] = 077406;   // Configura registro de status do segmento
    
    // Loop para detectar memória disponível
    for(;;) {
        UISA->r[0] = i;    // Configura registro de endereço do segmento
        if(fuibyte(0) < 0) // Verifica se o endereço é válido
            break;
        clearseg(i);       // Limpa o segmento
        maxmem++;          // Incrementa contador de memória máxima
        mfree(coremap, 1, i); // Libera memória no mapa de núcleo
        i++;               // Próximo segmento
    }
    
    // Configuração específica para CPU tipo 70
    if(cputype == 70)
    for(i=0; i<62; i=+2) {
        UBMAP->r[i] = i<<12;  // Configura mapeamento de blocos
        UBMAP->r[i+1] = 0;    // Limpa parte alta do registro
    }
    
    printf("mem = %l\n", maxmem*5/16); // Exibe quantidade de memória
    maxmem = min(maxmem, MAXMEM);      // Limita memória ao máximo permitido
    mfree(swapmap, nswap, swplo);      // Libera espaço de swap

    // Configuração dos registros de segmento do usuário
    UISA->r[7] = ka6[1]; 
    UISD->r[7] = 077406;
    
    // Detecção do relógio do sistema
    lks = CLOCK1;
    if(fuiword(lks) == -1) {
        lks = CLOCK2;
        if(fuiword(lks) == -1)
            panic("no clock"); // Pânico se não encontrar relógio
    }
    
    // Configuração do processo 0 (processo sistema)
    proc[0].p_addr = *ka6;       // Endereço base
    proc[0].p_size = USIZE;      // Tamanho do segmento
    proc[0].p_stat = SRUN;       // Estado: executando
    proc[0].p_flag =| SLOAD|SSYS; // Flags: carregado e sistema
    u.u_procp = &proc[0];        // Associa ao usuário atual
    
    *lks = 0115;  // Configura relógio
    
    // Inicialização de subsistemas
    cinit();      // Inicialização de dispositivos de caractere
    binit();      // Inicialização do sistema de buffers
    iinit();      // Inicialização do sistema de arquivos
    
    // Configuração do diretório raiz
    rootdir = iget(rootdev, ROOTINO);  // Obtém i-node raiz
    rootdir->i_flag =& ~ILOCK;         // Libera lock
    u.u_cdir = iget(rootdev, ROOTINO); // Diretório corrente = raiz
    u.u_cdir->i_flag =& ~ILOCK;        // Libera lock

    // Criação do primeiro processo de usuário
    if(newproc()) {
        expand(USIZE+1);          // Expande área de usuário
        estabur(0, 1, 0, 0);      // Estabelece registros de usuário
        copyout(icode, 0, sizeof icode); // Copia código inicial
        return;
    }
    sched();  // Chama escalonador
}

/*
 * Função sureg - Sincroniza registros de segmento do usuário
 * com a tabela de páginas do MMU
 */
sureg()
{
    register *up, *rp, a;

    a = u.u_procp->p_addr;  // Endereço base do processo
    up = &u.u_uisa[16];     // Ponteiro para registros de segmento do usuário
    rp = &UISA->r[16];      // Ponteiro para registros da MMU
    
    // Ajuste para CPU tipo 40
    if(cputype == 40) {
        up =- 8;
        rp =- 8;
    }
    
    // Copia registros de segmento de instrução
    while(rp > &UISA->r[0])
        *--rp = *--up + a;  // Ajusta endereços
        
    // Ajuste para segmento de texto compartilhado
    if((up=u.u_procp->p_textp) != NULL)
        a =- up->x_caddr;
        
    // Configura registros de segmento de dados
    up = &u.u_uisd[16];
    rp = &UISD->r[16];
    if(cputype == 40) {
        up =- 8;
        rp =- 8;
    }
    while(rp > &UISD->r[0]) {
        *--rp = *--up;
        if((*rp & WO) == 0)  // Se não for write-only
            rp[(UISA-UISD)/2] =- a;  // Ajusta endereço
    }
}

/*
 * Função estabur - Estabelece registros de segmento do usuário
 * Parâmetros:
 *   nt: tamanho do segmento de texto
 *   nd: tamanho do segmento de dados
 *   ns: tamanho do segmento de pilha
 *   sep: flag para espaços de endereçamento separados
 * Retorna:
 *   0 em sucesso, -1 em erro
 */
estabur(nt, nd, ns, sep)
{
    register a, *ap, *dp;

    // Verificação de limites para espaços separados
    if(sep) {
        if(cputype == 40)  // CPU tipo 40 não suporta
            goto err;
        if(nseg(nt) > 8 || nseg(nd)+nseg(ns) > 8)
            goto err;
    } else
        if(nseg(nt)+nseg(nd)+nseg(ns) > 8)
            goto err;
            
    // Verifica se há memória suficiente
    if(nt+nd+ns+USIZE > maxmem)
        goto err;
        
    // Configura segmentos de texto (read-only)
    a = 0;
    ap = &u.u_uisa[0];  // Ponteiro para registros de endereço
    dp = &u.u_uisd[0];  // Ponteiro para registros de descrição
    while(nt >= 128) {
        *dp++ = (127<<8) | RO;  // Descrição de segmento (127 páginas, RO)
        *ap++ = a;               // Endereço base
        a =+ 128;                // Incrementa endereço
        nt =- 128;               // Decrementa tamanho restante
    }
    if(nt) {  // Trata tamanho residual
        *dp++ = ((nt-1)<<8) | RO;
        *ap++ = a;
    }
    
    // Preenche com zeros se espaços separados
    if(sep)
    while(ap < &u.u_uisa[8]) {
        *ap++ = 0;
        *dp++ = 0;
    }
    
    // Configura segmentos de dados (read-write)
    a = USIZE;  // Começa após área do sistema
    while(nd >= 128) {
        *dp++ = (127<<8) | RW;
        *ap++ = a;
        a =+ 128;
        nd =- 128;
    }
    if(nd) {  // Trata tamanho residual
        *dp++ = ((nd-1)<<8) | RW;
        *ap++ = a;
        a =+ nd;
    }
    
    // Preenche registros restantes com zeros
    while(ap < &u.u_uisa[8]) {
        *dp++ = 0;
        *ap++ = 0;
    }
    
    // Se espaços separados, preenche segunda metade
    if(sep)
    while(ap < &u.u_uisa[16]) {
        *dp++ = 0;
        *ap++ = 0;
    }
    
    // Configura segmento de pilha (cresce para baixo)
    a =+ ns;
    while(ns >= 128) {
        a =- 128;
        ns =- 128;
        *--dp = (127<<8) | RW;
        *--ap = a;
    }
    if(ns) {  // Trata tamanho residual
        *--dp = ((128-ns)<<8) | RW | ED;  // ED = expand down
        *--ap = a-128;
    }
    
    // Se espaços não separados, duplica registros
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
    
    sureg();  // Sincroniza com MMU
    return(0);

err:
    u.u_error = ENOMEM;  // Define erro "memória insuficiente"
    return(-1);
}

/*
 * Função nseg - Calcula número de segmentos necessários
 * para um dado tamanho em palavras
 * Parâmetro:
 *   n: tamanho em palavras
 * Retorna:
 *   número de segmentos de 128 palavras necessários
 */
nseg(n)
{
    return((n+127)>>7);  // Equivalente a ceil(n/128)
}