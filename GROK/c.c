#include "../param.h"     // Inclui definições de parâmetros do sistema
#include "../user.h"      // Inclui estruturas e definições relacionadas ao usuário
#include "../systm.h"     // Inclui definições do sistema operacional
#include "../proc.h"      // Inclui definições para gerenciamento de processos
#include "../text.h"      // Inclui definições para gerenciamento de texto
#include "../inode.h"     // Inclui definições para inodes (estruturas de arquivos)
#include "../seg.h"       // Inclui definições para gerenciamento de segmentos de memória

#define CLOCK1 0177546    // Endereço do primeiro relógio de hardware
#define CLOCK2 0172540    // Endereço do segundo relógio de hardware (alternativo)

// Array icode contém instruções iniciais para o processo inicial do sistema
int icode[] = {
    0104413,  // Instrução para carregar endereço de memória
    0000014,  // Endereço de memória
    0000010,  // Valor auxiliar
    0000777,  // Máscara de permissões
    0000014,  // Endereço adicional
    0000000,  // Valor nulo
    0062457,  // Instrução para manipulação de string "/etc"
    0061564,  // Continuação da string "/etc"
    0064457,  // Instrução para manipulação de string "/init"
    0064556,  // Continuação da string "/init"
    0000164,  // Valor final
};

// Função principal do sistema operacional, responsável pela inicialização
main()
{
    extern schar;         // Declaração externa para variável schar
    register i, *p;       // Declaração de variáveis locais (i para iteração, p para ponteiro)

    updlock = 0;          // Desbloqueia atualizações de memória
    i = *ka6 + USIZE;     // Calcula endereço inicial de memória (baseado em ka6 + tamanho do usuário)
    UISD->r[0] = 077406;  // Configura registrador UISD com permissões iniciais

    // Loop para inicializar e liberar segmentos de memória
    for(;;) {
        UISA->r[0] = i;        // Define endereço inicial no registrador UISA
        if(fuibyte(0) < 0)     // Verifica se o endereço é inválido
            break;             // Sai do loop se inválido
        clearseg(i);           // Limpa o segmento de memória
        maxmem++;              // Incrementa contador de memória disponível
        mfree(coremap, 1, i);  // Libera o segmento na coremap
        i++;                   // Avança para o próximo segmento
    }

    // Configuração específica para CPU tipo 70 (mapeamento de memória)
    if(cputype == 70)
        for(i = 0; i < 62; i += 2) {
            UBMAP->r[i] = i << 12;     // Configura mapeamento de memória (deslocamento de 12 bits)
            UBMAP->r[i + 1] = 0;       // Zera registrador auxiliar
        }

    // Exibe quantidade de memória disponível (em KB)
    printf("mem = %l\n", maxmem * 5 / 16);
    maxmem = min(maxmem, MAXMEM);         // Limita memória ao máximo permitido
    mfree(swapmap, nswap, swplo);         // Libera espaço na swapmap

    UISA->r[7] = ka6[1];                  // Configura UISA com endereço de ka6
    UISD->r[7] = 077406;                  // Configura UISD com permissões
    lks = CLOCK1;                         // Define endereço do relógio primário
    if(fuiword(lks) == -1) {              // Verifica se o relógio primário está disponível
        lks = CLOCK2;                     // Tenta relógio secundário
        if(fuiword(lks) == -1)            // Verifica se o relógio secundário está disponível
            panic("no clock");            // Encerra com erro se nenhum relógio for encontrado
    }

    // Configura processo inicial (proc[0])
    proc[0].p_addr = *ka6;                // Define endereço do processo
    proc[0].p_size = USIZE;               // Define tamanho do processo
    proc[0].p_stat = SRUN;                // Define estado como "em execução"
    proc[0].p_flag |= SLOAD | SSYS;       // Define flags de carregamento e sistema
    u.u_procp = &proc[0];                 // Associa processo ao contexto do usuário
    *lks = 0115;                          // Configura relógio com valor inicial

    // Inicializa subsistemas
    cinit();                              // Inicializa subsistema de caracteres
    binit();                              // Inicializa subsistema de blocos
    iinit();                              // Inicializa subsistema de inodes

    // Configura diretório raiz
    rootdir = iget(rootdev, ROOTINO);     // Obtém inode do diretório raiz
    rootdir->i_flag &= ~ILOCK;            // Desbloqueia inode do diretório raiz
    u.u_cdir = iget(rootdev, ROOTINO);    // Define diretório atual do usuário
    u.u_cdir->i_flag &= ~ILOCK;           // Desbloqueia inode do diretório atual

    // Cria novo processo inicial
    if(newproc()) {
        expand(USIZE + 1);                // Expande espaço de memória do processo
        estabur(0, 1, 0, 0);              // Configura registradores de usuário
        copyout(icode, 0, sizeof icode);  // Copia código inicial para memória
        return;                           // Retorna após configuração
    }

    sched();                              // Inicia escalonador de processos
}

// Configura registradores de usuário (UISA e UISD) com base no processo atual
sureg()
{
    register *up, *rp, a;                 // Declaração de variáveis locais

    a = u.u_procp->p_addr;                // Obtém endereço do processo atual
    up = &u.u_uisa[16];                   // Ponteiro para registradores UISA do usuário
    rp = &UISA->r[16];                    // Ponteiro para registradores UISA do sistema
    if(cputype == 40) {                   // Ajuste para CPU tipo 40
        up -= 8;                          // Retrocede 8 posições
        rp -= 8;                          // Retrocede 8 posições
    }
    // Copia registradores UISA, ajustando com endereço do processo
    while(rp > &UISA->r[0])
        *--rp = *--up + a;

    // Ajusta endereço para texto, se aplicável
    if((up = u.u_procp->p_textp) != NULL)
        a -= up->x_caddr;

    up = &u.u_uisd[16];                   // Ponteiro para registradores UISD do usuário
    rp = &UISD->r[16];                    // Ponteiro para registradores UISD do sistema
    if(cputype == 40) {                   // Ajuste para CPU tipo 40
        up -= 8;                          // Retrocede 8 posições
        rp -= 8;                          // Retrocede 8 posições
    }
    // Copia registradores UISD, ajustando permissões de escrita
    while(rp > &UISD->r[0]) {
        *--rp = *--up;
        if((*rp & WO) == 0)               // Se não for apenas escrita
            rp[(UISA - UISD) / 2] -= a;   // Ajusta endereço no UISA correspondente
    }
}

// Configura registradores de usuário para texto, dados e pilha
estabur(nt, nd, ns, sep)
{
    register a, *ap, *dp;                 // Declaração de variáveis locais

    // Verifica compatibilidade com modo separado (sep) e tipo de CPU
    if(sep) {
        if(cputype == 40)                 // CPU tipo 40 não suporta modo separado
            goto err;
        if(nseg(nt) > 8 || nseg(nd) + nseg(ns) > 8)  // Verifica limite de segmentos
            goto err;
    } else
        if(nseg(nt) + nseg(nd) + nseg(ns) > 8)       // Verifica limite total de segmentos
            goto err;

    // Verifica se a memória solicitada excede o máximo disponível
    if(nt + nd + ns + USIZE > maxmem)
        goto err;

    a = 0;                                // Inicializa endereço base
    ap = &u.u_uisa[0];                    // Ponteiro para UISA do usuário
    dp = &u.u_uisd[0];                    // Ponteiro para UISD do usuário

    // Configura segmentos de texto (read-only)
    while(nt >= 128) {
        *dp++ = (127 << 8) | RO;          // Configura UISD com tamanho 127 e permissão RO
        *ap++ = a;                        // Define endereço no UISA
        a += 128;                         // Avança endereço
        nt -= 128;                        // Reduz tamanho do texto
    }
    if(nt) {                              // Configura segmento restante de texto
        *dp++ = ((nt - 1) << 8) | RO;     // Configura tamanho restante e permissão RO
        *ap++ = a;                        // Define endereço
    }

    // Preenche registradores não utilizados no modo separado
    if(sep)
        while(ap < &u.u_uisa[8]) {
            *ap++ = 0;                    // Zera UISA
            *dp++ = 0;                    // Zera UISD
        }

    a = USIZE;                            // Define endereço base para dados
    // Configura segmentos de dados (read-write)
    while(nd >= 128) {
        *dp++ = (127 << 8) | RW;          // Configura UISD com tamanho 127 e permissão RW
        *ap++ = a;                        // Define endereço no UISA
        a += 128;                         // Avança endereço
        nd -= 128;                        // Reduz tamanho dos dados
    }
    if(nd) {                              // Configura segmento restante de dados
        *dp++ = ((nd - 1) << 8) | RW;     // Configura tamanho restante e permissão RW
        *ap++ = a;                        // Define endereço
        a += nd;                          // Avança endereço
    }

    // Preenche registradores não utilizados
    while(ap < &u.u_uisa[8]) {
        *dp++ = 0;                        // Zera UISD
        *ap++ = 0;                        // Zera UISA
    }

    // Preenche registradores adicionais no modo separado
    if(sep)
        while(ap < &u.u_uisa[16]) {
            *dp++ = 0;                    // Zera UISD
            *ap++ = 0;                    // Zera UISA
        }

    a += ns;                              // Avança endereço para pilha
    // Configura segmentos de pilha (read-write, expansível)
    while(ns >= 128) {
        a -= 128;                         // Retrocede endereço
        ns -= 128;                        // Reduz tamanho da pilha
        *--dp = (127 << 8) | RW;          // Configura UISD com tamanho 127 e permissão RW
        *--ap = a;                        // Define endereço no UISA
    }
    if(ns) {                              // Configura segmento restante de pilha
        *--dp = ((128 - ns) << 8) | RW | ED; // Configura tamanho, permissão RW e expansível
        *--ap = a - 128;                  // Define endereço ajustado
    }

    // Copia registradores para modo não separado
    if(!sep) {
        ap = &u.u_uisa[0];                // Ponteiro para UISA inicial
        dp = &u.u_uisa[8];                // Ponteiro para UISA secundário
        while(ap < &u.u_uisa[8])
            *dp++ = *ap++;                // Copia UISA
        ap = &u.u_uisd[0];                // Ponteiro para UISD inicial
        dp = &u.u_uisd[8];                // Ponteiro para UISD secundário
        while(ap < &u.u_uisd[8])
            *dp++ = *ap++;                // Copia UISD
    }

    sureg();                              // Configura registradores no hardware
    return(0);                            // Retorna sucesso

err:
    u.u_error = ENOMEM;                   // Define erro de memória insuficiente
    return(-1);                           // Retorna falha
}

// Calcula o número de segmentos necessários para um tamanho dado
nseg(n)
{
    return((n + 127) >> 7);              // Divide por 128, arredondando para cima
}