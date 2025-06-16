// Inclui os arquivos de cabeçalho necessários
#include "../param.h"
#include "../user.h"
#include "../systm.h"
#include "../proc.h"
#include "../text.h"
#include "../inode.h"
#include "../seg.h"

// Define as constantes para os endereços dos clocks
#define CLOCK1 0177546
#define CLOCK2 0172540

// Define o código de inicialização do sistema
int icode[] = {
    0104413, // sys exit
    0000014, // código de término
    0000010, // 
    0000777, // 
    0000014, // 
    0000000, // 
    0062457, // 
    0061564, // 
    0064457, // 
    0064556, // 
    0000164, // 
};

// Função principal do sistema
main() {
    // Variáveis externas
    extern schar;
    register i, *p;

    // Inicializa a trava de atualização
    updlock = 0;

    // Calcula o endereço de início da memória
    i = *ka6 + USIZE;

    // Configura o registrador de segmento de usuário
    UISD->r[0] = 077406;

    // Loop infinito para inicializar a memória
    for (;;) {
        // Configura o endereço de segmento de usuário
        UISA->r[0] = i;

        // Verifica se o endereço é válido
        if (fuibyte(0) < 0)
            break;

        // Limpa o segmento de memória
        clearseg(i);

        // Incrementa a quantidade de memória disponível
        maxmem++;

        // Libera o segmento de memória no mapa de memória
        mfree(coremap, 1, i);

        // Incrementa o endereço de início da memória
        i++;
    }

    // Verifica se o tipo de CPU é 70
    if (cputype == 70) {
        // Configura o mapa de bits de usuário
        for (i = 0; i < 62; i += 2) {
            UBMAP->r[i] = i << 12;
            UBMAP->r[i + 1] = 0;
        }
    }

    // Imprime a quantidade de memória disponível
    printf("mem = %l\n", maxmem * 5 / 16);

    // Limita a quantidade de memória disponível
    maxmem = min(maxmem, MAXMEM);

    // Libera o espaço de swap
    mfree(swapmap, nswap, swplo);

    // Configura os registradores de segmento de usuário
    UISA->r[7] = ka6[1];
    UISD->r[7] = 077406;

    // Verifica se o clock está funcionando
    lks = CLOCK1;
    if (fuiword(lks) == -1) {
        lks = CLOCK2;
        if (fuiword(lks) == -1)
            panic("no clock");
    }

    // Configura o processo inicial
    proc[0].p_addr = *ka6;
    proc[0].p_size = USIZE;
    proc[0].p_stat = SRUN;
    proc[0].p_flag |= SLOAD | SSYS;
    u.u_procp = &proc[0];

    // Inicializa o clock
    *lks = 0115;

    // Inicializa os subsistemas
    cinit();
    binit();
    iinit();

    // Obtém o diretório raiz
    rootdir = iget(rootdev, ROOTINO);
    rootdir->i_flag &= ~ILOCK;
    u.u_cdir = iget(rootdev, ROOTINO);
    u.u_cdir->i_flag &= ~ILOCK;

    // Cria um novo processo
    if (newproc()) {
        // Expande a memória do processo
        expand(USIZE + 1);

        // Configura os registradores de segmento de usuário
        estabur(0, 1, 0, 0);

        // Copia o código de inicialização para a memória do processo
        copyout(icode, 0, sizeof icode);

        // Retorna
        return;
    }

    // Agenda o próximo processo
    sched();
}

// Função para configurar os registradores de segmento de usuário
sureg() {
    // Variáveis
    register *up, *rp, a;

    // Calcula o endereço de início da memória do processo
    a = u.u_procp->p_addr;

    // Configura os registradores de segmento de usuário
    up = &u.u_uisa[16];
    rp = &UISA->r[16];

    // Verifica se o tipo de CPU é 40
    if (cputype == 40) {
        up -= 8;
        rp -= 8;
    }

    // Loop para configurar os registradores de segmento de usuário
    while (rp > &UISA->r[0])
        *--rp = *--up + a;

    // Verifica se o processo tem texto compartilhado
    if ((up = u.u_procp->p_textp) != NULL)
        a -= up->x_caddr;

    // Configura os registradores de segmento de usuário para o texto compartilhado
    up = &u.u_uisd[16];
    rp = &UISD->r[16];

    // Verifica se o tipo de CPU é 40
    if (cputype == 40) {
        up -= 8;
        rp -= 8;
    }

    // Loop para configurar os registradores de segmento de usuário
    while (rp > &UISD->r[0]) {
        *--rp = *--up;
        if ((*rp & WO) == 0)
            rp[(UISA - UISD) / 2] -= a;
    }
}

// Função para configurar os registradores de segmento de usuário para um processo
estabur(nt, nd, ns, sep) {
    // Variáveis
    register a, *ap, *dp;

    // Verifica se o tipo de CPU é 40 e se o modo de separação de segmentos está ativado
    if (sep) {
        if (cputype == 40)
            goto err;
        if (nseg(nt) > 8 || nseg(nd) + nseg(ns) > 8)
            goto err;
    } else {
        // Verifica se o número total de segmentos excede o limite
        if (nseg(nt) + nseg(nd) + nseg(ns) > 8)
            goto err;
    }

    // Verifica se a quantidade de memória solicitada excede o limite
    if (nt + nd + ns + USIZE > maxmem)
        goto err;

    // Inicializa as variáveis
    a = 0;
    ap = &u.u_uisa[0];
    dp = &u.u_uisd[0];

    // Configura os registradores de segmento de usuário para o texto
    while (nt >= 128) {
        *dp++ = (127 << 8) | RO;
        *ap++ = a;
        a += 128;
        nt -= 128;
    }

    // Configura o registrador de segmento de usuário para o texto restante
    if (nt) {
        *dp++ = ((nt - 1) << 8) | RO;
        *ap++ = a;
    }

    // Verifica se o modo de separação de segmentos está ativado
    if (sep) {
        // Preenche os registradores de segmento de usuário com zeros
        while (ap < &u.u_uisa[8]) {
            *ap++ = 0;
            *dp++ = 0;
        }
    }

    // Configura os registradores de segmento de usuário para os dados
    a = USIZE;
    while (nd >= 128) {
        *dp++ = (127 << 8) | RW;
        *ap++ = a;
        a += 128;
        nd -= 128;
    }

    // Configura o registrador de segmento de usuário para os dados restantes
    if (nd) {
        *dp++ = ((nd - 1) << 8) | RW;
        *ap++ = a;
        a += nd;
    }

    // Preenche os registradores de segmento de usuário com zeros
    while (ap < &u.u_uisa[8]) {
        *dp++ = 0;
        *ap++ = 0;
    }

    // Verifica se o modo de separação de segmentos está ativado
    if (sep) {
        // Preenche os registradores de segmento de usuário com zeros
        while (ap < &u.u_uisa[16]) {
            *dp++ = 0;
            *ap++ = 0;
        }
    }

    // Configura os registradores de segmento de usuário para a pilha
    a += ns;
    while (ns >= 128) {
        a -= 128;
        ns -= 128;
        *--dp = (127 << 8) | RW;
        *--ap = a;
    }

    // Configura o registrador de segmento de usuário para a pilha restante
    if (ns) {
        *--dp = ((128 - ns) << 8) | RW | ED;
        *--ap = a - 128;
    }

    // Verifica se o modo de separação de segmentos está desativado
    if (!sep) {
        // Copia os registradores de segmento de usuário para a segunda metade
        ap = &u.u_uisa[0];
        dp = &u.u_uisa[8];
        while (ap < &u.u_uisa[8])
            *dp++ = *ap++;
        ap = &u.u_uisd[0];
        dp = &u.u_uisd[8];
        while (ap < &u.u_uisd[8])
            *dp++ = *ap++;
    }

    // Configura os registradores de segmento de usuário
    sureg();

    // Retorna com sucesso
    return 0;

err:
    // Retorna com erro
    u.u_error = ENOMEM;
    return -1;
}

// Função para calcular o número de segmentos necessários
nseg(n) {
    // Calcula o número de segmentos necessários
    return (n + 127) >> 7;
}