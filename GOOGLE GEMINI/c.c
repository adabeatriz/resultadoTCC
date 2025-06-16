#include "../param.h" // Inclui o arquivo de parâmetros do sistema.
#include "../user.h"  // Inclui o arquivo de definições da estrutura de usuário (u.h).
#include "../systm.h" // Inclui o arquivo de definições de chamadas de sistema.
#include "../proc.h"  // Inclui o arquivo de definições da estrutura de processo (proc.h).
#include "../text.h"  // Inclui o arquivo de definições de segmentos de texto.
#include "../inode.h" // Inclui o arquivo de definições de i-nodes.
#include "../seg.h"   // Inclui o arquivo de definições de segmentos de memória.

// Endereços de hardware para o relógio (timer) do sistema.
// CLOCK1 e CLOCK2 são endereços de registradores de hardware para o timer.
// 0177546 e 0172540 são valores octais, comuns em sistemas PDP-11.
#define CLOCK1 0177546
#define CLOCK2 0172540

// Array 'icode' contém o código de máquina inicial para o processo de inicialização.
// Este é o "código mágico" que o kernel injeta no primeiro processo de usuário (init).
// Os valores são octais e representam instruções PDP-11.
int icode[] = {
    0104413, // mov $014, r0 (Carrega 014 (12 decimal) no registrador r0)
    0000014, // .word 014 (Dados: 12 - provavelmente o número de segmentos)
    0000010, // .word 010 (Dados: 8 - provavelmente o tamanho do segmento de dados)
    0000777, // .word 0777 (Dados: 511 - provavelmente o tamanho do segmento de pilha)
    0000014, // .word 014 (Dados: 12 - outro valor relacionado a segmentos)
    0000000, // .word 0 (Dados: 0)
    0062457, // sys exec; /etc/init (Instrução para executar o programa /etc/init)
    0061564, // (Parte da string "/etc/init")
    0064457, // (Parte da string "/etc/init")
    0064556, // (Parte da string "/etc/init")
    0000164, // (Parte da string "/etc/init" e terminador nulo)
};

// Função principal de inicialização do sistema operacional.
// É a primeira função a ser executada após o bootstrap.
main()
{
    extern schar; // Declaração externa de 'schar' (provavelmente relacionado ao scheduler ou caracteres de inicialização)
    register i, *p; // Declaração de variáveis de registro para otimização.

    updlock = 0; // Inicializa o bloqueio de atualização (para sincronização de disco) como 0 (desbloqueado).
    i = *ka6 + USIZE; // 'i' recebe o endereço base da memória física (*ka6) mais o tamanho da área de usuário (USIZE).
                      // Isso provavelmente calcula o início da memória livre disponível.

    UISD->r[0] = 077406; // Configura o descritor de segmento de instrução (UISD) para o segmento 0.
                         // 077406 (octal) provavelmente define o tamanho e as permissões de acesso (leitura/execução).

    // Loop para determinar a quantidade de memória física disponível.
    // Ele tenta ler de endereços de memória sequenciais até encontrar um erro (memória inválida).
    for (;;)
    {
        UISA->r[0] = i; // Configura o endereço de segmento de instrução (UISA) para o segmento 0 com o valor atual de 'i'.
        if (fuibyte(0) < 0)
            break; // Tenta ler um byte do endereço virtual 0 (mapeado para 'i').
                   // Se 'fuibyte' retornar < 0, significa um erro de acesso à memória, indicando o fim da memória física.
        clearseg(i); // Limpa o segmento de memória no endereço 'i'.
        maxmem++;    // Incrementa o contador de páginas de memória máxima (maxmem).
        mfree(coremap, 1, i); // Libera 1 página de memória no 'coremap' (mapa de alocação de memória física) no endereço 'i'.
                              // Isso adiciona a página recém-descoberta à lista de memória livre.
        i++;                  // Avança para a próxima página de memória.
    }

    // Configuração de Mapeamento de Memória para CPU PDP-11/70.
    // Se a CPU for do tipo 70 (PDP-11/70), configura os registradores de mapeamento de usuário (UBMAP).
    // O PDP-11/70 tinha um sistema de gerenciamento de memória mais avançado.
    if (cputype == 70)
        for (i = 0; i < 62; i = +2)
        {                      // Loop para configurar os 62 registradores de mapeamento.
            UBMAP->r[i] = i << 12; // Configura o registrador de endereço de base (r[i]) com o endereço físico (i * 4096).
            UBMAP->r[i + 1] = 0;   // Configura o registrador de comprimento/permissões (r[i+1]) como 0 (sem mapeamento ou permissões padrão).
        }

    // Exibe a quantidade de memória física detectada.
    // `maxmem` é o número de páginas de memória de 512 bytes.
    // (maxmem * 5 / 16) é uma conversão para Kbytes (512 bytes/página * maxmem * 1024 bytes/KB / 512 bytes/página = maxmem/2).
    // A fórmula (maxmem * 5 / 16) pode ser uma aproximação ou específica para unidades de 16k.
    printf("mem = %l\n", maxmem * 5 / 16);

    maxmem = min(maxmem, MAXMEM); // Limita a memória máxima detectada ao valor definido por MAXMEM.
                                  // Isso garante que o sistema não tente usar mais memória do que o suportado ou configurado.

    mfree(swapmap, nswap, swplo); // Libera o espaço de swap.
                                  // 'swapmap' é o mapa de alocação da área de swap.
                                  // 'nswap' é o número total de blocos de swap.
                                  // 'swplo' é o offset inicial da área de swap.

    // Configuração do último segmento de usuário (provavelmente a pilha).
    UISA->r[7] = ka6[1]; // O segmento 7 do UISA recebe o valor de ka6[1] (endereço de base para a área de usuário).
    UISD->r[7] = 077406; // O segmento 7 do UISD recebe 077406 (tamanho e permissões).

    // Inicialização do relógio (timer) do sistema.
    lks = CLOCK1; // Tenta usar o CLOCK1 como o endereço do relógio.
    if (fuiword(lks) == -1)
    { // Tenta ler uma palavra do endereço do relógio.
        lks = CLOCK2;     // Se a leitura falhar, tenta usar o CLOCK2.
        if (fuiword(lks) == -1)
            panic("no clock"); // Se ambos falharem, o sistema entra em pânico (não há relógio).
    }

    // Inicialização do processo 0 (o processo de sistema, ou "swapper").
    proc[0].p_addr = *ka6;    // O endereço de memória do processo 0 é definido como o endereço base da área de usuário.
    proc[0].p_size = USIZE;   // O tamanho do processo 0 é definido como USIZE (tamanho da área de usuário).
    proc[0].p_stat = SRUN;    // O estado do processo 0 é definido como SRUN (em execução).
    proc[0].p_flag = |SLOAD | SSYS; // As flags do processo 0 são definidas: SLOAD (carregado na memória) e SSYS (processo de sistema).
    u.u_procp = &proc[0];     // O ponteiro para o processo atual na área de usuário (u.u_procp) aponta para o processo 0.

    *lks = 0115; // Inicia o relógio escrevendo 0115 (octal) em seu registrador.
                 // Este valor é um comando para o relógio (e.g., frequência de interrupção).

    cinit(); // Inicializa o console.
    binit(); // Inicializa o buffer cache.
    iinit(); // Inicializa o sistema de i-nodes.

    // Monta o diretório raiz.
    rootdir = iget(rootdev, ROOTINO); // Obtém o i-node do diretório raiz do dispositivo raiz (rootdev) e número de i-node (ROOTINO).
    rootdir->i_flag &= ~ILOCK;     // Libera o bloqueio do i-node raiz.

    // Define o diretório de trabalho atual do usuário.
    u.u_cdir = iget(rootdev, ROOTINO); // Define o diretório de trabalho atual (u.u_cdir) como o diretório raiz.
    u.u_cdir->i_flag &= ~ILOCK;     // Libera o bloqueio do i-node do diretório de trabalho.

    // Cria o primeiro processo de usuário (o processo 'init').
    if (newproc())
    {                         // Chama newproc() para criar um novo processo.
        expand(USIZE + 1);    // Expande a área de usuário para incluir um segmento de texto (USIZE + 1 página).
        estabur(0, 1, 0, 0);  // Estabelece os registros de base e descritor de usuário (segmentos de texto, dados, pilha).
                              // 0 texto, 1 dado (para o icode), 0 pilha, sem separação (segundo plano).
        copyout(icode, 0, sizeof icode); // Copia o código inicial ('icode') para o endereço virtual 0 do novo processo.
        return;               // Retorna, permitindo que o novo processo comece a executar.
    }
    sched(); // Se newproc() retornar 0 (o processo atual é o swapper/init), o scheduler é chamado para iniciar o primeiro processo.
}

// Função sureg(): Atualiza os registradores de segmentos de usuário (UISA e UISD)
// com base na área de usuário do processo atual (u.u_uisa e u.u_uisd).
// Isso é crucial para alternar o contexto de um processo.
sureg()
{
    register *up, *rp, a; // Ponteiros para os arrays de registradores e uma variável para o endereço.

    a = u.u_procp->p_addr; // 'a' recebe o endereço físico base do processo atual.
    up = &u.u_uisa[16];    // 'up' aponta para o final dos registradores UISA na área de usuário (u_uisa).
    rp = &UISA->r[16];     // 'rp' aponta para o final dos registradores de hardware UISA.

    // Ajusta os ponteiros se a CPU for PDP-11/40 (que pode ter menos registradores ou um layout diferente).
    if (cputype == 40)
    {
        up = -8; // Desloca 8 palavras para trás.
        rp = -8; // Desloca 8 palavras para trás.
    }

    // Copia os valores dos registradores UISA da área de usuário para os registradores de hardware UISA.
    // Os endereços são ajustados para serem físicos, adicionando 'a'.
    while (rp > &UISA->r[0])
        *--rp = *--up + a;

    // Se houver um segmento de texto (compartilhado), ajusta o endereço 'a'.
    // Segmentos de texto compartilhados são carregados em um endereço físico fixo,
    // então o offset 'a' precisa ser ajustado para mapear corretamente.
    if ((up = u.u_procp->p_textp) != NULL)
        a = -up->x_caddr; // 'a' é ajustado subtraindo o endereço físico do segmento de texto.

    up = &u.u_uisd[16]; // 'up' aponta para o final dos registradores UISD na área de usuário (u_uisd).
    rp = &UISD->r[16];  // 'rp' aponta para o final dos registradores de hardware UISD.

    // Ajusta os ponteiros se a CPU for PDP-11/40.
    if (cputype == 40)
    {
        up = -8;
        rp = -8;
    }

    // Copia os valores dos registradores UISD da área de usuário para os registradores de hardware UISD.
    // Para segmentos que não são somente leitura (WO), ajusta o endereço subtraindo 'a'.
    while (rp > &UISD->r[0])
    {
        *--rp = *--up;
        if ((*rp & WO) == 0) // Se o registrador de descritor não for somente leitura (WO).
            rp[(UISA - UISD) / 2] = -a; // Ajusta o endereço base no registrador UISA correspondente.
    }
}

// Função estabur(): Estabelece os registradores de base e descritor de usuário
// (UISA e UISD) para um novo processo, configurando seus segmentos de texto, dados e pilha.
// nt: número de páginas de texto
// nd: número de páginas de dados
// ns: número de páginas de pilha
// sep: flag para indicar se os segmentos de texto e dados/pilha são separados (para textos compartilhados).
estabur(nt, nd, ns, sep)
{
    register a, *ap, *dp; // 'a' para endereço, 'ap' para UISA, 'dp' para UISD.

    // Verifica se os segmentos são separados (texto compartilhado).
    if (sep)
    {
        if (cputype == 40)
            goto err; // Se for PDP-11/40 e separado, é um erro (pode não suportar).
        if (nseg(nt) > 8 || nseg(nd) + nseg(ns) > 8)
            goto err; // Verifica se o número de segmentos excede o limite.
    }
    else // Não separado (texto não compartilhado).
        if (nseg(nt) + nseg(nd) + nseg(ns) > 8)
        goto err; // Verifica o número total de segmentos.

    // Verifica se o tamanho total dos segmentos mais a área de usuário excede a memória máxima.
    if (nt + nd + ns + USIZE > maxmem)
        goto err;

    a = 0;                  // 'a' começa em 0 (endereço virtual inicial para o segmento de texto).
    ap = &u.u_uisa[0];      // 'ap' aponta para o início dos registradores UISA na área de usuário.
    dp = &u.u_uisd[0];      // 'dp' aponta para o início dos registradores UISD na área de usuário.

    // Configura o segmento de texto (somente leitura - RO).
    while (nt >= 128)
    {                        // Enquanto houver mais de 128 páginas de texto.
        *dp++ = (127 << 8) | RO; // Configura o descritor de segmento para 128 páginas (127+1) e somente leitura.
        *ap++ = a;           // Configura o endereço base do segmento.
        a = +128;            // Avança o endereço virtual em 128 páginas.
        nt = -128;           // Reduz o número de páginas de texto restantes.
    }
    if (nt)
    {                          // Se houver páginas restantes (menos de 128).
        *dp++ = ((nt - 1) << 8) | RO; // Configura o descritor para as páginas restantes e somente leitura.
        *ap++ = a;             // Configura o endereço base.
    }

    // Se os segmentos são separados, preenche as entradas intermediárias com zero.
    // Isso cria uma "lacuna" entre o texto e os segmentos de dados/pilha.
    if (sep)
        while (ap < &u.u_uisa[8])
        {
            *ap++ = 0;
            *dp++ = 0;
        }

    a = USIZE; // 'a' recebe o tamanho da área de usuário (USIZE), onde os dados começarão.

    // Configura o segmento de dados (leitura/escrita - RW).
    while (nd >= 128)
    {                        // Enquanto houver mais de 128 páginas de dados.
        *dp++ = (127 << 8) | RW; // Configura o descritor de segmento para 128 páginas e leitura/escrita.
        *ap++ = a;           // Configura o endereço base do segmento.
        a = +128;            // Avança o endereço virtual em 128 páginas.
        nd = -128;           // Reduz o número de páginas de dados restantes.
    }
    if (nd)
    {                          // Se houver páginas restantes.
        *dp++ = ((nd - 1) << 8) | RW; // Configura o descritor para as páginas restantes e leitura/escrita.
        *ap++ = a;             // Configura o endereço base.
        a = +nd;             // Avança o endereço virtual.
    }

    // Preenche as entradas intermediárias com zero até o final dos primeiros 8 pares de registradores.
    while (ap < &u.u_uisa[8])
    {
        *dp++ = 0;
        *ap++ = 0;
    }

    // Se os segmentos são separados, preenche as entradas intermediárias até o final dos 16 pares.
    if (sep)
        while (ap < &u.u_uisa[16])
        {
            *dp++ = 0;
            *ap++ = 0;
        }

    a = +ns; // 'a' avança pelo tamanho do segmento de pilha.

    // Configura o segmento de pilha (leitura/escrita - RW), que cresce para baixo.
    while (ns >= 128)
    {                        // Enquanto houver mais de 128 páginas de pilha.
        a = -128;            // Retrocede o endereço virtual em 128 páginas.
        ns = -128;           // Reduz o número de páginas de pilha restantes.
        *--dp = (127 << 8) | RW; // Configura o descritor de segmento para 128 páginas e leitura/escrita (decrementando dp).
        *--ap = a;           // Configura o endereço base (decrementando ap).
    }
    if (ns)
    {                          // Se houver páginas restantes.
        *--dp = ((128 - ns) << 8) | RW | ED; // Configura o descritor para as páginas restantes, leitura/escrita e ED (crescimento para baixo).
        *--ap = a - 128;       // Configura o endereço base.
    }

    // Se os segmentos não são separados, copia os primeiros 8 pares de registradores (texto/dados)
    // para as posições 8 a 15, efetivamente duplicando o mapeamento para permitir mais flexibilidade.
    if (!sep)
    {
        ap = &u.u_uisa[0];
        dp = &u.u_uisa[8];
        while (ap < &u.u_uisa[8])
            *dp++ = *ap++;
        ap = &u.u_uisd[0];
        dp = &u.u_uisd[8];
        while (ap < &u.u_uisd[8])
            *dp++ = *ap++;
    }

    sureg();    // Chama sureg() para aplicar as configurações de registradores de usuário ao hardware.
    return (0); // Retorna 0 em caso de sucesso.

err:
    u.u_error = ENOMEM; // Define o erro como ENOMEM (memória insuficiente).
    return (-1);        // Retorna -1 em caso de erro.
}

// Função nseg(): Calcula o número de segmentos necessários para um dado número de páginas.
// Um segmento em PDP-11 tem 128 páginas (64KB).
nseg(n)
{
    return ((n + 127) >> 7); // (n + 127) garante que mesmo uma única página ocupe um segmento inteiro.
                             // >> 7 é equivalente a dividir por 128 (2^7).
}