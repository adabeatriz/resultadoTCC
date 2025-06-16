      PROGRAM FF
C Este programa, 'FF', é uma ferramenta de pré-processamento para a criação de decks de entrada NASTRAN em formato de campo livre.
C Ele permite que o usuário crie ou modifique arquivos de entrada NASTRAN, oferecendo funcionalidades como salvamento,
C substituição de linhas e assistência ao usuário. Ele detecta o tipo de máquina para ajustar seu comportamento.

CDC   PROGRAM FF (TAPE3)
C Esta linha é um comentário específico para sistemas CDC, indicando que o programa 'FF' utiliza a unidade de fita 'TAPE3'.
C Isso pode ser uma convenção ou um requisito específico do sistema operacional CDC para alocar arquivos.

      LOGICAL       STAR, PCT, NOTYET, PUNCH, UPFLAG
C Declaração de variáveis lógicas (booleanas):
C - STAR: Usado para indicar um estado ou flag, seu uso específico não é claro sem mais contexto do programa.
C - PCT: (Percent?) Provavelmente um flag relacionado a porcentagens ou um modo de operação.
C - NOTYET: Indica algo que ainda não aconteceu ou não foi processado.
C - PUNCH: Flag para determinar se o deck NASTRAN deve ser "perfurado" (gravado em um arquivo de saída formatado para cartão).
C - UPFLAG: Flag para controle de conversão para maiúsculas (uppercase).

      INTEGER       SCREEN, PROM, FFFLAG, FACSF, NC(7),
     1              IBM, UNIVAC, CDC, VAX, PC,
     2              UNIX
C Declaração de variáveis inteiras:
C - SCREEN: Unidade de saída para a tela (console).
C - PROM: (Prompt?) Provavelmente um flag para controlar prompts de entrada do usuário.
C - FFFLAG: Flag para indicar o modo de entrada Free-Field (campo livre). Um valor 1234 parece ser usado para ativá-lo.
C - FACSF: Função ou variável relacionada ao tamanho do arquivo (File Access Size Function?).
C - NC(7): Um array que armazena valores numéricos possivelmente relacionados a diferentes tipos de máquinas.
C - IBM, UNIVAC, CDC, VAX, PC, UNIX: Variáveis inteiras usadas para identificar o tipo de máquina em que o programa está sendo executado.
C   Elas provavelmente armazenam códigos numéricos que representam cada plataforma.

      CHARACTER*1   FN(1), BK1, N1, Y1, X1,
     1              S1, W1, A1, H1, CARD1(5),
     2              FX, MARKQ, QMARK, TMP, RPRN,
     3              SPL(8), DOT, D1, T1
C Declaração de variáveis de caractere de comprimento 1:
C - FN(1): Array de caracteres para armazenar partes de nomes de arquivo (equivale a FN4, FN6, FN32, FNAME).
C - BK1: Caractere para um espaço em branco (' ').
C - N1: Caractere 'N' (de 'No').
C - Y1: Caractere 'Y' (de 'Yes').
C - X1: Caractere para entrada genérica do usuário (ex: 'X' para punch).
C - S1: Caractere 'S' (de 'Stop').
C - W1: Caractere 'W' (de 'Overwrite').
C - A1: Caractere 'A' (de 'Append').
C - H1: Caractere 'H' (de 'Help').
C - CARD1(5): Array de caracteres, parte de uma superposição para leitura de cartões.
C - FX: Caractere 'X', usado para um modo de punching alternativo.
C - MARKQ: Caractere de marca de pergunta (?).
C - QMARK: Caractere '?' para o prompt.
C - TMP: Caractere temporário.
C - RPRN: Caractere ')', usado na verificação de formato de entrada.
C - SPL(8): Array de caracteres, parte de uma superposição para dados que "transbordam" (SPILL).
C - DOT: Caractere '.' (ponto).
C - D1: Caractere 'D'.
C - T1: Caractere 'T'.

      CHARACTER*2   CARD2, REPL
C Declaração de variáveis de caractere de comprimento 2:
C - CARD2: Parte de uma superposição para leitura de cartões.
C - REPL: Caractere para "R:" (de 'Replace').

      CHARACTER*3   NEW, OLD, ODNW
C Declaração de variáveis de caractere de comprimento 3:
C - NEW: Caractere 'NEW' (para status de arquivo novo).
C - OLD: Caractere 'OLD' (para status de arquivo existente).
C - ODNW: Variável para armazenar o status do arquivo (OLD/NEW).

      CHARACTER*4   CARD(20), SAVE(20), BEGN, HELP, STOP,
     1              LIST, BLANK, END1, END2, END3,
     2              CANC1, CANC2, ALTER, CARD4, FN4,
     3              BBK(2), KSMB(9)
C Declaração de variáveis de caractere de comprimento 4:
C - CARD(20): Array para armazenar uma linha de entrada de cartão (até 80 colunas, 20 * 4 caracteres).
C - SAVE(20): Array para armazenar uma cópia da linha de entrada.
C - BEGN: Caractere 'BEGI' (de 'BEGIN').
C - HELP: Caractere 'HELP'.
C - STOP: Caractere 'STOP'.
C - LIST: Caractere 'LIST'.
C - BLANK: Caracteres '    ' (quatro espaços em branco).
C - END1, END2, END3: Caracteres para diferentes formas de "ENDDATA" ('ENDD', 'END ', 'ENDA').
C - CANC1: Caractere 'CANC' (de 'CANCEL').
C - CANC2: Caractere 'EL  ' (de 'CANCEL').
C - ALTER: Caractere 'LTER' (de 'ALTER').
C - CARD4: Parte de uma superposição para leitura de cartões.
C - FN4: Parte de uma superposição para nomes de arquivo.
C - BBK(2): Array de caracteres '(BLA', 'NK) ' usado para formar a string '(BLANK)'.
C - KSMB(9): Array de caracteres usado para símbolos de continuação (+C0N a +C8N).

      CHARACTER*6   FN6, BEGN6, HELP6, STOP6, CARD6,
     1              MTYPE(7), CNTLWD(3)
C Declaração de variáveis de caractere de comprimento 6:
C - FN6: Parte de uma superposição para nomes de arquivo.
C - BEGN6: Caractere 'BEGIN ' (de 'BEGIN ').
C - HELP6: Caractere 'HELP  ' (de 'HELP  ').
C - STOP6: Caractere 'STOP  ' (de 'STOP  ').
C - CARD6: Parte de uma superposição para leitura de cartões.
C - MTYPE(7): Array que armazena os nomes dos tipos de máquinas ('IBM PC', '  IBM', etc.).
C - CNTLWD(3): Array para armazenar palavras de controle ('CANCEL', 'PROMPT', 'LIST  ').

      CHARACTER*8   FNAME(4), MYFILE, BLNK8, SITE, CNTRL,
     1              USE(2), SYM(4), TAPEO3, TPF, FOROO3,
     2              DORK, KEEP, SPILL
C Declaração de variáveis de caractere de comprimento 8:
C - FNAME(4): Array para armazenar o nome completo do arquivo (até 32 caracteres, 4 * 8 caracteres).
C - MYFILE: Caractere 'MIFYLE' (nome de arquivo padrão).
C - BLNK8: Caracteres '        ' (oito espaços em branco).
C - SITE: ID do local de perfuração do cartão.
C - CNTRL: Caractere 'CENTRAL'.
C - USE(2): Array de caracteres '@USE 3.,' e 'FF$$ .  '.
C - SYM(4): Array de caracteres '@SYM    ', 'PUNCH$,,', '****', ' . '.
C - TAPEO3: Caractere 'TAPE3.' (nome de arquivo para CDC).
C - TPF: Caractere 'FF$$.' (nome de arquivo para UNIVAC).
C - FOROO3: Caractere 'FT03.' (nome de arquivo para VAX/UNIVAC).
C - DORK: Variável para status de fechamento de arquivo (DELETE ou KEEP).
C - KEEP: Caractere 'KEEP'.
C - SPILL: Parte de uma superposição, armazena dados que "transbordam".

      CHARACTER*11  F
C Declaração de variável de caractere de comprimento 11:
C - F: Usado para o cabeçalho do programa, 'FF        FF'.

      CHARACTER*32  FN32
C Declaração de variável de caractere de comprimento 32:
C - FN32: Parte de uma superposição para nomes de arquivo (comprimento total de FNAME).

      COMMON /SYSTEM/ IBUF, NOUT, NOGO, IN, ISYS(15)
C Declaração de bloco comum 'SYSTEM':
C Compartilha variáveis relacionadas ao sistema e E/S (Input/Output).
C - IBUF: Buffer de entrada.
C - NOUT: Unidade de saída padrão (normalmente console).
C - NOGO: Flag para indicar um erro ou condição de "não ir".
C - IN: Unidade de entrada padrão (normalmente teclado).
C - ISYS(15): Array para parâmetros do sistema.

      COMMON /MACHIN/ MACH
C Declaração de bloco comum 'MACHIN':
C Compartilha a variável que armazena o tipo de máquina detectado.
C - MACH: Armazena o código numérico da máquina (IBM, UNIVAC, CDC, VAX, PC, UNIX).

      COMMON /XXREAD/ INFLAG, INSAVE, LOOP4, IBMCDC
C Declaração de bloco comum 'XXREAD':
C Compartilha variáveis relacionadas à leitura de entrada.
C - INFLAG: Flag de entrada.
C - INSAVE: Salva o valor da unidade de entrada.
C - LOOP4: Variável relacionada a loops ou contagem.
C - IBMCDC: Flag para indicar sistemas IBM ou CDC.

      COMMON /XECHOX/ FFFLAG, IECHO(3), ISORT(5)
C Declaração de bloco comum 'XECHOX':
C Compartilha variáveis relacionadas ao eco de entrada.
C - FFFLAG: (Já declarado e explicado, mas presente no common block).
C - IECHO(3): Array para controle de eco.
C - ISORT(5): Array para ordenação (?).

      COMMON /XREADX/ SCREEN, LOOP, KOUNT, PROM, NOTYET,
     1              STAR, PCT, ICONT(36)
C Declaração de bloco comum 'XREADX':
C Compartilha variáveis relacionadas à leitura de entrada e controle de fluxo.
C - SCREEN: (Já declarado e explicado).
C - LOOP: Variável de loop ou contador.
C - KOUNT: Contador.
C - PROM: (Já declarado e explicado).
C - NOTYET: (Já declarado e explicado).
C - STAR: (Já declarado e explicado).
C - PCT: (Já declarado e explicado).
C - ICONT(36): Array para controle de continuação.

      COMMON /QMARKQ/ MARKQ, TMP(16), SPILL, SAVE
C Declaração de bloco comum 'QMARKQ':
C Compartilha variáveis relacionadas a marcas de pergunta e salvamento.
C - MARKQ: (Já declarado e explicado).
C - TMP(16): Array de caracteres temporário.
C - SPILL: (Já declarado e explicado).
C - SAVE: (Já declarado e explicado).

      COMMON /UPCASX/ UPFLAG, UPID(3)
C Declaração de bloco comum 'UPCASX':
C Compartilha variáveis relacionadas à conversão para maiúsculas.
C - UPFLAG: (Já declarado e explicado).
C - UPID(3): Array para identificação de maiúsculas.

      EQUIVALENCE   (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),
     1              (CARD(1),CARD6,CARD1(1),CARD2,CARD4),
     2              (SPL(1),SPILL)
C Declaração de EQUIVALENCE:
C Permite que diferentes variáveis compartilhem a mesma área de memória.
C - (FN(1),FN4,FN6,FN32,FNAME(1)): Todas essas variáveis referem-se ao mesmo espaço de memória,
C   permitindo que o nome do arquivo seja acessado com diferentes comprimentos (1, 4, 6, 32 caracteres ou como array de 8 caracteres).
C - (CARD(1),CARD6,CARD1(1),CARD2,CARD4): Permite acessar a mesma linha de cartão com diferentes formatos e comprimentos.
C - (SPL(1),SPILL): Permite acessar o array SPL ou a variável SPILL na mesma área de memória.

      DATA          BEGN, HELP, STOP, BLANK, BK1   /
     1              'BEGI', 'HELP', 'STOP', '    ', ' '   /
C Inicialização de variáveis com seus valores de dados:
C - BEGN: 'BEGI'
C - HELP: 'HELP'
C - STOP: 'STOP'
C - BLANK: '    '
C - BK1: ' ' (espaço em branco)

      DATA          BEGN6, HELP6, STOP6, BLNK8, KEEP  /
     1              'BEGIN ', 'HELP  ', 'STOP  ', '        ', 'KEEP' /
C Inicialização de variáveis com seus valores de dados:
C - BEGN6: 'BEGIN '
C - HELP6: 'HELP  '
C - STOP6: 'STOP  '
C - BLNK8: '        '
C - KEEP: 'KEEP'

      DATA          END1, END2, END3, N1, ALTER /
     1              'ENDD', 'END ', 'ENDA', 'N', 'LTER' /
C Inicialização de variáveis com seus valores de dados:
C - END1: 'ENDD'
C - END2: 'END '
C - END3: 'ENDA'
C - N1: 'N'
C - ALTER: 'LTER'

      DATA          CANC1, CANC2, DORK, LLI, LLJ   /
     1              'CANC', 'EL  ', 'DELETE', 4H  I , 4H  J  /
C Inicialização de variáveis com seus valores de dados:
C - CANC1: 'CANC'
C - CANC2: 'EL  '
C - DORK: 'DELETE' (status de arquivo para ser deletado)
C - LLI: 4H  I (representação de constante Hollerith para '  I')
C - LLJ: 4H  J (representação de constante Hollerith para '  J')
C   (Estas duas últimas são usadas para determinar o tipo de máquina pela diferença de endereço.)

      DATA          CNTRL, MYFILE, NEW, OLD, Y1    /
     1              'CENTRAL','MIFYLE', 'NEW', 'OLD', 'Y'   /
C Inicialização de variáveis com seus valores de dados:
C - CNTRL: 'CENTRAL'
C - MYFILE: 'MIFYLE'
C - NEW: 'NEW'
C - OLD: 'OLD'
C - Y1: 'Y'

      DATA          NC/ 12, 7, 28, 7, 28, 0, 28        /
C Inicialização do array NC:
C NC(1) = 12, NC(2) = 7, NC(3) = 28, NC(4) = 7, NC(5) = 28, NC(6) = 0, NC(7) = 28
C Estes valores provavelmente correspondem ao comprimento máximo de nomes de arquivo ou outros parâmetros por tipo de máquina.

      DATA          UNIVAC, IBM, CDC, VAX, PC, UNIX           /
     1              3,      2,   4,   5,   1,   7            /
C Inicialização dos códigos numéricos para cada tipo de máquina:
C - UNIVAC: 3
C - IBM: 2
C - CDC: 4
C - VAX: 5
C - PC: 1
C - UNIX: 7

      DATA          MTYPE                                   /
     1              'IBM PC', '  IBM', 'UNIVAC', '  CDC', '  VAX',
     2              ' *** ', '  UNIX'                      /
C Inicialização do array MTYPE com os nomes textuais das máquinas:
C - MTYPE(1) = 'IBM PC'
C - MTYPE(2) = '  IBM'
C - MTYPE(3) = 'UNIVAC'
C - MTYPE(4) = '  CDC'
C - MTYPE(5) = '  VAX'
C - MTYPE(6) = ' *** ' (provavelmente um placeholder ou não usado)
C - MTYPE(7) = '  UNIX'

      DATA          TAPEO3, TPF, FOROO3, F             /
     1              'TAPE3.', 'FF$$.', 'FT03.', 'FF        FF'  /
C Inicialização de variáveis de nomes de arquivo e cabeçalho:
C - TAPEO3: 'TAPE3.'
C - TPF: 'FF$$.'
C - FOROO3: 'FT03.'
C - F: 'FF        FF' (para o cabeçalho do programa)

      DATA          S1, A1, BBK,                  LIST  /
     1              'S',  'A',  '(BLA', 'NK) ',    'LIST' /
C Inicialização de variáveis (algumas já definidas, mas redefinidas aqui, o que pode indicar uso específico):
C - S1: 'S'
C - A1: 'A'
C - BBK: '(BLA', 'NK) '
C - LIST: 'LIST'

      DATA          W1, H1, FX, RPRN, ICNTL /
     1              'O', 'H', 'X', ')', 0   /
C Inicialização de variáveis:
C - W1: 'O' (de 'Overwrite')
C - H1: 'H' (de 'Help')
C - FX: 'X'
C - RPRN: ')'
C - ICNTL: 0 (contador de cartões cancelados)

      DATA          DOT, D1, T1                     /
     1              '.', 'D', 'T'                   /
C Inicialização de variáveis:
C - DOT: '.'
C - D1: 'D'
C - T1: 'T'

      DATA          CNTLWD,                  NCNTL, REPL  /
     1              'CANCEL', 'PROMPT', 'LIST  ', 3,       'R:'  /
C Inicialização de variáveis:
C - CNTLWD: 'CANCEL', 'PROMPT', 'LIST  '
C - NCNTL: 3 (número de palavras de controle)
C - REPL: 'R:'

      DATA          KSMB / '+C0N', '+C1N', '+C2N', '+C3N' ,
     1              '+C4N', '+C5N', '+C6N', '+C7N', '+C8N' /
C Inicialização do array KSMB com os símbolos de continuação NASTRAN:
C - KSMB(1) a KSMB(9)

      DATA    USE / '@USE 3.,', 'FF$$ .  '/        PUNCH /   .FALSE./
C Inicialização de variáveis:
C - USE: '@USE 3.,', 'FF$$ .  '
C - PUNCH: .FALSE. (inicialmente, não perfurar cartões)

      DATA    SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '           /
C Inicialização de variáveis:
C - SYM: '@SYM    ', 'PUNCH$,,', '****', ' . '

      DATA          QMARK / '?' /
C Inicialização de QMARK:
C - QMARK: '?'

      J = LLJ - LLI
C Calcula a diferença entre os endereços de memória de LLJ e LLI.
C Esta é uma técnica comum em Fortran antigo para tentar determinar o tamanho de uma palavra de máquina
C ou a arquitetura do sistema, pois a diferença de endereços pode variar entre diferentes plataformas.

      MACH = IBM
C Inicializa a variável MACH com o código para IBM como padrão.

      IF (J .EQ.   256) MACH = PC
C Se a diferença J for 256, define a máquina como PC.

      IF (J .EQ.   512) MACH = UNIVAC
C Se a diferença J for 512, define a máquina como UNIVAC.

      IF (J .GT. 65535) MACH = VAX
C Se a diferença J for maior que 65535, define a máquina como VAX.

      IF (J .GT. 2**30) MACH = CDC
C Se a diferença J for maior que 2^30, define a máquina como CDC.

      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))
     1    MACH = UNIX
C Condição mais complexa para distinguir VAX de UNIX, baseada em um valor de ponto flutuante.
C Isso pode ser uma forma de detectar a representação de ponto flutuante, que varia entre arquiteturas.
C XXI não está declarado, o que pode indicar um erro de digitação ou que é uma variável de sistema implícita.

      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))
     1    MACH = PC
C Outra condição para distinguir VAX de PC, também baseada em um valor de ponto flutuante.

      LU    = 2
C Define LU (unidade lógica de leitura) como 2.

      LOUT  = 3
C Define LOUT (unidade lógica de saída) como 3.

      IN    = 5
C Define IN (unidade lógica de entrada padrão) como 5.

      NOUT  = 6
C Define NOUT (unidade lógica de saída padrão) como 6.

      IPUN  = 7
C Define IPUN (unidade lógica para perfuração de cartões) como 7.

      SCREEN  = 6
C Define SCREEN (unidade de saída para a tela) como 6 (mesmo que NOUT).

      IF (MACH .EQ. VAX) SCREEN = 5
C Se a máquina for VAX, a tela de saída é 5 (pode ser um comportamento específico do VAX).

      LOOP    =-1
C Inicializa LOOP como -1.

      KOUNT   = 0
C Inicializa KOUNT como 0.

      KONTN   = 10000000
C Inicializa KONTN (contador de continuação) com um valor alto.

      IKI     = 1
C Inicializa IKI (índice para símbolos de continuação KSMB) como 1.

      PROM    =+1
C Inicializa PROM (prompt) como +1.

      STAR    =.FALSE.
C Inicializa STAR como falso.

      PCT     =.FALSE.
C Inicializa PCT como falso.

      NOTYET  =.FALSE.
C Inicializa NOTYET como falso.

      MARKQ   = QMARK
C Define MARKQ como o caractere de marca de pergunta '?'.

      IECHO(2)=-2
C Inicializa um elemento do array IECHO.

      INSAVE  = IN
C Salva o valor da unidade de entrada padrão.

      INFLAG  = 0
C Inicializa INFLAG como 0.

      FFFLAG  = 0
C Inicializa FFFLAG como 0.

      UPFLAG  =.FALSE.
C Inicializa UPFLAG como falso.

      ISYS(15)= 0
C Inicializa um elemento do array ISYS.

      J       = NC(MACH)
C Define J com o valor apropriado do array NC com base na máquina detectada.

      CARD4   = BLANK
C Inicializa CARD4 com espaços em branco.

      CARD6   = STOP6
C Inicializa CARD6 com 'STOP  '.

      FN4     = STOP
C Inicializa FN4 com 'STOP'.

      FN6     = STOP6
C Inicializa FN6 com 'STOP  '.

      LOOP4   = LOOP - 4
C Inicializa LOOP4 com LOOP - 4.

      DO 5 I = 1,20
C Inicia um loop para inicializar o array SAVE.

5     SAVE(I) = BLANK
C Define cada elemento de SAVE como espaços em branco.

      IBMCDC = UNIVAC + VAX + PC
C Calcula um valor para IBMCDC somando os códigos das máquinas. Isso pode ser um truque para agrupar máquinas não-IBM/CDC.

      IF (MACH.EQ.IBM .OR. MACH.EQ.CDC) IBMCDC = 0
C Se a máquina for IBM ou CDC, redefine IBMCDC para 0.
C Isso significa que IBMCDC será 0 para IBM e CDC, e um valor diferente de zero para UNIVAC, VAX e PC.

      IF (MACH .NE. CDC) GO TO 20
C Se a máquina não for CDC, pula para o rótulo 20.
C Este bloco de código é específico para a configuração de arquivos em sistemas CDC.

      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')
C Abre a unidade IN (5) associada ao arquivo 'INPUT' com status 'UNKNOWN'.

      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')
C Abre a unidade NOUT (6) associada ao arquivo 'OUTPUT' com status 'UNKNOWN'.

      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')
C Abre a unidade IPUN (7) associada ao arquivo 'PUNCH' com status 'UNKNOWN'.

      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)
C Abre a unidade LOUT (3) associada ao arquivo 'TAPE3' com status 'UNKNOWN'.
C Se ocorrer um erro na abertura, desvia para o rótulo 15.

      GO TO 20
C Pula para o rótulo 20, ignorando a mensagem de erro.

15    STOP 'SCRATCH FILE ERROR, UNIT 3'
C Se ocorrer um erro na abertura de 'TAPE3' (unidade 3), o programa para e exibe esta mensagem.

20    WRITE  (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)
C Escreve o cabeçalho do programa na unidade de saída padrão (NOUT).
C Imprime a variável F 7 vezes, seguida pelo tipo de máquina, e F novamente 4 vezes.

25    FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),
     1              2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,
     2              /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,
     3              'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',
     4              '  30602')
C Formato da saída do cabeçalho do programa.
C Contém informações sobre o programa, versão (Abril de 93), e informações de contato (COSMIC, Universidade da Geórgia).

30    WRITE  (NOUT,35) J
C Solicita ao usuário para inserir um nome de arquivo ou 'HELP'.
C Exibe o valor de J (comprimento máximo do nome do arquivo).

35    FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO',
     1 ',       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')
C Formato da mensagem de solicitação de entrada.

      READ (IN,40,ERR=330,END=330) FNAME
C Lê o nome do arquivo de entrada do usuário.
C Se ocorrer um erro ou fim de arquivo, desvia para o rótulo 330.

40    FORMAT (4A8)
C Formato para ler o nome do arquivo (4 campos de 8 caracteres, totalizando 32 caracteres).

      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4)
C Se o primeiro componente do nome do arquivo for 'MIFYLE', chama a sub-rotina FFHELP.
C Os asteriscos '*' indicam rótulos alternativos de retorno.
C - *30: Retorna para o rótulo 30 (repetir prompt de nome de arquivo).
C - *600: Retorna para o rótulo 600 (fim do programa).
C - 4: Argumento para FFHELP, indicando um tipo específico de ajuda.

      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE
C Se o primeiro componente do nome do arquivo estiver em branco, define-o como 'MIFYLE' (nome padrão).

      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1)
     1    GO TO 330
C Verifica se há caracteres indesejados após o comprimento máximo do nome do arquivo (J).
C Se houver, desvia para o rótulo 330 (nome de arquivo inválido).

      IF (FNAME(2) .NE. BLNK8) GO TO 50
C Se o segundo componente do nome do arquivo não estiver em branco, pula para o rótulo 50.
C Isso indica que o nome do arquivo é mais longo que 8 caracteres ou contém algo.

      CALL UPCASE (FNAME,J)
C Chama a sub-rotina UPCASE para converter o nome do arquivo para maiúsculas.
C J é o comprimento efetivo do nome do arquivo.

      IF (FN6 .EQ. BEGN6) GO TO 330
C Se os primeiros 6 caracteres do nome do arquivo forem 'BEGIN ', desvia para o rótulo 330 (nome de arquivo inválido).
C 'BEGIN' é um comando interno, não um nome de arquivo.

      IF (FN6 .EQ. STOP6) GO TO 600
C Se os primeiros 6 caracteres do nome do arquivo forem 'STOP  ', desvia para o rótulo 600 (fim do programa).

      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1)
C Se os primeiros 6 caracteres do nome do arquivo forem 'HELP  ', chama FFHELP.
C - 1: Argumento para FFHELP, indicando um tipo específico de ajuda.

50    ODNW = OLD
C Define o status do arquivo como 'OLD' (existente) inicialmente.

      IF (MACH .NE. VAX) GO TO 60
C Se a máquina não for VAX, pula para o rótulo 60.
C Este bloco é específico para sistemas VAX para adicionar a extensão ".DAT" se ausente.

      DO 52 I = 2,J
C Loop para procurar um ponto '.' ou um espaço em branco ' ' no nome do arquivo.

      IF (FN(I) .EQ. DOT) GO TO 60
C Se encontrar um ponto, pula para o rótulo 60 (extensão já existe).

      IF (FN(I) .EQ. BK1) GO TO 55
C Se encontrar um espaço em branco, pula para o rótulo 55 (fim do nome antes da extensão).

52    CONTINUE
C Continua o loop.

      I = J + 1
C Se não encontrou ponto ou espaço, define I para a posição após o último caractere válido.

55    FN(I  ) = DOT
C Adiciona o caractere '.' (ponto) ao nome do arquivo.

      FN(I+1) = D1
C Adiciona 'D' (de DAT).

      FN(I+2) = A1
C Adiciona 'A' (de DAT).

      FN(I+3) = T1
C Adiciona 'T' (de DAT).

60    IF (MACH .NE. IBM) GO TO 65
C Se a máquina não for IBM, pula para o rótulo 65.
C Este bloco é específico para sistemas IBM para verificar a existência do arquivo.

      I = IQZDDN(FNAME(1))
C Chama a função IQZDDN para verificar a existência do arquivo. Retorna 0 se o arquivo não existe.

      ODNW = OLD
C Mantém o status do arquivo como 'OLD'.

      IF (I .EQ. 0) ODNW = NEW
C Se IQZDDN retornou 0 (arquivo não existe), define o status como 'NEW'.

      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)
C Se o arquivo for novo, chama QQDCBF para definir o descritor do bloco de controle (DCB) do arquivo.
C Parâmetros: nome do arquivo, 0, 'F  ', 80 (largura do registro), 80 (tamanho do bloco), DA (?).

      CALL QQGETF (LU,FNAME(1),IERR)
C Chama QQGETF para obter o arquivo lógico (LU).
C IERR (código de erro) é retornado.

      IF (IERR .NE. 0) GO TO 130
C Se houver um erro, desvia para o rótulo 130.

65    IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130)
C Se IBMCDC for 0 (IBM ou CDC), abre o arquivo usando FNAME(1).
C Status ODNW ('OLD' ou 'NEW'). Se houver erro, desvia para 130.

      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32   ,STATUS=ODNW,ERR=130)
C Se IBMCDC não for 0 (UNIVAC, VAX ou PC), abre o arquivo usando FN32 (nome completo).
C Status ODNW. Se houver erro, desvia para 130.

      IF (ODNW .EQ. NEW) GO TO 140
C Se o arquivo foi aberto como 'NEW', pula para o rótulo 140 (continua com a entrada).

70    WRITE  (NOUT,80)
C Se o arquivo já existe, solicita ao usuário uma ação.

80    FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',',
     1         ' OR ''APPEND'' -')
C Formato da mensagem: "Arquivo já existe, digite 'STOP', 'OVERWRITE' ou 'APPEND'".

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C Se a máquina for CDC e a unidade de entrada for 5, rebobina a unidade de entrada.
C Isso pode ser necessário para garantir que a próxima leitura comece do início.

      READ (IN,90,END=70) X1
C Lê a resposta do usuário (um caractere). Se ocorrer fim de arquivo, repete o prompt (vai para 70).

90    FORMAT (A1)
C Formato para ler um caractere.

      CALL UPCASE (X1,1)
C Converte a entrada do usuário para maiúsculas.

      IF (X1 .EQ. S1) GO TO 480
C Se a entrada for 'S' (Stop), desvia para 480 (limpa nome de arquivo, vai para fim).

      IF (X1 .EQ. W1) GO TO 140
C Se a entrada for 'O' (Overwrite), desvia para 140 (continua, sobrescrevendo).

      IF (X1 .NE. A1) GO TO 70
C Se a entrada não for 'A' (Append), repete o prompt (vai para 70).

      SAVE(2) = BBK(1)
C Se 'Append', adiciona parte do string '(BLANK)' para fins de formatação interna (?).

      SAVE(3) = BBK(2)
C Adiciona a outra parte do string '(BLANK)'.

110   READ (LU,180,END=115) SAVE
C Lê a linha do arquivo existente na unidade LU para o array SAVE.
C Se ocorrer fim de arquivo, desvia para 115.

      IF (SAVE(1).EQ.BEGN .AND. SAVE(4).EQ.BLANK) FFFLAG = 1234
C Se a primeira parte do SAVE for 'BEGI' e a quarta for BLANK, define FFFLAG como 1234.
C Isso provavelmente indica que o arquivo contém dados no formato Free-Field.

      IF (SAVE(19) .EQ. KSMB(IKI)) IKI = IKI + 1
C Verifica se a coluna 73 (19º elemento, 4 caracteres cada) contém um símbolo de continuação NASTRAN.
C Se sim, incrementa o índice IKI.

      GO TO 110
C Continua lendo o arquivo.

115   BACKSPACE LU
C Volta uma linha no arquivo LU.

      IF (FFFLAG .EQ. 1234) WRITE (NOUT,120)
C Se FFFLAG for 1234 (arquivo com Free-Field), exibe uma mensagem de aviso.

120   FORMAT (/,' IF EXISTING FILE CONTAINS FREE-FIELD INPUT CARDS, ',
     1         ' THIS PROGRAM WILL NOT', /5X,'EXPAND THEM TO FIXED-',
     2         ' FIELD FORMATS',/)
C Mensagem de aviso sobre arquivos Free-Field existentes.

      WRITE (NOUT,255) SAVE
C Exibe a última linha lida do arquivo (que foi recuada com BACKSPACE).

      IF (FFFLAG.EQ.1234 .AND. INFLAG.EQ.0) CALL FFHELP (*125,*125,5)
C Se FFFLAG for 1234 e INFLAG for 0, chama FFHELP.
C - 5: Argumento para FFHELP.

125   CARD(1) = SAVE(1)
C Copia o primeiro elemento de SAVE para CARD.

      CARD(2) = SAVE(2)
C Copia o segundo elemento de SAVE para CARD.

      GO TO 140
C Pula para o rótulo 140.

130   IF (ODNW .EQ. NEW) GO TO 310
C Se ODNW for 'NEW', desvia para 310 (erro ao atribuir arquivo).

      ODNW = NEW
C Redefine ODNW para 'NEW'.

      GO TO 60
C Tenta abrir o arquivo novamente com o status 'NEW'.

140   IF (MACH .EQ. UNIVAC) J = FACSF(USE)
C Se a máquina for UNIVAC, chama a função FACSF com o array USE.

      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8
C Se o nome do arquivo for o padrão 'MIFYLE', redefine-o para espaços em branco.

      IF (FNAME(1) .EQ.  BLNK8) WRITE (NOUT,150)
C Se o nome do arquivo for em branco, exibe uma mensagem.

150   FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)
C Mensagem: "Saída não será salva".

      WRITE  (NOUT,160)
C Exibe o cabeçalho principal do programa.

160   FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',
     1  /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2  /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',
     3  //5X,'PLEASE BEGIN -',/)
C Mensagem de boas-vindas e instruções para o programa Free-Field NASTRAN.
C Informa que não haverá eco de entrada até "BEGIN BULK" e como terminar o trabalho.

170   CALL FFREAD (*320,CARD)
C Chama a sub-rotina FFREAD para ler uma linha de entrada.
C - *320: Rótulo de retorno em caso de erro.
C - CARD: Array onde a linha lida será armazenada.

      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230
C Se a entrada for 'CANC' 'EL  ' (CANCEL), desvia para 230.

      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230
C Se a entrada for 'LIST' e o segundo campo estiver em branco, desvia para 230.

      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350
C Se a entrada for 'R:' e o quarto campo estiver em branco, desvia para 350 (Replace).

      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400
C Se a entrada for 'STOP' e o segundo campo estiver em branco, desvia para 400.

      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370
C Se a entrada for completamente em branco, desvia para 370 (continuação?).

      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)
     1    CALL FFHELP (*280,*400,2)
C Se a entrada for 'HELP' e o segundo campo estiver em branco, chama FFHELP.
C - 2: Argumento para FFHELP.

      IF (LU .EQ. 2) WRITE (LU,180) CARD
C Se a unidade LU for 2, escreve o conteúdo de CARD para LU.
C LU=2 é o arquivo de saída temporário, LU=6 é a tela (NOUT).

180   FORMAT (20A4)
C Formato para escrever uma linha de 80 caracteres (20 campos de 4 caracteres).

      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD
C Se FFFLAG não for 1234 e INFLAG for 4, ecoa a entrada para a saída padrão (NOUT).
C Isso provavelmente controla o eco de entrada após "BEGIN BULK".

190   FORMAT (1X,20A4)
C Formato para ecoar a linha de entrada com um espaço inicial.

      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340
C Se a entrada for 'BEGI' e o quinto campo estiver em branco, desvia para 340.
C Isso corresponde a 'BEGIN BULK'.

      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)
     1    GO TO 170
C Se a entrada não for 'ENDD', 'END ', ou 'ENDA', continua o loop de leitura (vai para 170).

      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170
C Se o segundo campo for em branco ou 'LTER' (ALTER), continua o loop de leitura.
C Isso significa que "END" ou "END ALTER" são tratados diferentemente.

      GO TO 410
C Se for um comando de 'END' válido, desvia para 410.

230   IF (LU .NE. 2) GO TO 290
C Se a unidade LU não for 2 (ou seja, se o arquivo de save não for o temporário), desvia para 290.
C Este bloco de código é para CANCEL ou LIST de cartões no arquivo de save.

      J = 1
C Inicializa J como 1.

      IF (CARD(5).EQ.CANC1 .AND. ICONT(1).GT.0) J = ICONT(1) + 1
C Se o quinto campo for 'CANC' e ICONT(1) for maior que 0, J é definido como ICONT(1) + 1.
C ICONT(1) provavelmente armazena o número de cartões gerados.

      IF (CARD(5).EQ. LIST .AND. ICONT(1).GT.0) J = ICONT(1)
C Se o quinto campo for 'LIST' e ICONT(1) for maior que 0, J é definido como ICONT(1).

      DO 240 I = 1,J
C Loop para recuar J linhas no arquivo LU.

240   BACKSPACE LU
C Recua uma linha.

      ICONT(1) = 0
C Zera o contador de continuação.

      IF (CARD(5) .EQ. LIST) GO TO 260
C Se o quinto campo for 'LIST', desvia para 260 (apenas listar).

      READ (LU,180) SAVE
C Lê a linha do arquivo (agora recuado) para SAVE.

      J = J - 1
C Decrementa J.

      WRITE  (NOUT,250) J
C Escreve uma mensagem sobre o número de cartões cancelados.

250   FORMAT (1X,I4,' PREVIOUSLY GENERATED CARDS CANCELLED ***')
C Formato da mensagem de cancelamento.

      IF (J .GT. 0) WRITE (NOUT,255) SAVE
C Se J for maior que 0, exibe a última linha que foi cancelada.

255   FORMAT (/,' *** LAST CARD WAS:', /1X,20A4)
C Formato da mensagem da última linha cancelada.

      GO TO 280
C Pula para 280.

260   WRITE  (NOUT,265) J
C Exibe uma mensagem indicando o número de cartões a serem listados.

265   FORMAT (//,' *** PREVIOUS',I4,' CARDS WERE (COLS. 1-79) -',/)
C Formato da mensagem para listar cartões anteriores.

      DO 270 I = 1,J
C Loop para ler e listar os cartões.

      READ (LU,180,END=285) SAVE
C Lê o cartão para SAVE. Se fim de arquivo, desvia para 285.

270   WRITE  (NOUT,275) SAVE
C Escreve o cartão lido para a saída padrão.

275   FORMAT (1X,20A4)
C Formato para escrever o cartão.

280   CARD(1) = SAVE(1)
C Copia o primeiro elemento de SAVE para CARD.

      CARD(2) = SAVE(2)
C Copia o segundo elemento de SAVE para CARD.

      GO TO 170
C Retorna para o loop principal de leitura.

285   BACKSPACE LU
C Se ocorrer fim de arquivo durante a leitura para listar, recua uma linha.

      SAVE(1) = CARD(1)
C Copia o primeiro elemento de CARD para SAVE.

      SAVE(2) = CARD(2)
C Copia o segundo elemento de CARD para SAVE.

      GO TO 170
C Retorna para o loop principal de leitura.

290   WRITE  (NOUT,300) CARD4,CARD(2)
C Se a unidade LU não for 2 (não há arquivo de save), exibe uma mensagem de erro.

300   FORMAT (' *** ',A4,A3,'OPTION NOT ACTIVE.  NO SAVE FILE ',
     1         ' REQUESTED')
C Formato da mensagem de erro.

      GO TO 170
C Retorna para o loop principal de leitura.

310   WRITE  (NOUT,315) FNAME
C Exibe uma mensagem de erro se o arquivo não puder ser atribuído.

315   FORMAT (' *** CAN NOT ASSIGN FILE - ',4A8)
C Formato da mensagem de erro.

      GO TO 20
C Volta para a solicitação inicial de nome de arquivo.

320   WRITE  (NOUT,325)
C Exibe uma mensagem de erro de entrada.

325   FORMAT (' *INPUT ERROR/FF*')
C Formato da mensagem de erro de entrada.

      GO TO 170
C Retorna para o loop principal de leitura.

330   WRITE  (NOUT,335)
C Exibe uma mensagem de nome de arquivo inválido.

335   FORMAT (' *NOT A VALID FILE NAME*')
C Formato da mensagem de nome de arquivo inválido.

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C Se a máquina for CDC e a unidade de entrada for 5, rebobina a unidade de entrada.

      GO TO 20
C Volta para a solicitação inicial de nome de arquivo.

340   FFFLAG = 1234
C Define FFFLAG para 1234, ativando o modo Free-Field.

      IF (INFLAG .EQ. 0) CALL FFHELP (*170,*170,5)
C Se INFLAG for 0, chama FFHELP.
C - 5: Argumento para FFHELP.

      GO TO 170
C Retorna para o loop principal de leitura.

350   I = 3
C Inicializa I como 3 (índice para CARD1).

      IF (CARD1(5) .EQ. RPRN) I = 4
C Se CARD1(5) for ')', redefine I como 4. (Verifica a sintaxe de REPL).

      READ (CARD1(I),355,ERR=170) II
C Lê um dígito (I1) do campo CARD1(I) para II.
C Se ocorrer erro, desvia para 170.
C Este é o índice do campo a ser substituído (provavelmente).

355   FORMAT (I1)
C Formato para ler um único dígito inteiro.

      IF (II .EQ. 0) II = 10
C Se o dígito lido for 0, define II como 10. (Possivelmente para o 10º campo).

      I = I + 2
C Incrementa I por 2.

      DO 360 J = 1,8
C Loop para copiar os próximos 8 caracteres.

      SPL(J) = CARD1(I)
C Copia um caractere de CARD1 para SPL.

360   I = I + 1
C Incrementa I.

      SAVE(II) = SPILL
C Coloca o conteúdo de SPILL (que é SPL) na posição II do array SAVE.
C Isso realiza a substituição do campo.

      DO 365 I = 1,10
C Loop para copiar os 10 primeiros elementos de SAVE para CARD.

365   CARD(I) = SAVE(I)
C Copia o elemento.

      BACKSPACE LU
C Recua uma linha no arquivo LU.

      WRITE (LU,  180) SAVE
C Escreve o conteúdo atualizado de SAVE de volta para o arquivo LU.

      WRITE (NOUT,190) SAVE
C Escreve o conteúdo atualizado de SAVE para a saída padrão (eco).

      GO TO 170
C Retorna para o loop principal de leitura.

370   IF (LU .NE. 2) GO TO 385
C Se LU não for 2 (não é o arquivo de save temporário), desvia para 385.

      DO 375 J = 3,18
C Loop para verificar se há caracteres diferentes de branco nos campos 3 a 18 do CARD.

      IF (CARD(J) .NE. BLANK) GO TO 380
C Se encontrar um caractere não-branco, desvia para 380.

375   CONTINUE
C Continua o loop.

      GO TO 170
C Se todos os campos de 3 a 18 estiverem em branco, retorna para o loop principal.
C Isso pode ser para ignorar linhas completamente em branco.

380   BACKSPACE LU
C Recua uma linha no arquivo LU.

      READ (LU,180) SAVE
C Lê a linha recuada para SAVE.

      IF (SAVE(19) .EQ. BLANK) GO TO 390
C Se o campo de continuação (coluna 73-76) da linha lida estiver em branco, desvia para 390.

      WRITE  (NOUT,382)
C Exibe uma mensagem de erro se o primeiro campo estiver em branco, mas houver campo de continuação.

382   FORMAT (/,' BAD INPUT - FIRST FIELD BLANK. TRY AGAIN')
C Formato da mensagem de erro.

      WRITE  (NOUT,383)
C Exibe mensagem adicional sobre o campo de continuação.

383   FORMAT (13X,'NOT ALLOW. PREVIOUS CARD HAS CONTINUATION FIELD ',
     1         'DEFINED')
C Formato da mensagem de erro.

      GO TO 170
C Retorna para o loop principal de leitura.

385   WRITE  (NOUT,382)
C Exibe a mensagem de "BAD INPUT".

      WRITE  (NOUT,387)
C Exibe uma mensagem de erro específica para o caso sem arquivo de save.

387   FORMAT (13X,'NOT ALLOW WITHOUT SAVE FILE')
C Formato da mensagem de erro.

      GO TO 170
C Retorna para o loop principal de leitura.

390   KONTN = KONTN + 1
C Incrementa o contador de continuação.

      IF (MOD(KONTN,10000) .EQ. 0) IKI = IKI + 1
C Se KONTN for um múltiplo de 10000, incrementa o índice de símbolos de continuação.

      CALL INT2K8 (*385,KONTN,SAVE(19))
C Chama a sub-rotina INT2K8 para converter KONTN para um formato de 8 caracteres e armazená-lo em SAVE(19).
C *385: Rótulo de retorno em caso de erro.

      SAVE(19) = KSMB(IKI)
C Define o campo de continuação de SAVE com o símbolo KSMB apropriado.

      CARD(1 ) = KSMB(IKI)
C Define o primeiro campo de CARD com o símbolo KSMB.

      CARD(2 ) = SAVE(20)
C Copia o 20º campo de SAVE para o 2º campo de CARD.

      WRITE  (NOUT,395)
C Exibe uma mensagem indicando que os cartões anteriores foram substituídos.

395   FORMAT (' ...PREVIOUS CARDS REPLACED BY:')
C Formato da mensagem.

      WRITE (NOUT,190) SAVE
C Escreve o conteúdo de SAVE para a saída padrão.

      WRITE (NOUT,190) CARD
C Escreve o conteúdo de CARD para a saída padrão.

      BACKSPACE LU
C Recua uma linha no arquivo LU.

      WRITE (LU,180) SAVE
C Escreve SAVE de volta para LU.

      WRITE (LU,180) CARD
C Escreve CARD de volta para LU.

      GO TO 170
C Retorna para o loop principal de leitura.

400   BACKSPACE LOUT
C Recua uma linha no arquivo LOUT (unidade 3).

410   ENDFILE LOUT
C Escreve um marcador de fim de arquivo para LOUT.

      IF (LU .NE. 6) ENDFILE LU
C Se LU não for 6 (tela), escreve um marcador de fim de arquivo para LU.

      PUNCH =.FALSE.
C Inicializa PUNCH como falso.

      SITE  = BLNK8
C Inicializa SITE (ID do local de perfuração) como em branco.

420   WRITE  (NOUT,430) QMARK
C Pergunta ao usuário se ele deseja perfurar o deck NASTRAN.

430   FORMAT (/,' *** DO YOU WANT TO PUNCH OUT THE NASTRAN DECK',A1,
     1         ' (Y,N,X,HELP) ')
C Formato da pergunta: "Você deseja perfurar o deck NASTRAN? (Y,N,X,HELP)".

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C Se a máquina for CDC e a unidade de entrada for 5, rebobina a unidade de entrada.

      READ (IN,90,END=420) X1
C Lê a resposta do usuário. Se fim de arquivo, repete a pergunta (vai para 420).

      CALL UPCASE (X1,1)
C Converte a resposta para maiúsculas.

      IF (X1 .EQ. H1) CALL FFHELP (*420,*480,3)
C Se a resposta for 'H' (Help), chama FFHELP.
C - 3: Argumento para FFHELP.

      IF (X1 .EQ. N1) GO TO 500
C Se a resposta for 'N' (No), desvia para 500 (fim do programa).

      IF (X1.NE.Y1 .AND. X1.NE.FX) GO TO 420
C Se a resposta não for 'Y' ou 'X', repete a pergunta (vai para 420).

      PUNCH =.TRUE.
C Define PUNCH como verdadeiro.

      LX = LOUT
C Define LX (unidade de origem para perfuração) como LOUT (unidade 3).

      IF (X1 .EQ. FX) LX = LU
C Se a resposta for 'X', define LX como LU (unidade do arquivo de save).

      IF (MACH .NE. UNIVAC) GO TO 460
C Se a máquina não for UNIVAC, pula para 460.
C Este bloco é específico para UNIVAC, solicitando o ID do local de perfuração.

      IPUN = 1
C Define IPUN como 1 (unidade de perfuração para UNIVAC, pode ser diferente da unidade 7).

      WRITE  (NOUT,440)
C Pergunta o ID do local.

440   FORMAT (/,' *** ENTER SITE-ID, OR ''CENTRAL'', WHERE CARDS ARE',
     1         ' TO BE PUNCHED ')
C Formato da pergunta.

450   READ (IN,40,ERR=450,END=450) SITE
C Lê o SITE-ID. Se erro/fim de arquivo, repete a leitura.

      IF (SITE .EQ. BLNK8) GO TO 490
C Se o SITE-ID estiver em branco, desvia para 490 (não perfurar, limpar nome de arquivo).

      CALL UPCASE (SITE,8)
C Converte o SITE-ID para maiúsculas.

      IF (SITE .EQ. CNTRL) GO TO 460
C Se o SITE-ID for 'CENTRAL', pula para 460.

      SYM(3) = SITE
C Define o terceiro elemento de SYM com o SITE-ID.

      J = FACSF(SYM)
C Chama a função FACSF com SYM (possivelmente para configuração específica do site).

460   REWIND LX
C Rebobina a unidade de origem (LX).

470   READ (LX,180,END=500) CARD
C Lê uma linha do arquivo de origem (LX) para CARD. Se fim de arquivo, desvia para 500.

      DO 475 J = 2,NCNTL
C Loop para verificar se a linha lida é um comando de controle.

      IF (CARD6 .NE. CNTLWD(J)) GO TO 475
C Se CARD6 não for igual à palavra de controle atual, continua para a próxima.

      IF (CARD(4) .EQ. BLANK) GO TO 470
C Se o quarto campo estiver em branco, ignora a linha (é um comando de controle sem dados?) e lê a próxima.

475   CONTINUE
C Continua o loop.

      IF ((CARD6.EQ.HELP6 .OR. CARD6.EQ.STOP6) .AND. CARD(3).EQ.BLANK)
     1    GO TO 470
C Se CARD6 for 'HELP  ' ou 'STOP  ' e o terceiro campo estiver em branco, ignora a linha e lê a próxima.

      IF (CARD6.EQ.CNTLWD(1) .AND. CARD(4).EQ.BLANK) ICNTL = ICNTL + 1
C Se CARD6 for 'CANCEL' e o quarto campo estiver em branco, incrementa o contador de cartões cancelados.

      WRITE (IPUN,180) CARD
C Escreve o cartão na unidade de perfuração (IPUN).

      GO TO 470
C Continua lendo e perfurando os cartões.

480   FNAME(1) = BLNK8
C Se o usuário digitou 'S' (Stop) antes de decidir perfurar, o nome do arquivo é limpo.

490   PUNCH    =.FALSE.
C Define PUNCH como falso.

500   WRITE  (NOUT,510)
C Mensagem de despedida.

510   FORMAT (//10X,'ADIEU MY FRIEND.  IT IS A PLEASURE TO SERVE YOU')
C Formato da mensagem: "ADEUS MEU AMIGO. É UM PRAZER SERVIR VOCÊ".

      IF (FNAME(1) .NE. BLNK8) WRITE (NOUT,520) FNAME
C Se o nome do arquivo não estiver em branco, informa ao usuário onde o deck NASTRAN foi salvo.

520   FORMAT (10X,'DON''T FORGET - YOUR NASTRAN DECK IS IN FILE -',
     1   /25X,4A8, /10X,'WHICH IS ACCESSIBLE BY THE SYSTEM EDITOR')
C Formato da mensagem.

      IF (.NOT.PUNCH) GO TO 550
C Se PUNCH for falso, pula para 550.

      WRITE  (NOUT,530)
C Informa ao usuário para pegar os cartões perfurados.

530   FORMAT (/10X,'AND DON''T FORGET TO PICK UP YOUR PUNCHED CARDS')
C Formato da mensagem.

      IF (SITE .NE. BLNK8) WRITE (NOUT,535) SITE
C Se SITE-ID não estiver em branco, exibe o SITE-ID.

      IF (SITE.EQ.BLNK8 .AND. MACH.NE.VAX) WRITE (NOUT,540)
C Se SITE-ID estiver em branco e a máquina não for VAX, informa o local central.

      IF (SITE.EQ.BLNK8 .AND. MACH.EQ.VAX) WRITE (NOUT,545)
C Se SITE-ID estiver em branco e a máquina for VAX, informa o nome do arquivo FORTRAN.

535   FORMAT (10X,'WHEN YOU SIGN OFF',22X,'SITE-ID: ',A8)
C Formato da mensagem com SITE-ID.

540   FORMAT (10X,'AT THE CENTRAL-SITE')
C Formato da mensagem de local central.

545   FORMAT (10X,'IN FORTRAN FILE FOR007.DAT')
C Formato da mensagem de arquivo FORTRAN para VAX.

550   IF (FNAME(1) .EQ. BLNK8) GO TO 570
C Se o nome do arquivo for em branco, pula para 570.

      IF (MACH .EQ. UNIVAC) FOROO3 = TPF
C Se a máquina for UNIVAC, define FOROO3 como TPF ('FF$$.').

      IF (MACH .EQ.   CDC) FOROO3 = TAPEO3
C Se a máquina for CDC, define FOROO3 como TAPEO3 ('TAPE3.').

      WRITE  (NOUT,555)
C Informa ao usuário que uma cópia dos cartões de entrada foi salva.

555   FORMAT (//10X,'A COPY OF YOUR ACTUAL INPUT CARDS WAS SAVED IN')
C Formato da mensagem.

      IF (MACH .NE. VAX) WRITE (NOUT,560) FOROO3
C Se a máquina não for VAX, exibe o nome do arquivo FORTRAN.

      IF (MACH .EQ. VAX) WRITE (NOUT,565)
C Se a máquina for VAX, exibe o nome do arquivo FORTRAN específico para VAX.

560   FORMAT (1H+,56X,'FORTRAN FILE - ',A8)
C Formato da mensagem com o nome do arquivo FORTRAN.

565   FORMAT (10X,'FORTRAN FILE FOR003.DAT')
C Formato da mensagem de arquivo FORTRAN para VAX.

570   IF (ICNTL .NE. 0) WRITE (NOUT,575)
C Se ICNTL (contador de cartões cancelados) não for zero, exibe um aviso.

575   FORMAT (/4X,'*** WARNING - CANCELLED CARDS IN PUNCHED DECK NEED ',
     1         'TO BE REMOVED', /19X,'OR MODIFIED BEFORE USE')
C Formato da mensagem de aviso.

      IF (FNAME(1) .NE. BLNK8) DORK = KEEP
C Se o nome do arquivo não estiver em branco, define DORK (status de fechamento do arquivo) como 'KEEP'.
C Caso contrário, DORK permanece como 'DELETE' (inicializado nos dados).

      CLOSE (UNIT=LU  ,STATUS=DORK)
C Fecha a unidade LU com o status DORK (KEEP ou DELETE).

      CLOSE (UNIT=LOUT,STATUS=DORK)
C Fecha a unidade LOUT com o status DORK.

      IF (DORK.EQ.KEEP .OR. (PUNCH .AND. MACH.EQ.VAX)) WRITE (NOUT,585)
C Se os arquivos foram mantidos ou se houve perfuração em um VAX, exibe um lembrete para deletar os arquivos.

585   FORMAT (/4X,'*** DON''T FORGET TO DELETE YOUR FILES GENERATED BY',
     1         ' THIS RUN ***')
C Formato da mensagem de lembrete para deletar arquivos.

      WRITE  (NOUT,590)
C Mensagem final de "JOB DONE".

590   FORMAT (/26X,'*** JOB DONE ***',/)
C Formato da mensagem.

600   CONTINUE
C Rótulo de continuação.

      END
C Fim do programa principal.