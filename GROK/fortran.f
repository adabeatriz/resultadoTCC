PROGRAM FF
C     PROGRAMA FF: Processa entrada de dados em formato livre para NASTRAN
C     CDC PROGRAM FF (TAPE3): Comentário original indicando possível uso de TAPE3
      LOGICAL         STAR,     PCT,      NOTYET,   PUNCH,    UPFLAG
C     Declaração de variáveis lógicas para controle de estado
C     STAR: Indica presença de asterisco
C     PCT: Indica presença de porcentagem
C     NOTYET: Controle de estado "ainda não processado"
C     PUNCH: Controla se a saída será perfurada (punched cards)
C     UPFLAG: Flag para conversão de texto para maiúsculas
      INTEGER         SCREEN,   PROM,     FFFLAG,   FACSF,    NC(7),
     1                IBM,      UNIVAC,   CDC,      VAX,      PC,
     2                UNIX
C     Declaração de variáveis inteiras
C     SCREEN: Unidade de saída para tela
C     PROM: Controla prompts
C     FFFLAG: Flag para controle de entrada em formato livre
C     FACSF: Função para manipulação de arquivos (UNIVAC)
C     NC: Array com número de caracteres por máquina
C     IBM, UNIVAC, CDC, VAX, PC, UNIX: Identificadores de tipos de máquinas
      CHARACTER*1     FN(1),    BK1,      N1,       Y1,       X1,
     1                S1,       W1,       A1,       H1,       CARD1(5),
     2                FX,       MARKQ,    QMARK,    TMP,      RPRN,
     3                SPL(8),   DOT,      D1,       T1
C     Declaração de caracteres de tamanho 1
C     FN: Array para nome de arquivo (equivalência com FN4, FN6, etc.)
C     BK1: Caractere em branco
C     N1, Y1, X1, S1, W1, A1, H1: Caracteres para comandos (N, Y, X, etc.)
C     CARD1: Array para entrada de cartão
C     FX, MARKQ, QMARK, TMP, RPRN, SPL, DOT, D1, T1: Caracteres auxiliares
      CHARACTER*2     CARD2,    REPL
C     CARD2: Armazena comandos de 2 caracteres
C     REPL: String para substituição
      CHARACTER*3     NEW,      OLD,      ODNW
C     NEW, OLD: Strings para status de arquivo (novo ou existente)
C     ODNW: Define se o arquivo é novo ou velho
      CHARACTER*4     CARD(20), SAVE(20), BEGN,     HELP,     STOP,
     1                LIST,     BLANK,    END1,     END2,     END3,
     2                CANC1,    CANC2,    ALTER,    CARD4,    FN4,
     3                BBK(2),   KSMB(9)
C     Declaração de strings de 4 caracteres
C     CARD: Array para entrada de cartões
C     SAVE: Array para salvar cartões
C     BEGN, HELP, STOP, LIST, BLANK, END1, END2, END3, CANC1, CANC2, ALTER: Comandos
C     CARD4, FN4: Variáveis auxiliares
C     BBK: Parênteses para comandos
C     KSMB: Símbolos de continuação
      CHARACTER*6     FN6,      BEGN6,    HELP6,    STOP6,    CARD6,
     1                MTYPE(7), CNTLWD(3)
C     Declaração de strings de 6 caracteres
C     FN6: Nome de arquivo
C     BEGN6, HELP6, STOP6: Comandos de 6 caracteres
C     CARD6: Entrada de cartão
C     MTYPE: Tipos de máquinas
C     CNTLWD: Palavras de controle (CANCEL, PROMPT, LIST)
      CHARACTER*8     FNAME(4), MYFILE,   BLNK8,    SITE,     CNTRL,
     1                USE(2),   SYM(4),   TAPEO3,   TPF,      FOROO3,
     2                DORK,     KEEP,     SPILL
C     Declaração de strings de 8 caracteres
C     FNAME: Array para nome de arquivo
C     MYFILE, BLNK8, SITE, CNTRL, USE, SYM, TAPEO3, TPF, FOROO3, DORK, KEEP, SPILL: Strings auxiliares
      CHARACTER*11    F
C     F: String para mensagem de cabeçalho
      CHARACTER*32    FN32
C     FN32: Nome de arquivo de até 32 caracteres
      COMMON /SYSTEM/ IBUF,     NOUT,     NOGO,     IN,       ISYS(15)
C     Bloco comum SYSTEM: Variáveis de controle do sistema
C     IBUF: Buffer de entrada
C     NOUT: Unidade de saída
C     NOGO: Flag de erro
C     IN: Unidade de entrada
C     ISYS: Array de configurações do sistema
      COMMON /MACHIN/ MACH
C     Bloco comum MACHIN: Identificador da máquina
      COMMON /XXREAD/ INFLAG,   INSAVE,   LOOP4,    IBMCDC
C     Bloco comum XXREAD: Variáveis de leitura
C     INFLAG: Flag de entrada
C     INSAVE: Unidade de entrada salva
C     LOOP4: Controle de loop
C     IBMCDC: Combinação de máquinas
      COMMON /XECHOX/ FFFLAG,   IECHO(3), ISORT(5)
C     Bloco comum XECHOX: Controle de eco e ordenação
C     FFFLAG: Flag para formato livre
C     IECHO: Array para controle de eco
C     ISORT: Array para ordenação
      COMMON /XREADX/ SCREEN,   LOOP,     KOUNT,    PROM,     NOTYET,
     1                STAR,     PCT,      ICONT(36)
C     Bloco comum XREADX: Variáveis de leitura e controle
C     SCREEN: Unidade de tela
C     LOOP, KOUNT: Contadores de loop
C     PROM: Controle de prompt
C     NOTYET, STAR, PCT: Flags lógicas
C     ICONT: Array de controle de continuação
      COMMON /QMARKQ/ MARKQ,    TMP(16),  SPILL,    SAVE
C     Bloco comum QMARKQ: Variáveis para marcação e armazenamento
C     MARKQ: Caractere de interrogação
C     TMP: Array temporário
C     SPILL: String de transbordo
C     SAVE: Array para salvar cartões
      COMMON /UPCASX/ UPFLAG,   UPID(3)
C     Bloco comum UPCASX: Controle de conversão para maiúsculas
C     UPFLAG: Flag de conversão
C     UPID: Array de identificação
      EQUIVALENCE     (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),
     1                (CARD(1),CARD6,CARD1(1),CARD2,CARD4),
     2                (SPL(1),SPILL)
C     Define equivalências entre variáveis para compartilhamento de memória
      DATA            BEGN,     HELP,     STOP,     BLANK,    BK1    /
     1                'BEGI',   'HELP',   'STOP',   '    ',   ' '    /
C     Inicializa strings de 4 caracteres: comandos e espaço em branco
      DATA            BEGN6,    HELP6,    STOP6,    BLNK8,    KEEP   /
     1                'BEGIN ', 'HELP  ', 'STOP  ', '    ',   'KEEP' /
C     Inicializa strings de 6 e 8 caracteres: comandos e controle
      DATA            END1,     END2,    END3,     N1,       ALTER  /
     1                'ENDD',   'END ',   'ENDA',   'N',      'LTER' /
C     Inicializa comandos de término e caractere 'N'
      DATA            CANC1,    CANC2,    DORK,     LLI,      LLJ    /
     1                'CANC',   'EL  ',   'DELETE', 4H  I ,   4H  J  /
C     Inicializa comandos de cancelamento e identificadores
      DATA            CNTRL,    MYFILE,   NEW,      OLD,      Y1     /
     1                'CENTRAL','MIFYLE', 'NEW',    'OLD',    'Y'    /
C     Inicializa strings de controle e arquivo
      DATA            NC/ 12,   7,   28,  7,   28,  0,  28           /
C     Inicializa array com número de caracteres por máquina
      DATA            UNIVAC,   IBM, CDC, VAX, PC,      UNIX         /
     1                3,        2,   4,   5,   1,       7            /
C     Inicializa identificadores de máquinas
      DATA            MTYPE                                          /
     1                'IBM PC', '  IBM',  'UNIVAC', '  CDC',  '  VAX',
     2                ' ***  ', '  UNIX'                             /
C     Inicializa tipos de máquinas
      DATA            TAPEO3,   TPF,      FOROO3,   F                /
     1                'TAPE3.', 'FF$$.',  'FT03.',  'FF       FF'    /
C     Inicializa nomes de arquivos e mensagem de cabeçalho
      DATA            S1,       A1,       BBK,                LIST   /
     1                'S',      'A',      '(BLA', 'NK) ',     'LIST' /
C     Inicializa caracteres e comandos
      DATA            W1,       H1,       FX,       RPRN,     ICNTL  /
     1                'O',      'H',      'X',      ')',      0      /
C     Inicializa caracteres de controle e contador
      DATA            DOT,      D1,       T1                         /
     1                '.',      'D',      'T'                        /
C     Inicializa caracteres para extensão de arquivo
      DATA            CNTLWD,                       NCNTL,    REPL   /
     1                'CANCEL', 'PROMPT', 'LIST  ', 3,        'R:'   /
C     Inicializa palavras de controle e string de substituição
      DATA            KSMB  /   '+C0N',   '+C1N',   '+C2N',   '+C3N' ,
     1                '+C4N',   '+C5N',   '+C6N',   '+C7N',   '+C8N' /
C     Inicializa símbolos de continuação
      DATA      USE / '@USE 3.,', 'FF$$ .  '/       PUNCH /   .FALSE./
C     Inicializa comandos de uso e flag de perfuração
      DATA      SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '          /
C     Inicializa símbolos para UNIVAC
      DATA            QMARK / '?' /
C     Inicializa caractere de interrogação
      J = LLJ - LLI
C     Calcula diferença entre LLJ e LLI para identificar máquina
      MACH = IBM
C     Define máquina padrão como IBM
      IF (J .EQ.   256) MACH = PC
C     Se J for 256, define máquina como PC
      IF (J .EQ.   512) MACH = UNIVAC
C     Se J for 512, define máquina como UNIVAC
      IF (J .GT. 65535) MACH = VAX
C     Se J for maior que 65535, define máquina como VAX
      IF (J .GT. 2**30) MACH = CDC
C     Se J for maior que 2^30, define máquina como CDC
      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))
     1    MACH = UNIX
C     Ajusta para UNIX se VAX e XXI fora do intervalo
      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))
     1    MACH = PC
C     Ajusta para PC se VAX e XXI fora do intervalo
      LU      = 2
C     Define unidade lógica de entrada/saída como 2
      LOUT    = 3
C     Define unidade de saída como 3
      IN      = 5
C     Define unidade de entrada como 5
      NOUT    = 6
C     Define unidade de saída padrão como 6
      IPUN    = 7
C     Define unidade de perfuração como 7
      SCREEN  = 6
C     Define unidade de tela como 6
      IF (MACH .EQ. VAX) SCREEN = 5
C     Ajusta unidade de tela para 5 se máquina for VAX
      LOOP    =-1
C     Inicializa contador de loop como -1
      KOUNT   = 0
C     Inicializa contador como 0
      KONTN   = 10000000
C     Inicializa contador de continuação
      IKI     = 1
C     Inicializa índice de continuação
      PROM    =+1
C     Ativa prompt
      STAR    =.FALSE.
C     Desativa flag de asterisco
      PCT     =.FALSE.
C     Desativa flag de porcentagem
      NOTYET  =.FALSE.
C     Desativa flag de "ainda não processado"
      MARKQ   = QMARK
C     Define marcador como interrogação
      IECHO(2)=-2
C     Define eco para -2
      INSAVE  = IN
C     Salva unidade de entrada
      INFLAG  = 0
C     Inicializa flag de entrada
      FFFLAG  = 0
C     Inicializa flag de formato livre
      UPFLAG  =.FALSE.
C     Desativa flag de conversão para maiúsculas
      ISYS(15)= 0
C     Inicializa array de sistema
      J       = NC(MACH)
C     Define J com base no número de caracteres da máquina
      CARD4   = BLANK
C     Inicializa CARD4 como vazio
      CARD6   = STOP6
C     Inicializa CARD6 como STOP
      FN4     = STOP
C     Inicializa FN4 como STOP
      FN6     = STOP6
C     Inicializa FN6 como STOP
      LOOP4   = LOOP - 4
C     Ajusta contador de loop
      DO 5 I = 1,20
C     Loop para inicializar array SAVE
 5    SAVE(I) = BLANK
C     Define todos os elementos de SAVE como vazio
      IBMCDC = UNIVAC + VAX + PC
C     Calcula combinação de máquinas
      IF (MACH.EQ.IBM .OR. MACH.EQ.CDC) IBMCDC = 0
C     Zera IBMCDC se máquina for IBM ou CDC
      IF (MACH .NE. CDC) GO TO 20
C     Pula para 20 se máquina não for CDC
      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')
C     Abre arquivo de entrada INPUT
      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')
C     Abre arquivo de saída OUTPUT
      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')
C     Abre arquivo de perfuração PUNCH
      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)
C     Abre arquivo TAPE3, trata erro em 15
      GO TO 20
 15   STOP 'SCRATCH FILE ERROR, UNIT 3'
C     Para o programa se houver erro ao abrir TAPE3
 20   WRITE  (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)
C     Escreve cabeçalho com tipo de máquina
 25   FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),
     1       2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,
     2       /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,
     3       'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',
     4       '  30602')
C     Formato do cabeçalho
 30   WRITE  (NOUT,35) J
C     Solicita nome do arquivo ou comando
 35   FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO'
     1,       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')
C     Formato da solicitação
      READ (IN,40,ERR=330,END=330) FNAME
C     Lê nome do arquivo
 40   FORMAT (4A8)
C     Formato de leitura de 4 strings de 8 caracteres
      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4)
C     Chama ajuda se FNAME for MYFILE
      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE
C     Define FNAME como MYFILE se vazio
      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1)
     1   GO TO 330
C     Verifica se nome do arquivo é válido
      IF (FNAME(2) .NE. BLNK8) GO TO 50
C     Pula para 50 se FNAME(2) não for vazio
      CALL UPCASE (FNAME,J)
C     Converte nome do arquivo para maiúsculas
      IF (FN6 .EQ. BEGN6) GO TO 330
C     Erro se FN6 for BEGIN
      IF (FN6 .EQ. STOP6) GO TO 600
C     Termina se FN6 for STOP
      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1)
C     Chama ajuda se FN6 for HELP
 50   ODNW = OLD
C     Define arquivo como existente
      IF (MACH .NE. VAX) GO TO 60
C     Pula para 60 se máquina não for VAX
      DO 52 I = 2,J
C     Loop para adicionar extensão ao nome do arquivo
      IF (FN(I) .EQ. DOT) GO TO 60
C     Pula se já houver ponto
      IF (FN(I) .EQ. BK1) GO TO 55
C     Pula se encontrar espaço
 52   CONTINUE
      I = J + 1
 55   FN(I  ) = DOT
C     Adiciona ponto
      FN(I+1) = D1
C     Adiciona 'D'
      FN(I+2) = A1
C     Adiciona 'A'
      FN(I+3) = T1
C     Adiciona 'T'
 60   IF (MACH .NE. IBM) GO TO 65
C     Pula para 65 se máquina não for IBM
      I = IQZDDN(FNAME(1))
C     Obtém informações do arquivo (IBM)
      ODNW = OLD
C     Define como arquivo existente
      IF (I .EQ. 0) ODNW = NEW
C     Define como novo se não encontrado
      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)
C     Cria buffer para novo arquivo
      CALL QQGETF (LU,FNAME(1),IERR)
C     Obtém arquivo
      IF (IERR .NE. 0) GO TO 130
C     Trata erro de arquivo
 65   IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130)
C     Abre arquivo para máquinas IBM ou CDC
      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32    ,STATUS=ODNW,ERR=130)
C     Abre arquivo para outras máquinas
      IF (ODNW .EQ. NEW) GO TO 140
C     Pula para 140 se arquivo for novo
 70   WRITE  (NOUT,80)
C     Avisa que arquivo já existe
 80   FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',',
     1        ' OR ''APPEND'' -')
C     Formato da mensagem
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C     Rebobina entrada para CDC
      READ (IN,90,END=70) X1
C     Lê comando do usuário
 90   FORMAT (A1)
C     Formato de leitura de caractere
      CALL UPCASE (X1,1)
C     Converte comando para maiúsculas
      IF (X1 .EQ. S1) GO TO 480
C     Para se comando for 'S' (STOP)
      IF (X1 .EQ. W1) GO TO 140
C     Sobrescreve se comando for 'O' (OVERWRITE)
      IF (X1 .NE. A1) GO TO 70
C     Repete se comando não for 'A' (APPEND)
      SAVE(2) = BBK(1)
C     Define parêntese de abertura
      SAVE(3) = BBK(2)
C     Define parêntese de fechamento
 110  READ (LU,180,END=115) SAVE
C     Lê cartões do arquivo
      IF (SAVE(1).EQ.BEGN .AND. SAVE(4).EQ.BLANK) FFFLAG = 1234
C     Define flag de formato livre se comando for BEGIN
      IF (SAVE(19) .EQ. KSMB(IKI)) IKI = IKI + 1
C     Incrementa índice de continuação
      GO TO 110
C     Continua leitura
 115  BACKSPACE LU
C     Volta uma posição no arquivo
      IF (FFFLAG .EQ. 1234) WRITE (NOUT,120)
C     Avisa se arquivo contém entrada em formato livre
 120  FORMAT (/,' IF EXISTING FILE CONTAINS FREE-FIELD INPUT CARDS, ',
     1        ' THIS PROGRAM WILL NOT', /5X,'EXPAND THEM TO FIXED-',
     2        ' FIELD FORMATS',/)
C     Formato da mensagem
      WRITE (NOUT,255) SAVE
C     Exibe último cartão lido
      IF (FFFLAG.EQ.1234 .AND. INFLAG.EQ.0) CALL FFHELP (*125,*125,5)
C     Chama ajuda para formato livre
 125  CARD(1) = SAVE(1)
C     Copia primeiro cartão
      CARD(2) = SAVE(2)
C     Copia segundo cartão
      GO TO 140
 130  IF (ODNW .EQ. NEW) GO TO 310
C     Trata erro se arquivo for novo
      ODNW = NEW
C     Define como novo
      GO TO 60
C     Tenta abrir novamente
 140  IF (MACH .EQ. UNIVAC) J = FACSF(USE)
C     Configura arquivo para UNIVAC
      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8
C     Define nome como vazio se MYFILE
      IF (FNAME(1) .EQ. BLNK8) WRITE (NOUT,150)
C     Avisa que saída não será salva
 150  FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)
C     Formato da mensagem
      WRITE  (NOUT,160)
C     Exibe mensagem inicial do programa
 160  FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',
     1 /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2 /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',
     3 //5X,'PLEASE BEGIN -',/)
C     Formato da mensagem
 170  CALL FFREAD (*320,CARD)
C     Lê entrada em formato livre
      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230
C     Cancela cartões se comando for CANCEL
      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230
C     Lista cartões se comando for LIST
      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350
C     Substitui cartões se comando for R:
      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400
C     Para se comando for STOP
      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370
C     Trata entrada vazia
      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)
     1   CALL FFHELP (*280,*400,2)
C     Chama ajuda se comando for HELP
      IF (LU .EQ. 2) WRITE (LU,180) CARD
C     Escreve cartão no arquivo
 180  FORMAT (20A4)
C     Formato de escrita de 20 strings de 4 caracteres
      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD
C     Exibe cartão se não for formato livre
 190  FORMAT (1X,20A4)
C     Formato de exibição
      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340
C     Define formato livre se comando for BEGIN
      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)
     1   GO TO 170
C     Continua leitura se não for comando de término
      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170
C     Ignora se comando for ALTER
      GO TO 410
C     Finaliza se comando de término
 230  IF (LU .NE. 2) GO TO 290
C     Verifica se há arquivo de saída
      J = 1
C     Inicializa contador
      IF (CARD(5).EQ.CANC1 .AND. ICONT(1).GT.0) J = ICONT(1) + 1
C     Ajusta contador para CANCEL
      IF (CARD(5).EQ. LIST .AND. ICONT(1).GT.0) J = ICONT(1)
C     Ajusta contador para LIST
      DO 240 I = 1,J
C     Loop para retroceder no arquivo
 240  BACKSPACE LU
C     Retrocede no arquivo
      ICONT(1) = 0
C     Zera contador de continuação
      IF (CARD(5) .EQ. LIST) GO TO 260
C     Lista cartões se comando for LIST
      READ (LU,180) SAVE
C     Lê último cartão
      J = J - 1
C     Decrementa contador
      WRITE  (NOUT,250) J
C     Exibe número de cartões cancelados
 250  FORMAT (1X,I4,' PREVIOUSLY GENERATED CARDS CANCELLED ***')
C     Formato da mensagem
      IF (J .GT. 0) WRITE (NOUT,255) SAVE
C     Exibe último cartão
 255  FORMAT (/,' *** LAST CARD WAS:', /1X,20A4)
C     Formato de exibição
      GO TO 280
 260  WRITE  (NOUT,265) J
C     Exibe número de cartões listados
 265  FORMAT (//,' *** PREVIOUS',I4,' CARDS WERE (COLS. 1-79) -',/)
C     Formato da mensagem
      DO 270 I = 1,J
C     Loop para listar cartões
      READ (LU,180,END=285) SAVE
C     Lê cartão
 270  WRITE  (NOUT,275) SAVE
C     Exibe cartão
 275  FORMAT (1X,20A4)
C     Formato de exibição
 280  CARD(1) = SAVE(1)
C     Copia primeiro cartão
      CARD(2) = SAVE(2)
C     Copia segundo cartão
      GO TO 170
C     Volta para leitura
 285  BACKSPACE LU
C     Retrocede no arquivo
      SAVE(1) = CARD(1)
C     Salva primeiro cartão
      SAVE(2) = CARD(2)
C     Salva segundo cartão
      GO TO 170
C     Volta para leitura
 290  WRITE  (NOUT,300) CARD4,CARD(2)
C     Avisa que opção não está ativa
 300  FORMAT (' *** ',A4,A3,'OPTION NOT ACTIVE.  NO SAVE FILE ',
     1        'REQUESTED')
C     Formato da mensagem
      GO TO 170
C     Volta para leitura
 310  WRITE  (NOUT,315) FNAME
C     Avisa que não pode atribuir arquivo
 315  FORMAT (' *** CAN NOT ASSIGN FILE - ',4A8)
C     Formato da mensagem
      GO TO 20
C     Volta para solicitação de arquivo
 320  WRITE  (NOUT,325)
C     Avisa erro de entrada
 325  FORMAT (' *INPUT ERROR/FF*')
C     Formato da mensagem
      GO TO 170
C     Volta para leitura
 330  WRITE  (NOUT,335)
C     Avisa que nome do arquivo é inválido
 335  FORMAT (' *NOT A VALID FILE NAME*')
C     Formato da mensagem
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C     Rebobina entrada para CDC
      GO TO 20
C     Volta para solicitação de arquivo
 340  FFFLAG = 1234
C     Ativa formato livre
      IF (INFLAG .EQ. 0) CALL FFHELP (*170,*170,5)
C     Chama ajuda para formato livre
      GO TO 170
C     Volta para leitura
 350  I = 3
C     Define posição inicial
      IF (CARD1(5) .EQ. RPRN) I = 4
C     Ajusta posição se houver parêntese
      READ (CARD1(I),355,ERR=170) II
C     Lê número do cartão
 355  FORMAT (I1)
C     Formato de leitura
      IF (II .EQ. 0) II = 10
C     Define 10 se número for 0
      I = I + 2
C     Avança posição
      DO 360 J = 1,8
C     Loop para copiar caracteres
      SPL(J) = CARD1(I)
C     Copia caractere
 360  I = I + 1
C     Avança posição
      SAVE(II) = SPILL
C     Salva string de transbordo
      DO 365 I = 1,10
C     Loop para copiar cartões
 365  CARD(I) = SAVE(I)
C     Copia cartão
      BACKSPACE LU
C     Retrocede no arquivo
      WRITE (LU,  180) SAVE
C     Escreve cartão salvo
      WRITE (NOUT,190) SAVE
C     Exibe cartão
      GO TO 170
C     Volta para leitura
 370  IF (LU .NE. 2) GO TO 385
C     Verifica se há arquivo de saída
      DO 375 J = 3,18
C     Loop para verificar campos
      IF (CARD(J) .NE. BLANK) GO TO 380
C     Pula se campo não for vazio
 375  CONTINUE
      GO TO 170
C     Volta para leitura
 380  BACKSPACE LU
C     Retrocede no arquivo
      READ (LU,180) SAVE
C     Lê último cartão
      IF (SAVE(19) .EQ. BLANK) GO TO 390
C     Verifica campo de continuação
      WRITE  (NOUT,382)
C     Avisa erro de entrada
 382  FORMAT (/,' BAD INPUT - FIRST FIELD BLANK. TRY AGAIN')
C     Formato da mensagem
      WRITE  (NOUT,383)
C     Explica erro
 383  FORMAT (13X,'NOT ALLOW. PREVIOUS CARD HAS CONTINUATION FIELD ',
     1        'DEFINED')
C     Formato da mensagem
      GO TO 170
C     Volta para leitura
 385  WRITE  (NOUT,382)
C     Avisa erro de entrada
      WRITE  (NOUT,387)
C     Explica erro
 387  FORMAT (13X,'NOT ALLOW WITHOUT SAVE FILE')
C     Formato da mensagem
      GO TO 170
C     Volta para leitura
 390  KONTN = KONTN + 1
C     Incrementa o contador de continuação para rastrear cartões
      IF (MOD(KONTN,10000) .EQ. 0) IKI = IKI + 1
C     Incrementa o índice de continuação (IKI) a cada 10.000 cartões
      CALL INT2K8 (*385,KONTN,SAVE(19))
C     Converte o contador KONTN para uma string de 8 caracteres e armazena em SAVE(19)
      SAVE(19) = KSMB(IKI)
C     Atribui o símbolo de continuação correspondente ao índice IKI
      CARD(1 ) = KSMB(IKI)
C     Define o primeiro campo do cartão como o símbolo de continuação
      CARD(2 ) = SAVE(20)
C     Copia o último campo salvo para o segundo campo do cartão
      WRITE (NOUT,395)
C     Escreve mensagem indicando que cartões anteriores foram substituídos
 395  FORMAT (' ...PREVIOUS CARDS REPLACED BY:')
C     Formato da mensagem de substituição
      WRITE (NOUT,190) SAVE
C     Exibe o cartão salvo
      WRITE (NOUT,190) CARD
C     Exibe o cartão atualizado
      BACKSPACE LU
C     Retrocede uma posição no arquivo de saída
      WRITE (LU,180) SAVE
C     Escreve o cartão salvo no arquivo
      WRITE (LU,180) CARD
C     Escreve o cartão atualizado no arquivo
      GO TO 170
C     Volta para a leitura do próximo cartão
 400  BACKSPACE LOUT
C     Retrocede uma posição no arquivo de saída temporário
 410  ENDFILE LOUT
C     Finaliza o arquivo de saída temporário
      IF (LU .NE. 6) ENDFILE LU
C     Finaliza o arquivo de saída principal se não for a unidade 6
      PUNCH =.FALSE.
C     Desativa a flag de perfuração
      SITE  = BLNK8
C     Define o identificador do site como vazio
 420  WRITE  (NOUT,430) QMARK
C     Solicita se o usuário deseja perfurar o baralho NASTRAN
 430  FORMAT (/,' *** DO YOU WANT TO PUNCH OUT THE NASTRAN DECK',A1,
     1       ' (Y,N,X,HELP) ')
C     Formato da mensagem de solicitação
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
C     Rebobina a unidade de entrada para CDC se necessário
      READ (IN,90,END=420) X1
C     Lê a resposta do usuário (Y, N, X ou H)
 90   FORMAT (A1)
C     Formato de leitura de um caractere
      CALL UPCASE (X1,1)
C     Converte a resposta para maiúsculas
      IF (X1 .EQ. H1) CALL FFHELP (*420,*480,3)
C     Chama a função de ajuda se a resposta for 'H' (HELP)
      IF (X1 .EQ. N1) GO TO 500
C     Pula para o término se a resposta for 'N' (não perfurar)
      IF (X1.NE.Y1 .AND. X1.NE.FX) GO TO 420
C     Repete a solicitação se a resposta não for 'Y' ou 'X'
      PUNCH =.TRUE.
C     Ativa a flag de perfuração
      LX = LOUT
C     Define a unidade de leitura como o arquivo de saída temporário
      IF (X1 .EQ. FX) LX = LU
C     Usa o arquivo principal se a resposta for 'X'
      IF (MACH .NE. UNIVAC) GO TO 460
C     Pula para 460 se a máquina não for UNIVAC
      IPUN = 1
C     Define a unidade de perfuração como 1 para UNIVAC
      WRITE  (NOUT,440)
C     Solicita o identificador do site para perfuração
 440  FORMAT (/,' *** ENTER SITE-ID, OR ''CENTRAL'', WHERE CARDS ARE',
     1       ' TO BE PUNCHED ')
C     Formato da mensagem de solicitação do site
 450  READ (IN,40,ERR=450,END=450) SITE
C     Lê o identificador do site
 40   FORMAT (4A8)
C     Formato de leitura de 4 strings de 8 caracteres
      IF (SITE .EQ. BLNK8) GO TO 490
C     Pula para 490 se o site for vazio
      CALL UPCASE (SITE,8)
C     Converte o identificador do site para maiúsculas
      IF (SITE .EQ. CNTRL) GO TO 460
C     Pula para 460 se o site for 'CENTRAL'
      SYM(3) = SITE
C     Define o identificador do site no array de símbolos
      J = FACSF(SYM)
C     Configura o arquivo para perfuração no UNIVAC
 460  REWIND LX
C     Rebobina a unidade de leitura
 470  READ (LX,180,END=500) CARD
C     Lê um cartão do arquivo
 180  FORMAT (20A4)
C     Formato de leitura de 20 strings de 4 caracteres
      DO 475 J = 2,NCNTL
C     Loop para verificar palavras de controle
      IF (CARD6 .NE. CNTLWD(J)) GO TO 475
C     Pula se o cartão não for uma palavra de controle
      IF (CARD(4) .EQ. BLANK) GO TO 470
C     Ignora cartões com campo 4 vazio
 475  CONTINUE
      IF ((CARD6.EQ.HELP6 .OR. CARD6.EQ.STOP6) .AND. CARD(3).EQ.BLANK)
     1   GO TO 470
C     Ignora cartões HELP ou STOP com campo 3 vazio
      IF (CARD6.EQ.CNTLWD(1) .AND. CARD(4).EQ.BLANK) ICNTL = ICNTL + 1
C     Incrementa contador de cartões cancelados
      WRITE (IPUN,180) CARD
C     Escreve o cartão na unidade de perfuração
      GO TO 470
C     Continua leitura do próximo cartão
 480  FNAME(1) = BLNK8
C     Define o nome do arquivo como vazio
 490  PUNCH    =.FALSE.
C     Desativa a flag de perfuração
 500  WRITE  (NOUT,510)
C     Exibe mensagem de despedida
 510  FORMAT (//10X,'ADIEU MY FRIEND.  IT IS A PLEASURE TO SERVE YOU')
C     Formato da mensagem de despedida
      IF (FNAME(1) .NE. BLNK8) WRITE (NOUT,520) FNAME
C     Informa o nome do arquivo de saída, se existir
 520  FORMAT (10X,'DON''T FORGET - YOUR NASTRAN DECK IS IN FILE -',
     1    /25X,4A8, /10X,'WHICH IS ACCESSIBLE BY THE SYSTEM EDITOR')
C     Formato da mensagem do arquivo
      IF (.NOT.PUNCH) GO TO 550
C     Pula para 550 se não houver perfuração
      WRITE  (NOUT,530)
C     Avisa para coletar cartões perfurados
 530  FORMAT (/10X,'AND DON''T FORGET TO PICK UP YOUR PUNCHED CARDS')
C     Formato da mensagem
      IF (SITE .NE. BLNK8) WRITE (NOUT,535) SITE
C     Informa o site de perfuração, se especificado
      IF (SITE.EQ.BLNK8 .AND. MACH.NE.VAX) WRITE (NOUT,540)
C     Informa site central para máquinas não VAX
      IF (SITE.EQ.BLNK8 .AND. MACH.EQ.VAX) WRITE (NOUT,545)
C     Informa arquivo Fortran para VAX
 535  FORMAT (10X,'WHEN YOU SIGN OFF',22X,'SITE-ID: ',A8)
C     Formato da mensagem do site
 540  FORMAT (10X,'AT THE CENTRAL-SITE')
C     Formato da mensagem do site central
 545  FORMAT (10X,'IN FORTRAN FILE FOR007.DAT')
C     Formato da mensagem do arquivo VAX
 550  IF (FNAME(1) .EQ. BLNK8) GO TO 570
C     Pula para 570 se não houver arquivo de saída
      IF (MACH .EQ. UNIVAC) FOROO3 = TPF
C     Define nome do arquivo para UNIVAC
      IF (MACH .EQ.    CDC) FOROO3 = TAPEO3
C     Define nome do arquivo para CDC
      WRITE  (NOUT,555)
C     Informa que uma cópia dos cartões foi salva
 555  FORMAT (//10X,'A COPY OF YOUR ACTUAL INPUT CARDS WAS SAVED IN')
C     Formato da mensagem
      IF (MACH .NE. VAX) WRITE (NOUT,560) FOROO3
C     Informa o nome do arquivo para máquinas não VAX
      IF (MACH .EQ. VAX) WRITE (NOUT,565)
C     Informa o nome do arquivo para VAX
 560  FORMAT (1H+,56X,'FORTRAN FILE - ',A8)
C     Formato da mensagem do arquivo
 565  FORMAT (10X,'FORTRAN FILE FOR003.DAT')
C     Formato da mensagem do arquivo VAX
 570  IF (ICNTL .NE. 0) WRITE (NOUT,575)
C     Avisa se houver cartões cancelados no baralho
 575  FORMAT (/4X,'*** WARNING - CANCELLED CARDS IN PUNCHED DECK NEED ',
     1       'TO BE REMOVED', /19X,'OR MODIFIED BEFORE USE')
C     Formato da mensagem de aviso
      IF (FNAME(1) .NE. BLNK8) DORK = KEEP
C     Define para manter o arquivo se ele existir
      CLOSE (UNIT=LU  ,STATUS=DORK)
C     Fecha o arquivo principal com status (KEEP ou DELETE)
      CLOSE (UNIT=LOUT,STATUS=DORK)
C     Fecha o arquivo temporário com status
      IF (DORK.EQ.KEEP .OR. (PUNCH .AND. MACH.EQ.VAX)) WRITE (NOUT,585)
C     Avisa para deletar arquivos gerados
 585  FORMAT (/4X,'*** DON''T FORGET TO DELETE YOUR FILES GENERATED BY',
     1       ' THIS RUN ***')
C     Formato da mensagem
      WRITE  (NOUT,590)
C     Exibe mensagem de conclusão do trabalho
 590  FORMAT (/26X,'*** JOB DONE ***',/)
C     Formato da mensagem de conclusão
 600  CONTINUE
C     Ponto de término do programa
      END
C     Fim do programa
