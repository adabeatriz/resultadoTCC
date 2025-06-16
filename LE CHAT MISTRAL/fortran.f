      PROGRAM FF
      ! PROGRAM FF (TAPE3)
      ! Programa principal para processamento de entrada de dados no formato Free-Field para NASTRAN

      LOGICAL         STAR,     PCT,      NOTYET,   PUNCH,    UPFLAG
      ! Variáveis lógicas para controle de fluxo e flags

      INTEGER         SCREEN,   PROM,     FFFLAG,   FACSF,    NC(7),
     1                IBM,      UNIVAC,   CDC,      VAX,      PC,
     2                UNIX
      ! Variáveis inteiras para controle de sistema e identificação de máquina

      CHARACTER*1     FN(1),    BK1,      N1,       Y1,       X1,
     1                S1,       W1,       A1,       H1,       CARD1(5),
     2                FX,       MARKQ,    QMARK,    TMP,      RPRN,
     3                SPL(8),   DOT,      D1,       T1
      ! Variáveis de caracteres para manipulação de strings e caracteres individuais

      CHARACTER*2     CARD2,    REPL
      ! Variáveis de caracteres para manipulação de strings de 2 caracteres

      CHARACTER*3     NEW,      OLD,      ODNW
      ! Variáveis de caracteres para manipulação de strings de 3 caracteres

      CHARACTER*4     CARD(20), SAVE(20), BEGN,     HELP,     STOP,
     1                LIST,     BLANK,    END1,     END2,     END3,
     2                CANC1,    CANC2,    ALTER,    CARD4,    FN4,
     3                BBK(2),   KSMB(9)
      ! Variáveis de caracteres para manipulação de strings de 4 caracteres

      CHARACTER*6     FN6,      BEGN6,    HELP6,    STOP6,    CARD6,
     1                MTYPE(7), CNTLWD(3)
      ! Variáveis de caracteres para manipulação de strings de 6 caracteres

      CHARACTER*8     FNAME(4), MYFILE,   BLNK8,    SITE,     CNTRL,
     1                USE(2),   SYM(4),   TAPEO3,   TPF,      FOROO3,
     2                DORK,     KEEP,     SPILL
      ! Variáveis de caracteres para manipulação de strings de 8 caracteres

      CHARACTER*11    F
      ! Variável de caracteres para manipulação de strings de 11 caracteres

      CHARACTER*32    FN32
      ! Variável de caracteres para manipulação de strings de 32 caracteres

      COMMON /SYSTEM/ IBUF,     NOUT,     NOGO,     IN,       ISYS(15)
      ! Bloco comum para variáveis de sistema

      COMMON /MACHIN/ MACH
      ! Bloco comum para identificação de máquina

      COMMON /XXREAD/ INFLAG,   INSAVE,   LOOP4,    IBMCDC
      ! Bloco comum para controle de leitura

      COMMON /XECHOX/ FFFLAG,   IECHO(3), ISORT(5)
      ! Bloco comum para controle de eco e ordenação

      COMMON /XREADX/ SCREEN,   LOOP,     KOUNT,    PROM,     NOTYET,
     1                STAR,     PCT,      ICONT(36)
      ! Bloco comum para controle de leitura e processamento

      COMMON /QMARKQ/ MARKQ,    TMP(16),  SPILL,    SAVE
      ! Bloco comum para manipulação de strings e temporários

      COMMON /UPCASX/ UPFLAG,   UPID(3)
      ! Bloco comum para controle de maiúsculas

      EQUIVALENCE     (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),
     1                (CARD(1),CARD6,CARD1(1),CARD2,CARD4),
     2                (SPL(1),SPILL)
      ! Equivalência de variáveis para manipulação de strings

      DATA            BEGN,     HELP,     STOP,     BLANK,    BK1
     1                /'BEGI',   'HELP',   'STOP',   '    ',   ' '    /
      ! Inicialização de variáveis de caracteres

      DATA            BEGN6,    HELP6,    STOP6,    BLNK8,    KEEP
     1                /'BEGIN ', 'HELP  ', 'STOP  ', '    ',   'KEEP' /
      ! Inicialização de variáveis de caracteres

      DATA            END1,     END2,     END3,     N1,       ALTER
     1                /'ENDD',   'END ',   'ENDA',   'N',      'LTER' /
      ! Inicialização de variáveis de caracteres

      DATA            CANC1,    CANC2,    DORK,     LLI,      LLJ
     1                /'CANC',   'EL  ',   'DELETE', 4H  I ,   4H  J  /
      ! Inicialização de variáveis de caracteres

      DATA            CNTRL,    MYFILE,   NEW,      OLD,      Y1
     1                /'CENTRAL','MIFYLE', 'NEW',    'OLD',    'Y'    /
      ! Inicialização de variáveis de caracteres

      DATA            NC/ 12,   7,   28,  7,   28,  0,  28           /
      ! Inicialização de variáveis inteiras

      DATA            UNIVAC,   IBM, CDC, VAX, PC,      UNIX
     1                / 3,        2,   4,   5,   1,       7            /
      ! Inicialização de variáveis inteiras para identificação de máquina

      DATA            MTYPE
     1                /'IBM PC', '  IBM',  'UNIVAC', '  CDC',  '  VAX',
     2                ' ***  ', '  UNIX'                             /
      ! Inicialização de variáveis de caracteres para tipos de máquina

      DATA            TAPEO3,   TPF,      FOROO3,   F
     1                /'TAPE3.', 'FF$$.',  'FT03.',  'FF       FF'    /
      ! Inicialização de variáveis de caracteres

      DATA            S1,       A1,       BBK,                LIST
     1                /'S',      'A',      '(BLA', 'NK) ',     'LIST' /
      ! Inicialização de variáveis de caracteres

      DATA            W1,       H1,       FX,       RPRN,     ICNTL
     1                /'O',      'H',      'X',      ')',      0      /
      ! Inicialização de variáveis de caracteres

      DATA            DOT,      D1,       T1
     1                /'.',      'D',      'T'                        /
      ! Inicialização de variáveis de caracteres

      DATA            CNTLWD,                       NCNTL,    REPL
     1                /'CANCEL', 'PROMPT', 'LIST  ', 3,        'R:'   /
      ! Inicialização de variáveis de caracteres

      DATA            KSMB
     1                /'+C0N',   '+C1N',   '+C2N',   '+C3N' ,
     1                '+C4N',   '+C5N',   '+C6N',   '+C7N',   '+C8N' /
      ! Inicialização de variáveis de caracteres

      DATA      USE / '@USE 3.,', 'FF$$ .  '/       PUNCH /   .FALSE./
      ! Inicialização de variáveis de caracteres e lógicas

      DATA      SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '          /
      ! Inicialização de variáveis de caracteres

      DATA            QMARK / '?' /
      ! Inicialização de variáveis de caracteres

      J = LLJ - LLI
      ! Cálculo da diferença entre LLJ e LLI

      MACH = IBM
      ! Definição inicial da máquina como IBM

      IF (J .EQ.   256) MACH = PC
      ! Verificação se a máquina é PC

      IF (J .EQ.   512) MACH = UNIVAC
      ! Verificação se a máquina é UNIVAC

      IF (J .GT. 65535) MACH = VAX
      ! Verificação se a máquina é VAX

      IF (J .GT. 2**30) MACH = CDC
      ! Verificação se a máquina é CDC

      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))
     1    MACH = UNIX
      ! Verificação se a máquina é UNIX

      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))
     1    MACH = PC
      ! Verificação se a máquina é PC

      LU      = 2
      LOUT    = 3
      IN      = 5
      NOUT    = 6
      IPUN    = 7
      SCREEN  = 6
      ! Definição de unidades de entrada e saída

      IF (MACH .EQ. VAX) SCREEN = 5
      ! Ajuste da unidade de tela para VAX

      LOOP    =-1
      KOUNT   = 0
      KONTN   = 10000000
      IKI     = 1
      PROM    =+1
      STAR    =.FALSE.
      PCT     =.FALSE.
      NOTYET  =.FALSE.
      MARKQ   = QMARK
      IECHO(2)=-2
      INSAVE  = IN
      INFLAG  = 0
      FFFLAG  = 0
      UPFLAG  =.FALSE.
      ISYS(15)= 0
      ! Inicialização de variáveis de controle

      J       = NC(MACH)
      CARD4   = BLANK
      CARD6   = STOP6
      FN4     = STOP
      FN6     = STOP6
      LOOP4   = LOOP - 4
      ! Inicialização de variáveis de caracteres e controle

      DO 5 I = 1,20
 5    SAVE(I) = BLANK
      ! Inicialização do array SAVE com espaços em branco

      IBMCDC = UNIVAC + VAX + PC
      IF (MACH.EQ.IBM .OR. MACH.EQ.CDC) IBMCDC = 0
      ! Cálculo de IBMCDC

      IF (MACH .NE. CDC) GO TO 20
      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')
      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')
      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')
      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)
      GO TO 20
      ! Abertura de arquivos para CDC

 15   STOP 'SCRATCH FILE ERROR, UNIT 3'
      ! Mensagem de erro para falha na abertura de arquivo

 20   WRITE  (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)
      ! Escrita do cabeçalho do programa

 25   FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),
     1       2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,
     2       /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,
     3       'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',
     4       '  30602')
      ! Formato do cabeçalho do programa

 30   WRITE  (NOUT,35) J
      ! Solicitação do nome do arquivo

 35   FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO',
     1,       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')
      ! Formato da solicitação do nome do arquivo

      READ (IN,40,ERR=330,END=330) FNAME
      ! Leitura do nome do arquivo

 40   FORMAT (4A8)
      ! Formato da leitura do nome do arquivo

      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4)
      ! Chamada de ajuda se o nome do arquivo for MYFILE

      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE
      ! Definição do nome do arquivo como MYFILE se estiver em branco

      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1)
     1   GO TO 330
      ! Verificação de espaços em branco no nome do arquivo

      IF (FNAME(2) .NE. BLNK8) GO TO 50
      CALL UPCASE (FNAME,J)
      ! Conversão do nome do arquivo para maiúsculas

      IF (FN6 .EQ. BEGN6) GO TO 330
      IF (FN6 .EQ. STOP6) GO TO 600
      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1)
      ! Verificação de comandos especiais

 50   ODNW = OLD
      IF (MACH .NE. VAX) GO TO 60
      DO 52 I = 2,J
      IF (FN(I) .EQ. DOT) GO TO 60
      IF (FN(I) .EQ. BK1) GO TO 55
 52   CONTINUE
      I = J + 1
 55   FN(I  ) = DOT
      FN(I+1) = D1
      FN(I+2) = A1
      FN(I+3) = T1
      ! Adição de extensão ao nome do arquivo para VAX

 60   IF (MACH .NE. IBM) GO TO 65
      I = IQZDDN(FNAME(1))
      ODNW = OLD
      IF (I .EQ. 0) ODNW = NEW
      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)
      CALL QQGETF (LU,FNAME(1),IERR)
      IF (IERR .NE. 0) GO TO 130
      ! Verificação e abertura de arquivo para IBM

 65   IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130)
      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32    ,STATUS=ODNW,ERR=130)
      ! Abertura de arquivo para outras máquinas

      IF (ODNW .EQ. NEW) GO TO 140
      ! Verificação se o arquivo é novo

 70   WRITE  (NOUT,80)
      ! Solicitação de ação para arquivo existente

 80   FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',',
     1        ' OR ''APPEND'' -')
      ! Formato da solicitação de ação para arquivo existente

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      ! Rebobinamento da entrada para CDC

      READ (IN,90,END=70) X1
      ! Leitura da ação para arquivo existente

 90   FORMAT (A1)
      ! Formato da leitura da ação para arquivo existente

      CALL UPCASE (X1,1)
      ! Conversão da ação para maiúsculas

      IF (X1 .EQ. S1) GO TO 480
      IF (X1 .EQ. W1) GO TO 140
      IF (X1 .NE. A1) GO TO 70
      ! Verificação da ação para arquivo existente

      SAVE(2) = BBK(1)
      SAVE(3) = BBK(2)
      ! Definição de variáveis de controle

 110  READ (LU,180,END=115) SAVE
      ! Leitura do arquivo existente

      IF (SAVE(1).EQ.BEGN .AND. SAVE(4).EQ.BLANK) FFFLAG = 1234
      IF (SAVE(19) .EQ. KSMB(IKI)) IKI = IKI + 1
      GO TO 110
      ! Verificação de flags e controle de leitura

 115  BACKSPACE LU
      ! Retorno ao início do arquivo

      IF (FFFLAG .EQ. 1234) WRITE (NOUT,120)
      ! Mensagem de aviso para formato Free-Field

 120  FORMAT (/,' IF EXISTING FILE CONTAINS FREE-FIELD INPUT CARDS, ',
     1        ' THIS PROGRAM WILL NOT', /5X,'EXPAND THEM TO FIXED-',
     2        ' FIELD FORMATS',/)
      ! Formato da mensagem de aviso para formato Free-Field

      WRITE (NOUT,255) SAVE
      ! Escrita do último cartão lido

      IF (FFFLAG.EQ.1234 .AND. INFLAG.EQ.0) CALL FFHELP (*125,*125,5)
      ! Chamada de ajuda para formato Free-Field

 125  CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 140
      ! Definição de variáveis de controle e continuação

 130  IF (ODNW .EQ. NEW) GO TO 310
      ODNW = NEW
      GO TO 60
      ! Tratamento de erro na abertura de arquivo

 140  IF (MACH .EQ. UNIVAC) J = FACSF(USE)
      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8
      IF (FNAME(1) .EQ.  BLNK8) WRITE (NOUT,150)
      ! Ajuste de variáveis para UNIVAC e verificação do nome do arquivo

 150  FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)
      ! Formato da mensagem de saída não salva

      WRITE  (NOUT,160)
      ! Mensagem de início do programa

 160  FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',
     1 /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2 /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',
     3 //5X,'PLEASE BEGIN -',/)
      ! Formato da mensagem de início do programa

 170  CALL FFREAD (*320,CARD)
      ! Chamada de leitura de cartão

      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230
      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230
      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350
      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400
      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370
      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)
     1   CALL FFHELP (*280,*400,2)
      ! Verificação de comandos especiais

      IF (LU .EQ. 2) WRITE (LU,180) CARD
      ! Escrita do cartão no arquivo de saída

 180  FORMAT (20A4)
      ! Formato da escrita do cartão

      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD
      ! Escrita do cartão na tela

 190  FORMAT (1X,20A4)
      ! Formato da escrita do cartão na tela

      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340
      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)
     1   GO TO 170
      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170
      GO TO 410
      ! Verificação de comandos especiais e continuação

 230  IF (LU .NE. 2) GO TO 290
      J = 1
      IF (CARD(5).EQ.CANC1 .AND. ICONT(1).GT.0) J = ICONT(1) + 1
      IF (CARD(5).EQ. LIST .AND. ICONT(1).GT.0) J = ICONT(1)
      DO 240 I = 1,J
 240  BACKSPACE LU
      ICONT(1) = 0
      IF (CARD(5) .EQ. LIST) GO TO 260
      READ (LU,180) SAVE
      J = J - 1
      WRITE  (NOUT,250) J
      ! Cancelamento de cartões anteriores

 250  FORMAT (1X,I4,' PREVIOUSLY GENERATED CARDS CANCELLED ***')
      ! Formato da mensagem de cancelamento de cartões anteriores

      IF (J .GT. 0) WRITE (NOUT,255) SAVE
      ! Escrita do último cartão cancelado

 255  FORMAT (/,' *** LAST CARD WAS:', /1X,20A4)
      ! Formato da escrita do último cartão cancelado

      GO TO 280
      ! Continuação

 260  WRITE  (NOUT,265) J
      ! Lista de cartões anteriores

 265  FORMAT (//,' *** PREVIOUS',I4,' CARDS WERE (COLS. 1-79) -',/)
      ! Formato da lista de cartões anteriores

      DO 270 I = 1,J
      READ (LU,180,END=285) SAVE
 270  WRITE  (NOUT,275) SAVE
      ! Leitura e escrita dos cartões anteriores

 275  FORMAT (1X,20A4)
      ! Formato da escrita dos cartões anteriores

 280  CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 170
      ! Definição de variáveis de controle e continuação

 285  BACKSPACE LU
      SAVE(1) = CARD(1)
      SAVE(2) = CARD(2)
      GO TO 170
      ! Retorno ao início do arquivo e continuação

 290  WRITE  (NOUT,300) CARD4,CARD(2)
      ! Mensagem de opção não ativa

 300  FORMAT (' *** ',A4,A3,'OPTION NOT ACTIVE.  NO SAVE FILE ',
     1        'REQUESTED')
      ! Formato da mensagem de opção não ativa

      GO TO 170
      ! Continuação

 310  WRITE  (NOUT,315) FNAME
      ! Mensagem de erro na atribuição de arquivo

 315  FORMAT (' *** CAN NOT ASSIGN FILE - ',4A8)
      ! Formato da mensagem de erro na atribuição de arquivo

      GO TO 20
      ! Retorno ao início do programa

 320  WRITE  (NOUT,325)
      ! Mensagem de erro de entrada

 325  FORMAT (' *INPUT ERROR/FF*')
      ! Formato da mensagem de erro de entrada

      GO TO 170
      ! Continuação

 330  WRITE  (NOUT,335)
      ! Mensagem de nome de arquivo inválido

 335  FORMAT (' *NOT A VALID FILE NAME*')
      ! Formato da mensagem de nome de arquivo inválido

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      ! Rebobinamento da entrada para CDC

      GO TO 20
      ! Retorno ao início do programa

 340  FFFLAG = 1234
      IF (INFLAG .EQ. 0) CALL FFHELP (*170,*170,5)
      GO TO 170
      ! Definição de flag e chamada de ajuda para formato Free-Field

 350  I = 3
      IF (CARD1(5) .EQ. RPRN) I = 4
      READ (CARD1(I),355,ERR=170) II
      ! Leitura do número do campo a ser substituído

 355  FORMAT (I1)
      ! Formato da leitura do número do campo

      IF (II .EQ. 0) II = 10
      I = I + 2
      DO 360 J = 1,8
      SPL(J) = CARD1(I)
 360  I = I + 1
      SAVE(II) = SPILL
      DO 365 I = 1,10
 365  CARD(I) = SAVE(I)
      BACKSPACE LU
      WRITE (LU,  180) SAVE
      WRITE (NOUT,190) SAVE
      GO TO 170
      ! Substituição de campo e continuação

 370  IF (LU .NE. 2) GO TO 385
      DO 375 J = 3,18
      IF (CARD(J) .NE. BLANK) GO TO 380
 375  CONTINUE
      GO TO 170
      ! Verificação de cartão em branco

 380  BACKSPACE LU
      READ (LU,180) SAVE
      IF (SAVE(19) .EQ. BLANK) GO TO 390
      WRITE  (NOUT,382)
      ! Mensagem de entrada inválida

 382  FORMAT (/,' BAD INPUT - FIRST FIELD BLANK. TRY AGAIN')
      ! Formato da mensagem de entrada inválida

      WRITE  (NOUT,383)
      ! Mensagem de continuação não permitida

 383  FORMAT (13X,'NOT ALLOW. PREVIOUS CARD HAS CONTINUATION FIELD ',
     1        'DEFINED')
      ! Formato da mensagem de continuação não permitida

      GO TO 170
      ! Continuação

 385  WRITE  (NOUT,382)
      WRITE  (NOUT,387)
      ! Mensagem de entrada inválida sem arquivo de saída

 387  FORMAT (13X,'NOT ALLOW WITHOUT SAVE FILE')
      ! Formato da mensagem de entrada inválida sem arquivo de saída

      GO TO 170
      ! Continuação

 390  KONTN = KONTN + 1
      IF (MOD(KONTN,10000) .EQ. 0) IKI = IKI + 1
      CALL INT2K8 (*385,KONTN,SAVE(19))
      SAVE(19) = KSMB(IKI)
      CARD(1 ) = KSMB(IKI)
      CARD(2 ) = SAVE(20)
      WRITE  (NOUT,395)
      ! Substituição de cartões anteriores

 395  FORMAT (' ...PREVIOUS CARDS REPLACED BY:')
      ! Formato da mensagem de substituição de cartões anteriores

      WRITE (NOUT,190) SAVE
      WRITE (NOUT,190) CARD
      BACKSPACE LU
      WRITE (LU,180) SAVE
      WRITE (LU,180) CARD
      GO TO 170
      ! Escrita dos cartões substituídos e continuação

 400  BACKSPACE LOUT
      ! Retorno ao início do arquivo de saída

 410  ENDFILE LOUT
      IF (LU .NE. 6) ENDFILE LU
      PUNCH =.FALSE.
      SITE  = BLNK8
      ! Fim do arquivo de saída e definição de variáveis de controle

 420  WRITE  (NOUT,430) QMARK
      ! Solicitação de perfuração do deck NASTRAN

 430  FORMAT (/,' *** DO YOU WANT TO PUNCH OUT THE NASTRAN DECK',A1,
     1       ' (Y,N,X,HELP) ')
      ! Formato da solicitação de perfuração do deck NASTRAN

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      ! Rebobinamento da entrada para CDC

      READ (IN,90,END=420) X1
      ! Leitura da resposta para perfuração

      CALL UPCASE (X1,1)
      ! Conversão da resposta para maiúsculas

      IF (X1 .EQ. H1) CALL FFHELP (*420,*480,3)
      IF (X1 .EQ. N1) GO TO 500
      IF (X1.NE.Y1 .AND. X1.NE.FX) GO TO 420
      ! Verificação da resposta para perfuração

      PUNCH =.TRUE.
      LX = LOUT
      IF (X1 .EQ. FX) LX = LU
      IF (MACH .NE. UNIVAC) GO TO 460
      IPUN = 1
      WRITE  (NOUT,440)
      ! Solicitação do site-ID para perfuração

 440  FORMAT (/,' *** ENTER SITE-ID, OR ''CENTRAL'', WHERE CARDS ARE',
     1       ' TO BE PUNCHED ')
      ! Formato da solicitação do site-ID para perfuração

 450  READ (IN,40,ERR=450,END=450) SITE
      ! Leitura do site-ID para perfuração

      IF (SITE .EQ. BLNK8) GO TO 490
      CALL UPCASE (SITE,8)
      IF (SITE .EQ. CNTRL) GO TO 460
      ! Verificação do site-ID para perfuração

      SYM(3) = SITE
      J = FACSF(SYM)
      ! Definição do site-ID para perfuração

 460  REWIND LX
      ! Rebobinamento do arquivo de saída

 470  READ (LX,180,END=500) CARD
      DO 475 J = 2,NCNTL
      IF (CARD6 .NE. CNTLWD(J)) GO TO 475
      IF (CARD(4) .EQ. BLANK) GO TO 470
 475  CONTINUE
      IF ((CARD6.EQ.HELP6 .OR. CARD6.EQ.STOP6) .AND. CARD(3).EQ.BLANK)
     1   GO TO 470
      IF (CARD6.EQ.CNTLWD(1) .AND. CARD(4).EQ.BLANK) ICNTL = ICNTL + 1
      WRITE (IPUN,180) CARD
      GO TO 470
      ! Leitura e escrita do arquivo de saída para perfuração

 480  FNAME(1) = BLNK8
      ! Definição do nome do arquivo como em branco

 490  PUNCH    =.FALSE.
      ! Definição da perfuração como falsa

 500  WRITE  (NOUT,510)
      ! Mensagem de despedida

 510  FORMAT (//10X,'ADIEU MY FRIEND.  IT IS A PLEASURE TO SERVE YOU')
      ! Formato da mensagem de despedida

      IF (FNAME(1) .NE. BLNK8) WRITE (NOUT,520) FNAME
      ! Mensagem de lembrete do arquivo de saída

 520  FORMAT (10X,'DON''T FORGET - YOUR NASTRAN DECK IS IN FILE -',
     1    /25X,4A8, /10X,'WHICH IS ACCESSIBLE BY THE SYSTEM EDITOR')
      ! Formato da mensagem de lembrete do arquivo de saída

      IF (.NOT.PUNCH) GO TO 550
      WRITE  (NOUT,530)
      ! Mensagem de lembrete da perfuração

 530  FORMAT (/10X,'AND DON''T FORGET TO PICK UP YOUR PUNCHED CARDS')
      ! Formato da mensagem de lembrete da perfuração

      IF (SITE .NE. BLNK8) WRITE (NOUT,535) SITE
      IF (SITE.EQ.BLNK8 .AND. MACH.NE.VAX) WRITE (NOUT,540)
      IF (SITE.EQ.BLNK8 .AND. MACH.EQ.VAX) WRITE (NOUT,545)
      ! Mensagem de lembrete do site-ID para perfuração

 535  FORMAT (10X,'WHEN YOU SIGN OFF',22X,'SITE-ID: ',A8)
      ! Formato da mensagem de lembrete do site-ID para perfuração

 540  FORMAT (10X,'AT THE CENTRAL-SITE')
      ! Formato da mensagem de lembrete do site-ID para perfuração

 545  FORMAT (10X,'IN FORTRAN FILE FOR007.DAT')
      ! Formato da mensagem de lembrete do site-ID para perfuração

 550  IF (FNAME(1) .EQ. BLNK8) GO TO 570
      IF (MACH .EQ. UNIVAC) FOROO3 = TPF
      IF (MACH .EQ.    CDC) FOROO3 = TAPEO3
      WRITE  (NOUT,555)
      ! Mensagem de lembrete do arquivo de entrada

 555  FORMAT (//10X,'A COPY OF YOUR ACTUAL INPUT CARDS WAS SAVED IN')
      ! Formato da mensagem de lembrete do arquivo de entrada

      IF (MACH .NE. VAX) WRITE (NOUT,560) FOROO3
      IF (MACH .EQ. VAX) WRITE (NOUT,565)
      ! Mensagem de lembrete do arquivo de entrada

 560  FORMAT (1H+,56X,'FORTRAN FILE - ',A8)
      ! Formato da mensagem de lembrete do arquivo de entrada

 565  FORMAT (10X,'FORTRAN FILE FOR003.DAT')
      ! Formato da mensagem de lembrete do arquivo de entrada

 570  IF (ICNTL .NE. 0) WRITE (NOUT,575)
      ! Mensagem de aviso de cartões cancelados

 575  FORMAT (/4X,'*** WARNING - CANCELLED CARDS IN PUNCHED DECK NEED ',
     1       'TO BE REMOVED', /19X,'OR MODIFIED BEFORE USE')
      ! Formato da mensagem de aviso de cartões cancelados

      IF (FNAME(1) .NE. BLNK8) DORK = KEEP
      CLOSE (UNIT=LU  ,STATUS=DORK)
      CLOSE (UNIT=LOUT,STATUS=DORK)
      IF (DORK.EQ.KEEP .OR. (PUNCH .AND. MACH.EQ.VAX)) WRITE (NOUT,585)
      ! Fechamento de arquivos e mensagem de lembrete de exclusão

 585  FORMAT (/4X,'*** DON''T FORGET TO DELETE YOUR FILES GENERATED BY',
     1       ' THIS RUN ***')
      ! Formato da mensagem de lembrete de exclusão

      WRITE  (NOUT,590)
      ! Mensagem de conclusão

 590  FORMAT (/26X,'*** JOB DONE ***',/)
      ! Formato da mensagem de conclusão

 600  CONTINUE
      END
      ! Fim do programa
