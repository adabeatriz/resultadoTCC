      PROGRAM FF           ! Programa principal chamado FF
C     Inicialização de variáveis lógicas usadas no controle do fluxo do programa
      LOGICAL         STAR,     PCT,      NOTYET,   PUNCH,    UPFLAG    

C     Variáveis inteiras usadas para controle de sistema, contadores, máquina, etc.
      INTEGER         SCREEN,   PROM,     FFFLAG,   FACSF,    NC(7),    
     1                IBM,      UNIVAC,   CDC,      VAX,      PC,       
     2                UNIX        

C     Declaração de variáveis do tipo caractere com diferentes tamanhos
      CHARACTER*1     FN(1),    BK1,      N1,       Y1,       X1,       
     1                S1,       W1,       A1,       H1,       CARD1(5), 
     2                FX,       MARKQ,    QMARK,    TMP,      RPRN,     
     3                SPL(8),   DOT,      D1,       T1        

      CHARACTER*2     CARD2,    REPL        
      CHARACTER*3     NEW,      OLD,      ODNW        
      CHARACTER*4     CARD(20), SAVE(20), BEGN,     HELP,     STOP,     
     1                LIST,     BLANK,    END1,     END2,     END3,     
     2                CANC1,    CANC2,    ALTER,    CARD4,    FN4,      
     3                BBK(2),   KSMB(9)        

      CHARACTER*6     FN6,      BEGN6,    HELP6,    STOP6,    CARD6,    
     1                MTYPE(7), CNTLWD(3)        

      CHARACTER*8     FNAME(4), MYFILE,   BLNK8,    SITE,     CNTRL,    
     1                USE(2),   SYM(4),   TAPEO3,   TPF,      FOROO3,   
     2                DORK,     KEEP,     SPILL        

      CHARACTER*11    F        
      CHARACTER*32    FN32        

C     Áreas de memória compartilhada entre partes do programa (COMMON blocks)
      COMMON /SYSTEM/ IBUF,     NOUT,     NOGO,     IN,       ISYS(15)  
      COMMON /MACHIN/ MACH        
      COMMON /XXREAD/ INFLAG,   INSAVE,   LOOP4,    IBMCDC        
      COMMON /XECHOX/ FFFLAG,   IECHO(3), ISORT(5)        
      COMMON /XREADX/ SCREEN,   LOOP,     KOUNT,    PROM,     NOTYET,   
     1                STAR,     PCT,      ICONT(36)        
      COMMON /QMARKQ/ MARKQ,    TMP(16),  SPILL,    SAVE        
      COMMON /UPCASX/ UPFLAG,   UPID(3)        

C     EQUIVALÊNCIA de variáveis para reuso da mesma área de memória
      EQUIVALENCE     (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),        
     1                (CARD(1),CARD6,CARD1(1),CARD2,CARD4),        
     2                (SPL(1),SPILL)        

C     Inicialização de dados - comandos e palavras-chave
      DATA            BEGN,     HELP,     STOP,     BLANK,    BK1    /  
     1                'BEGI',   'HELP',   'STOP',   '    ',   ' '    /  

      DATA            BEGN6,    HELP6,    STOP6,    BLNK8,    KEEP   /  
     1                'BEGIN ', 'HELP  ', 'STOP  ', '    ',   'KEEP' /  

      DATA            END1,     END2,     END3,     N1,       ALTER  /  
     1                'ENDD',   'END ',   'ENDA',   'N',      'LTER' /  

      DATA            CANC1,    CANC2,    DORK,     LLI,      LLJ    /  
     1                'CANC',   'EL  ',   'DELETE', 4H  I ,   4H  J  /  

      DATA            CNTRL,    MYFILE,   NEW,      OLD,      Y1     /  
     1                'CENTRAL','MIFYLE', 'NEW',    'OLD',    'Y'    /  

      DATA            NC/ 12,   7,   28,  7,   28,  0,  28           /  

      DATA            UNIVAC,   IBM, CDC, VAX, PC,      UNIX         /  
     1                3,        2,   4,   5,   1,       7            /  

C     Tipos de máquinas/sistemas que o programa pode reconhecer
      DATA            MTYPE                                          /  
     1                'IBM PC', '  IBM',  'UNIVAC', '  CDC',  '  VAX',  
     2                ' ***  ', '  UNIX'                             /  

C     Arquivos típicos utilizados para entrada/saída
      DATA            TAPEO3,   TPF,      FOROO3,   F                /  
     1                'TAPE3.', 'FF$$.',  'FT03.',  'FF       FF'    /  

C     Caracteres usados para leitura/interpretação de comandos
      DATA            S1,       A1,       BBK,                LIST   /  
     1                'S',      'A',      '(BLA', 'NK) ',     'LIST' /  

      DATA            W1,       H1,       FX,       RPRN,     ICNTL  /  
     1                'O',      'H',      'X',      ')',      0      /  

      DATA            DOT,      D1,       T1                         /  
     1                '.',      'D',      'T'                        /  

      DATA            CNTLWD,                       NCNTL,    REPL   /  
     1                'CANCEL', 'PROMPT', 'LIST  ', 3,        'R:'   /  

      DATA            KSMB  /   '+C0N',   '+C1N',   '+C2N',   '+C3N' ,  
     1                '+C4N',   '+C5N',   '+C6N',   '+C7N',   '+C8N' /  

C     Instruções iniciais do programa
      DATA      USE / '@USE 3.,', 'FF$$ .  '/       PUNCH /   .FALSE./  
      DATA      SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '          /  
      DATA            QMARK / '?' /           

C     Determinação da máquina com base em diferença de variáveis codificadas
      J = LLJ - LLI        
      MACH = IBM        
      IF (J .EQ.   256) MACH = PC        
      IF (J .EQ.   512) MACH = UNIVAC        
      IF (J .GT. 65535) MACH = VAX        
      IF (J .GT. 2**30) MACH = CDC        

C     Verificações adicionais para redefinir máquina como UNIX ou PC
      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))      
     1    MACH = UNIX        
      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))     
     1    MACH = PC             

C     Definição das unidades lógicas padrão (entrada/saída/etc.)
      LU      = 2        
      LOUT    = 3        
      IN      = 5        
      NOUT    = 6        
      IPUN    = 7        
      SCREEN  = 6        
      IF (MACH .EQ. VAX) SCREEN = 5        

C     Inicialização de variáveis de controle e estado do programa
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

C     Determina o número máximo de caracteres de nome de arquivo permitidos
      J       = NC(MACH)        

C     Inicialização de variáveis relacionadas à entrada/saída
      CARD4   = BLANK        
      CARD6   = STOP6        
      FN4     = STOP        
      FN6     = STOP6        
      LOOP4   = LOOP - 4        

C     Preenchimento do array SAVE com espaços em branco
      DO 5 I = 1,20        
 5    SAVE(I) = BLANK        

C     Determina se a máquina está no grupo IBM/CDC
      IBMCDC = UNIVAC + VAX + PC        
      IF (MACH.EQ.IBM .OR. MACH.EQ.CDC) IBMCDC = 0              

C     Abertura de arquivos padrão se máquina for CDC
      IF (MACH .NE. CDC) GO TO 20        
      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')        
      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')        
      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')        
      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)        
      GO TO 20              

C     Tratamento de erro na criação do arquivo temporário
 15   STOP 'SCRATCH FILE ERROR, UNIT 3'      ! Interrompe a execução se houver erro no arquivo temporário

      WRITE (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)
C     Escreve no dispositivo de saída NOUT, os dados de identificação do sistema, versão e informações do fornecedor.

 25   FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),  
     1       2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,        
     2       /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,       
     3       'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',  
     4       '  30602')              ! Mensagem padrão do cabeçalho

      WRITE (NOUT,35) J              ! Escreve no NOUT o número de caracteres permitidos
 35   FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO'  
     1,       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')        

      READ (IN,40,ERR=330,END=330) FNAME  ! Lê o nome do arquivo de entrada
 40   FORMAT (4A8)                  ! Nome do arquivo composto de até 4 strings de 8 caracteres

      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4) ! Ajuda se o nome do arquivo for o padrão
      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE          ! Define o nome padrão se for em branco

      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1) GO TO 330
C     Verifica se há final correto no nome do arquivo, caso contrário, considera inválido

      IF (FNAME(2) .NE. BLNK8) GO TO 50       ! Se houver continuação de nome, pula para o ponto 50

      CALL UPCASE (FNAME,J)                   ! Converte o nome do arquivo para maiúsculas
      IF (FN6 .EQ. BEGN6) GO TO 330           ! Se o comando for 'BEGIN', volta
      IF (FN6 .EQ. STOP6) GO TO 600           ! Se for 'STOP', encerra
      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1) ! Chama ajuda se for 'HELP'

 50   ODNW = OLD                              ! Define status padrão como arquivo já existente

      IF (MACH .NE. VAX) GO TO 60             ! Se não for VAX, vai para 60

      DO 52 I = 2,J                           ! Loop para adicionar extensão .DAT se não houver
      IF (FN(I) .EQ. DOT) GO TO 60
      IF (FN(I) .EQ. BK1) GO TO 55
 52   CONTINUE

      I = J + 1
 55   FN(I  ) = DOT
      FN(I+1) = D1
      FN(I+2) = A1
      FN(I+3) = T1

 60   IF (MACH .NE. IBM) GO TO 65

      I = IQZDDN(FNAME(1))        ! Checa se o nome do arquivo já está definido no sistema (IBM)
      ODNW = OLD
      IF (I .EQ. 0) ODNW = NEW     ! Se não estiver, define como novo

      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)
      CALL QQGETF (LU,FNAME(1),IERR)          ! Tenta obter o arquivo
      IF (IERR .NE. 0) GO TO 130              ! Se erro, vai para tratamento

 65   IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130) 
      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32    ,STATUS=ODNW,ERR=130) 
C     Abre o arquivo para leitura ou escrita, dependendo da plataforma

      IF (ODNW .EQ. NEW) GO TO 140

 70   WRITE (NOUT,80)           ! Arquivo já existe, oferece opções
 80   FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',',
     1        ' OR ''APPEND'' -')

      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      READ (IN,90,END=70) X1           ! Lê a opção do usuário
 90   FORMAT (A1)
      CALL UPCASE (X1,1)               ! Converte a opção para maiúsculo

      IF (X1 .EQ. S1) GO TO 480        ! STOP
      IF (X1 .EQ. W1) GO TO 140        ! OVERWRITE
      IF (X1 .NE. A1) GO TO 70         ! Se não for APPEND, repete

      SAVE(2) = BBK(1)
      SAVE(3) = BBK(2)

C     Leitura do conteúdo do arquivo já existente
 110  READ (LU,180,END=115) SAVE
      IF (SAVE(1).EQ.BEGN .AND. SAVE(4).EQ.BLANK) FFFLAG = 1234
      IF (SAVE(19) .EQ. KSMB(IKI)) IKI = IKI + 1
      GO TO 110

 115  BACKSPACE LU
      IF (FFFLAG .EQ. 1234) WRITE (NOUT,120)
 120  FORMAT (/,' IF EXISTING FILE CONTAINS FREE-FIELD INPUT CARDS, ',
     1        ' THIS PROGRAM WILL NOT', /5X,'EXPAND THEM TO FIXED-',
     2        ' FIELD FORMATS',/)

      WRITE (NOUT,255) SAVE
      IF (FFFLAG.EQ.1234 .AND. INFLAG.EQ.0) CALL FFHELP (*125,*125,5)

 125  CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 140

 130  IF (ODNW .EQ. NEW) GO TO 310
      ODNW = NEW
      GO TO 60

 140  IF (MACH .EQ. UNIVAC) J = FACSF(USE)
      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8
      IF (FNAME(1) .EQ.  BLNK8) WRITE (NOUT,150)
 150  FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)

      WRITE (NOUT,160)
 160  FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',
     1 /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2 /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',
     3 //5X,'PLEASE BEGIN -',/)

C     A partir daqui começa o loop principal de leitura e manipulação de cartões
170  CALL FFREAD (*320,CARD)

C Verifica comandos especiais inseridos no campo CARD
      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230  ! Comando CANCELA
      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230  ! Comando LISTA
      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350  ! Comando REPLACE
      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400  ! Comando STOP
      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370  ! Linha em branco
      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)
     1   CALL FFHELP (*280,*400,2)                         ! Ajuda

C Escreve a linha lida, se LU = 2
      IF (LU .EQ. 2) WRITE (LU,180) CARD
180   FORMAT (20A4)

C Escreve no output se certas condições são atendidas
      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD
190   FORMAT (1X,20A4)

C Verifica início do bloco BEGN
      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340

C Se não for END, continua lendo
      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)
     1   GO TO 170

C Verifica final do bloco
      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170

C Finaliza entrada
      GO TO 410

C ===== CANCELA ou LISTA =====
230   IF (LU .NE. 2) GO TO 290
      J = 1
C Se for comando CANCELA e houver histórico, ajusta J
      IF (CARD(5).EQ.CANC1 .AND. ICONT(1).GT.0) J = ICONT(1) + 1
      IF (CARD(5).EQ. LIST .AND. ICONT(1).GT.0) J = ICONT(1)

C Retrocede no arquivo LU
      DO 240 I = 1,J
240   BACKSPACE LU
      ICONT(1) = 0

C Leitura de linha anterior
      IF (CARD(5) .EQ. LIST) GO TO 260
      READ (LU,180) SAVE
      J = J - 1
      WRITE (NOUT,250) J
250   FORMAT (1X,I4,' PREVIOUSLY GENERATED CARDS CANCELLED ***')
      IF (J .GT. 0) WRITE (NOUT,255) SAVE
255   FORMAT (/,' *** LAST CARD WAS:', /1X,20A4)
      GO TO 280

260   WRITE (NOUT,265) J
265   FORMAT (//,' *** PREVIOUS',I4,' CARDS WERE (COLS. 1-79) -',/)
      DO 270 I = 1,J
        READ (LU,180,END=285) SAVE
270     WRITE (NOUT,275) SAVE
275     FORMAT (1X,20A4)

280   CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 170

285   BACKSPACE LU
      SAVE(1) = CARD(1)
      SAVE(2) = CARD(2)
      GO TO 170

C Caso LU ≠ 2, operação não disponível
290   WRITE (NOUT,300) CARD4,CARD(2)
300   FORMAT (' *** ',A4,A3,'OPTION NOT ACTIVE.  NO SAVE FILE REQUESTED')
      GO TO 170

C ===== ERROS =====
310   WRITE (NOUT,315) FNAME
315   FORMAT (' *** CAN NOT ASSIGN FILE - ',4A8)
      GO TO 20

320   WRITE (NOUT,325)
325   FORMAT (' *INPUT ERROR/FF*')
      GO TO 170

330   WRITE (NOUT,335)
335   FORMAT (' *NOT A VALID FILE NAME*')
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      GO TO 20

C ===== Comando BEGN =====
340   FFFLAG = 1234
      IF (INFLAG .EQ. 0) CALL FFHELP (*170,*170,5)
      GO TO 170

C ===== Comando REPLACE =====
350   I = 3
      IF (CARD1(5) .EQ. RPRN) I = 4
      READ (CARD1(I),355,ERR=170) II
355   FORMAT (I1)
      IF (II .EQ. 0) II = 10
      I = I + 2
      DO 360 J = 1,8
        SPL(J) = CARD1(I)
360     I = I + 1

C Substitui a entrada
      SAVE(II) = SPILL
      DO 365 I = 1,10
365     CARD(I) = SAVE(I)

      BACKSPACE LU
      WRITE (LU,180) SAVE
      WRITE (NOUT,190) SAVE
      GO TO 170

C ===== Entrada em branco =====
370   IF (LU .NE. 2) GO TO 385
      DO 375 J = 3,18
        IF (CARD(J) .NE. BLANK) GO TO 380
375   CONTINUE
      GO TO 170

C Substituição com campo de continuação
380   BACKSPACE LU
      READ (LU,180) SAVE
      IF (SAVE(19) .EQ. BLANK) GO TO 390
      WRITE (NOUT,382)
382   FORMAT (/,' BAD INPUT - FIRST FIELD BLANK. TRY AGAIN')
      WRITE (NOUT,383)
383   FORMAT (13X,'NOT ALLOW. PREVIOUS CARD HAS CONTINUATION FIELD DEFINED')
      GO TO 170

C Erro sem arquivo de backup
385   WRITE (NOUT,382)
      WRITE (NOUT,387)
387   FORMAT (13X,'NOT ALLOW WITHOUT SAVE FILE')
      GO TO 170

C Geração de novo número de cartão
390   KONTN = KONTN + 1
      IF (MOD(KONTN,10000) .EQ. 0) IKI = IKI + 1
      CALL INT2K8 (*385,KONTN,SAVE(19))
      SAVE(19) = KSMB(IKI)
      CARD(1 ) = KSMB(IKI)
      CARD(2 ) = SAVE(20)
      WRITE (NOUT,395)
395   FORMAT (' ...PREVIOUS CARDS REPLACED BY:')
      WRITE (NOUT,190) SAVE
      WRITE (NOUT,190) CARD
      BACKSPACE LU
      WRITE (LU,180) SAVE
      WRITE (LU,180) CARD
      GO TO 170

C ===== Encerramento - STOP =====
400   BACKSPACE LOUT
410   ENDFILE LOUT
      IF (LU .NE. 6) ENDFILE LU
      PUNCH =.FALSE.
      SITE  = BLNK8

C Pergunta ao usuário se deseja "punch out" o baralho NASTRAN
420   WRITE (NOUT,430) QMARK
430   FORMAT (/,' *** DO YOU WANT TO PUNCH OUT THE NASTRAN DECK',A1,' (Y,N,X,HELP) ')
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      READ (IN,90,END=420) X1
      CALL UPCASE (X1,1)

C Trata respostas
      IF (X1 .EQ. H1) CALL FFHELP (*420,*480,3)
      IF (X1 .EQ. N1) GO TO 500
      IF (X1.NE.Y1 .AND. X1.NE.FX) GO TO 420

C Ativa punch
      PUNCH =.TRUE.
      LX = LOUT
      IF (X1 .EQ. FX) LX = LU

C Configurações específicas por máquina
      IF (MACH .NE. UNIVAC) GO TO 460
      IPUN = 1
      WRITE (NOUT,440)
440   FORMAT (/,' *** ENTER SITE-ID, OR ''CENTRAL'', WHERE CARDS ARE TO BE PUNCHED ')
450   READ (IN,40,ERR=450,END=450) SITE
      IF (SITE .EQ. BLNK8) GO TO 490
      CALL UPCASE (SITE,8)
      IF (SITE .EQ. CNTRL) GO TO 460
      SYM(3) = SITE
      J = FACSF(SYM)

460   REWIND LX

C Percorre e escreve os cartões a serem punchados
470   READ (LX,180,END=500) CARD
      DO 475 J = 2,NCNTL
        IF (CARD6 .NE. CNTLWD(J)) GO TO 475
        IF (CARD(4) .EQ. BLANK) GO TO 470
475   CONTINUE
      IF ((CARD6.EQ.HELP6 .OR. CARD6.EQ.STOP6) .AND. CARD(3).EQ.BLANK) GO TO 470
      IF (CARD6.EQ.CNTLWD(1) .AND. CARD(4).EQ.BLANK) ICNTL = ICNTL + 1
      WRITE (IPUN,180) CARD
      GO TO 470

480   FNAME(1) = BLNK8

490   PUNCH =.FALSE.

500   WRITE (NOUT,510)
510   FORMAT (//10X,'ADIEU MY FRIEND.  IT IS A PLEASURE TO SERVE YOU')

      IF (FNAME(1) .NE. BLNK8) WRITE (NOUT,520) FNAME
520   FORMAT (10X,'DON''T FORGET - YOUR NASTRAN DECK IS IN FILE -',
     1    /25X,4A8, /10X,'WHICH IS ACCESSIBLE BY THE SYSTEM EDITOR')

C Mensagem de retirada dos cartões
      IF (.NOT.PUNCH) GO TO 550
      WRITE (NOUT,530)
530   FORMAT (/10X,'AND DON''T FORGET TO PICK UP YOUR PUNCHED CARDS')
      IF (SITE .NE. BLNK8) WRITE (NOUT,535) SITE
      IF (SITE.EQ.BLNK8 .AND. MACH.NE.VAX) WRITE (NOUT,540)
      IF (SITE.EQ.BLNK8 .AND. MACH.EQ.VAX) WRITE (NOUT,545)

535   FORMAT (10X,'WHEN YOU SIGN OFF',22X,'SITE-ID: ',A8)
540   FORMAT (10X,'AT THE CENTRAL-SITE')
545   FORMAT (10X,'IN FORTRAN FILE FOR007.DAT')

C Mensagem de cópia do arquivo salvo
550   IF (FNAME(1) .EQ. BLNK8) GO TO 570
      IF (MACH .EQ. UNIVAC) FOROO3 = TPF
      IF (MACH .EQ.    CDC) FOROO3 = TAPEO3
      WRITE (NOUT,555)
555   FORMAT (//10X,'A COPY OF YOUR ACTUAL INPUT CARDS WAS SAVED IN')
      IF (MACH .NE. VAX) WRITE (NOUT,560) FOROO3
      IF (MACH .EQ. VAX) WRITE (NOUT,565)
560   FORMAT (1H+,56X,'FORTRAN FILE - ',A8)
565   FORMAT (10X,'FORTRAN FILE FOR003.DAT')

570   IF (ICNTL .NE. 0) WRITE (NOUT,575)
575   FORMAT (/4X,'*** WARNING - CANCELLED CARDS IN PUNCHED DECK NEED TO BE REMOVED',
     1       /19X,'OR MODIFIED BEFORE USE')

      IF (FNAME(1) .NE. BLNK8) DORK = KEEP
      CLOSE (UNIT=LU  ,STATUS=DORK)
      CLOSE (UNIT=LOUT,STATUS=DORK)

      IF (DORK.EQ.KEEP .OR. (PUNCH .AND. MACH.EQ.VAX)) WRITE (NOUT,585)
585   FORMAT (/4X,'*** DON''T FORGET TO DELETE YOUR FILES GENERATED BY THIS RUN ***')

      WRITE (NOUT,590)
590   FORMAT (/26X,'*** JOB DONE ***',/)

600   CONTINUE
      END