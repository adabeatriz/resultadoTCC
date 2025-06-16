C Programa FF - Gera arquivo de entrada NASTRAN em formato livre

      PROGRAM FF            
C Variáveis lógicas
      LOGICAL         STAR,     PCT,      NOTYET,   PUNCH,    UPFLAG    
C Variáveis inteiras
      INTEGER         SCREEN,   PROM,     FFFLAG,   FACSF,    NC(7),    
     1                IBM,      UNIVAC,   CDC,      VAX,      PC,       
     2                UNIX        
C Variáveis caractere
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
C Blocos comuns
      COMMON /SYSTEM/ IBUF,     NOUT,     NOGO,     IN,       ISYS(15)  
      COMMON /MACHIN/ MACH        
      COMMON /XXREAD/ INFLAG,   INSAVE,   LOOP4,    IBMCDC        
      COMMON /XECHOX/ FFFLAG,   IECHO(3), ISORT(5)        
      COMMON /XREADX/ SCREEN,   LOOP,     KOUNT,    PROM,     NOTYET,   
     1                STAR,     PCT,      ICONT(36)        
      COMMON /QMARKQ/ MARKQ,    TMP(16),  SPILL,    SAVE        
      COMMON /UPCASX/ UPFLAG,   UPID(3)        
C Equivalências
      EQUIVALENCE     (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),        
     1                (CARD(1),CARD6,CARD1(1),CARD2,CARD4),        
     2                (SPL(1),SPILL)        
C Inicialização de variáveis
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
      DATA            MTYPE                                          /  
     1                'IBM PC', '  IBM',  'UNIVAC', '  CDC',  '  VAX',  
     2                ' ***  ', '  UNIX'                             /  
      DATA            TAPEO3,   TPF,      FOROO3,   F                /  
     1                'TAPE3.', 'FF$$.',  'FT03.',  'FF       FF'    /  
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
      DATA      USE / '@USE 3.,', 'FF$$ .  '/       PUNCH /   .FALSE./  
      DATA      SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '          /  
      DATA            QMARK / '?' /           
C Determina o tipo de máquina
      J = LLJ - LLI        
      MACH = IBM        
      IF (J .EQ.   256) MACH = PC        
      IF (J .EQ.   512) MACH = UNIVAC        
      IF (J .GT. 65535) MACH = VAX        
      IF (J .GT. 2**30) MACH =  CDC        
      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))      
     1    MACH = UNIX        
      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))     
     1    MACH = PC             
C Inicialização de unidades de arquivo
      LU      = 2        
      LOUT    = 3        
      IN      = 5        
      NOUT    = 6        
      IPUN    = 7        
      SCREEN  = 6        
      IF (MACH .EQ. VAX) SCREEN = 5        
C Inicialização de variáveis de controle
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
C Abre arquivos
      IF (MACH .NE. CDC) GO TO 20        
      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')        
      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')        
      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')        
      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)        
      GO TO 20              
 15   STOP 'SCRATCH FILE ERROR, UNIT 3'               
 20   WRITE  (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)        
 25   FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),  
     1       2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,        
     2       /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,       
     3       'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',  
     4       '  30602')        
C Lê nome do arquivo de entrada
 30   WRITE  (NOUT,35) J        
 35   FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO'  
     1,       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')        
      READ (IN,40,ERR=330,END=330) FNAME        
 40   FORMAT (4A8)        
      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4)        
      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE        
C Verifica se arquivo de entrada é válido
      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1)       
     1   GO TO 330        
      IF (FNAME(2) .NE. BLNK8) GO TO 50        
      CALL UPCASE (FNAME,J)        
      IF (FN6 .EQ. BEGN6) GO TO 330        
      IF (FN6 .EQ. STOP6) GO TO 600        
      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1)        
C Determina tipo de arquivo de entrada
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
C Abre arquivo de entrada
 60   IF (MACH .NE. IBM) GO TO 65               
      I = IQZDDN(FNAME(1))        
      ODNW = OLD        
      IF (I .EQ. 0) ODNW = NEW        
      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)        
      CALL QQGETF (LU,FNAME(1),IERR)        
      IF (IERR .NE. 0) GO TO 130               
 65   IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130) 
      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32    ,STATUS=ODNW,ERR=130) 
C Lê arquivo de entrada
 70   WRITE  (NOUT,80)        
 80   FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',', 
     1        ' OR ''APPEND'' -')        
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN        
      READ (IN,90,END=70) X1        
 90   FORMAT (A1)        
      CALL UPCASE (X1,1)        
      IF (X1 .EQ. S1) GO TO 480        
      IF (X1 .EQ. W1) GO TO 140        
      IF (X1 .NE. A1) GO TO 70        
C Processa arquivo de entrada
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
C Erro ao abrir arquivo de entrada
 130  IF (ODNW .EQ. NEW) GO TO 310        
      ODNW = NEW        
      GO TO 60        
C Processa entrada de dados
 140  IF (MACH .EQ. UNIVAC) J = FACSF(USE)        
      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8        
      IF (FNAME(1) .EQ.  BLNK8) WRITE (NOUT,150)        
 150  FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)        
      WRITE  (NOUT,160)        
 160  FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',        
     1 /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2 /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',        
     3 //5X,'PLEASE BEGIN -',/)        
 170  CALL FFREAD (*320,CARD)        
C Verifica comandos
      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230        
      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230        
      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350        
      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400        
      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370        
      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)        
     1   CALL FFHELP (*280,*400,2)        
C Processa dados
      IF (LU .EQ. 2) WRITE (LU,180) CARD        
 180  FORMAT (20A4)        
      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD       
 190  FORMAT (1X,20A4)        
      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340        
      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)        
     1   GO TO 170        
      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170        
      GO TO 410        
C Fecha arquivos e finaliza programa
 600  CONTINUE        
      END