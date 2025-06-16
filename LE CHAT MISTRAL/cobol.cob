IDENTIFICATION DIVISION.
* Esta divisão identifica o programa COBOL.

ENVIRONMENT DIVISION.
* Esta divisão define o ambiente de execução do programa.

INPUT-OUTPUT SECTION.
* Esta seção define os arquivos de entrada e saída.

FILE-CONTROL.
* Esta seção controla os arquivos usados pelo programa.
    SELECT PAYROLL-FILE-IN
        ASSIGN TO "TW12C.DAT".
    * Define o arquivo de entrada de folha de pagamento.
    SELECT INCOME-TAX-REPORT-OUT
        ASSIGN TO "PROG6_OUT.DAT".
    * Define o arquivo de saída do relatório de imposto de renda.

DATA DIVISION.
* Esta divisão define as variáveis e estruturas de dados usadas no programa.

FILE SECTION.
* Esta seção define os layouts dos arquivos.

FD  PAYROLL-FILE-IN
    RECORD CONTAINS 80 CHARACTERS
    LABEL RECORDS ARE OMITTED
    DATA RECORD IS PR-PAYROLL-REC-IN.
* Define o arquivo de entrada de folha de pagamento com registros de 80 caracteres.
01  PR-PAYROLL-REC-IN               PIC X(80).
* Define o registro de entrada de folha de pagamento.

FD  INCOME-TAX-REPORT-OUT
    RECORD CONTAINS 132 CHARACTERS
    LABEL RECORDS ARE OMITTED
    DATA RECORD IS IT-REPORT-OUT.
* Define o arquivo de saída do relatório de imposto de renda com registros de 132 caracteres.
01  IT-REPORT-OUT                   PIC X(132).
* Define o registro de saída do relatório de imposto de renda.

WORKING-STORAGE SECTION.
* Esta seção define as variáveis de trabalho usadas no programa.

01  WS-SWITCHES.
* Variáveis de controle.
    05  WS-EOF-SWITCH               PIC X(1).
    * Indicador de fim de arquivo.

01  WS-PAGE-LAYOUT.
* Variáveis para controle de layout da página.
    05  WS-LINES-USED               PIC 9(2).
    * Contador de linhas usadas na página.
    05  WS-PAGE-COUNT               PIC 9(2).
    * Contador de páginas.

01  WS-CALC-WORK-AREA.
* Área de trabalho para cálculos.
    05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99.
    * Total de isenções.
    05  WS-ANN-EARN                 PIC 9(6)V99.
    * Ganhos anuais.
    05  WS-EARNINGS                 PIC 9(6)V99.
    * Ganhos.
    05  WS-ANN-TAX-AMT              PIC 9(6)V99.
    * Valor anual do imposto.
    05  WS-PER-TAX-AMT              PIC 9(6)V99.
    * Valor do imposto por período.

01  WS-TABLE-CONTROLS.
* Controles para tabelas.
    05  WS-ROW-FOUND-SWITCH         PIC X(1).
    * Indicador de linha encontrada.
        88  WS-ROW-FOUND            VALUE "Y".
        88  WS-END-OF-TABLE         VALUE "E".

01  WS-TOTALS.
* Totais.
    05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99.
    * Total de ganhos neste período.
    05  WS-FED-TAX-AMT-TOT         PIC 9(6)V99.
    * Total do valor do imposto federal.
    05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99.
    * Total do imposto federal neste período.

01  WS-MARR-TAX-DATA.
* Dados de imposto para casados.
    05  FILLER               PIC X(16) VALUE "0000002400000000".
    05  FILLER               PIC X(16) VALUE "0240006600000015".
    05  FILLER               PIC X(16) VALUE "0660010900063018".
    05  FILLER               PIC X(16) VALUE "1090015000140421".
    05  FILLER               PIC X(16) VALUE "1500019200226524".
    05  FILLER               PIC X(16) VALUE "1920023600327328".
    05  FILLER               PIC X(16) VALUE "2360028900450532".
    05  FILLER               PIC X(16) VALUE "2890099999620137".

01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
* Tabela de imposto para casados.
    05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
                            INDEXED BY M-INDEX.
        10  WS-MARR-LOW      PIC 9(5).
        * Limite inferior.
        10  WS-MARR-HIGH     PIC 9(5).
        * Limite superior.
        10  WS-MARR-BASE-AMT PIC 9(4).
        * Valor base.
        10  WS-MARR-PERCENT  PIC V99.
        * Percentual.

01  WS-SING-TAX-DATA.
* Dados de imposto para solteiros.
    05  FILLER               PIC X(16) VALUE "0000001420000000".
    05  FILLER               PIC X(16) VALUE "0142003300000015".
    05  FILLER               PIC X(16) VALUE "0330006800028218".
    05  FILLER               PIC X(16) VALUE "0680010200091221".
    05  FILLER               PIC X(16) VALUE "1020014200162626".
    05  FILLER               PIC X(16) VALUE "1420017200266630".
    05  FILLER               PIC X(16) VALUE "1720022500356634".
    05  FILLER               PIC X(16) VALUE "2250099999536839".

01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
* Tabela de imposto para solteiros.
    05  WS-SING-TAX-ROW      OCCURS 8 TIMES
                            INDEXED BY S-INDEX.
        10  WS-SING-LOW      PIC 9(5).
        * Limite inferior.
        10  WS-SING-HIGH     PIC 9(5).
        * Limite superior.
        10  WS-SING-BASE-AMT PIC 9(4).
        * Valor base.
        10  WS-SING-PERCENT  PIC V99.
        * Percentual.

01  WS-PR-PAYROLL-REC-IN.
* Registro de entrada de folha de pagamento.
    05  WS-PR-REC-CODE-IN           PIC 9(2).
    * Código do registro.
    05  WS-PR-SSN-IN.
    * Número do seguro social.
        10  WS-PR-SSN-3-IN          PIC X(3).
        10  WS-PR-SSN-2-IN          PIC X(2).
        10  WS-PR-SSN-4-IN          PIC X(4).
    05  WS-PR-EMPL-NAME-IN          PIC X(18).
    * Nome do empregado.
    05  FILLER                      PIC X(20) VALUE SPACES.
    05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99.
    * Ganhos neste período.
    05  FILLER                      PIC X(14) VALUE SPACES.
    05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).
    * Estado civil.
    05  WS-PR-EXEMPTIONS-IN         PIC 9(1).
    * Número de isenções.
    05  FILLER                      PIC X(8) VALUE SPACES.

01  WS-HEADING.
* Cabeçalho do relatório.
    05  FILLER                      PIC X(7) VALUE "FEDERAL".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(6) VALUE "INCOME".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(3) VALUE "TAX".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "REGISTER".
    05  FILLER                      PIC X(50) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "PAGE".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  WS-PAGE-OUT                 PIC ZZ9.
    * Número da página.
    05  FILLER                      PIC X(47) VALUE SPACES.

01  WS-COLUMN-HEADING-01.
* Cabeçalho da coluna 01.
    05  FILLER                      PIC X(6) VALUE "SOCIAL".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "SEC.".
    05  FILLER                      PIC X(22) VALUE SPACES.
    05  FILLER                      PIC X(1) VALUE "M".
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(2) VALUE "WH".
    05  FILLER                      PIC X(3) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(10) VALUE "ANNUALIZED".
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "ADJUSTED".
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  FILLER                      PIC X(7) VALUE "FEDERAL".
    05  FILLER                      PIC X(5) VALUE SPACES.
    05  FILLER                      PIC X(3) VALUE "FED".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(3) VALUE "TAX".
    05  FILLER                      PIC X(36) VALUE SPACES.

01  WS-COLUMN-HEADING-02.
* Cabeçalho da coluna 02.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(6) VALUE "NUMBER".
    05  FILLER                      PIC X(7) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "EMPLOYEE".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "NAME".
    05  FILLER                      PIC X(5) VALUE SPACES.
    05  FILLER                      PIC X(1) VALUE "S".
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(2) VALUE "EX".
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "THIS".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "PER.".
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  FILLER                      PIC X(3) VALUE "TAX".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "AMT.".
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  FILLER                      PIC X(4) VALUE "THIS".
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(6) VALUE "PERIOD".
    05  FILLER                      PIC X(34) VALUE SPACES.

01  WS-DETAIL-LINE.
* Linha de detalhe.
    05  WS-SSN-OUT.
    * Número do seguro social.
        10  WS-SSN-3-OUT            PIC X(3).
        10  FILLER                  PIC X(1) VALUE "-".
        10  WS-SSN-2-OUT            PIC X(2).
        10  FILLER                  PIC X(1) VALUE "-".
        10  WS-SSN-4-OUT            PIC X(4).
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-EMPL-NAME-OUT            PIC X(18).
    * Nome do empregado.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-MARITAL-STATUS-OUT       PIC 9.
    * Estado civil.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-WITHHOLD-EXEMPT-OUT      PIC 99.
    * Número de isenções.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-EARN-THIS-PER-OUT        PIC ZZ,ZZZ.99.
    * Ganhos neste período.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-ANN-EARN-OUT             PIC ZZZ,ZZZ.99.
    * Ganhos anualizados.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-ADJ-EARN-OUT             PIC ZZZ,ZZZ.99.
    * Ganhos ajustados.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-FED-TAX-AMT-OUT          PIC ZZ,ZZZ.99.
    * Valor do imposto federal.
    05  FILLER                      PIC X(4) VALUE SPACES.
    05  WS-FED-TAX-THIS-PER-OUT     PIC Z,ZZZ.99.
    * Valor do imposto federal neste período.
    05  FILLER                      PIC X(36) VALUE SPACES.

01  WS-TAX-EXEMPT-LINE.
* Linha de isenção de imposto.
    05  WS-TE-SSN-OUT.
    * Número do seguro social.
        10  WS-TE-SSN-3-OUT         PIC X(3).
        10  FILLER                  PIC X(1) VALUE "-".
        10  WS-TE-SSN-2-OUT         PIC X(2).
        10  FILLER                  PIC X(1) VALUE "-".
        10  WS-TE-SSN-4-OUT         PIC X(4).
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-TE-EMPL-NAME-OUT         PIC X(18).
    * Nome do empregado.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-TE-MARITAL-STATUS-OUT    PIC 9.
    * Estado civil.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-TE-WITHHOLD-EXEMPT-OUT   PIC 99.
    * Número de isenções.
    05  FILLER                      PIC X(2) VALUE SPACES.
    05  WS-TE-EARN-THIS-PER-OUT     PIC ZZ,ZZZ.99.
    * Ganhos neste período.
    05  FILLER                      PIC X(3) VALUE SPACES.
    05  FILLER                      PIC X(3) VALUE "-  ".
    05  FILLER                      PIC X(7) VALUE "  T A X".
    05  FILLER                      PIC X(6) VALUE SPACES.
    05  FILLER                  PIC X(13) VALUE "E X E M P T  ".
    05  FILLER                      PIC X(3) VALUE "  -".

01  WS-TOTAL-LINE.
* Linha de total.
    05  FILLER                      PIC X(20) VALUE SPACES.
    05  FILLER                  PIC X(13) VALUE "T O T A L S :".
    05  FILLER                      PIC X(6) VALUE  SPACES.
    05  WS-TOT-EARN-THIS-PER-OUT    PIC ZZZ,ZZZ.99.
    * Total de ganhos neste período.
    05  FILLER                      PIC X(25) VALUE SPACES.
    05  WS-TOT-FED-TAX-AMT-OUT      PIC ZZZ,ZZZ.99.
    * Total do valor do imposto federal.
    05  FILLER                      PIC X(3) VALUE SPACES.
    05  WS-TOT-FED-TAX-THIS-PER-OUT PIC ZZ,ZZZ.99.
    * Total do imposto federal neste período.
    05  FILLER                      PIC X(1) VALUE SPACES.
    05  FILLER                      PIC X(1) VALUE "*".
    05  FILLER                      PIC X(34) VALUE SPACES.

PROCEDURE DIVISION.
* Esta divisão contém a lógica do programa.

A00-MAINLINE-PARA.
* Parágrafo principal.
    OPEN INPUT PAYROLL-FILE-IN
         OUTPUT INCOME-TAX-REPORT-OUT.
    * Abre os arquivos de entrada e saída.
    PERFORM B10-INIT-PARA.
    * Inicializa variáveis.
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.
    * Lê o primeiro registro do arquivo de entrada.
    PERFORM B20-PROCESS-PARA
        UNTIL WS-EOF-SWITCH = "Y".
    * Processa os registros até o fim do arquivo.
    PERFORM C20-TOTAL-PARA.
    * Calcula os totais.
    CLOSE PAYROLL-FILE-IN
       INCOME-TAX-REPORT-OUT.
    * Fecha os arquivos.
    STOP RUN.
    * Encerra o programa.

B10-INIT-PARA.
* Parágrafo de inicialização.
    MOVE ZEROS TO WS-LINES-USED
                  WS-EARN-THIS-PER-TOT
                  WS-FED-TAX-AMT-TOT
                  WS-FED-TAX-THIS-PER-TOT.
    * Zera os contadores e totais.
    MOVE 1 TO WS-PAGE-COUNT.
    * Inicializa o contador de páginas.
    PERFORM C10-HEADINGS-PARA.
    * Escreve os cabeçalhos.

B20-PROCESS-PARA.
* Parágrafo de processamento.
    MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING
        WS-EXEMPT-SUB-TOT ROUNDED.
    * Calcula o total de isenções.
    MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING
        WS-ANN-EARN.
    * Calcula os ganhos anualizados.
    SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
        WS-EARNINGS ROUNDED.
    * Calcula os ganhos ajustados.

    IF WS-PR-MARITAL-STATUS-IN = 1 THEN
        PERFORM C30-SINGLE-TAX-PARA
    ELSE
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.
    * Processa imposto para solteiros.

    IF WS-PR-MARITAL-STATUS-IN = 2 THEN
        PERFORM C40-MARRIED-TAX-PARA
    ELSE
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.
    * Processa imposto para casados.

    IF WS-PR-MARITAL-STATUS-IN = 3 THEN
        PERFORM C60-TAX-EXEMPT-PARA
    END-IF.
    * Processa isenção de imposto.

    ADD 2 TO WS-LINES-USED.
    * Incrementa o contador de linhas usadas.
    ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.
    * Adiciona os ganhos deste período ao total.
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.
    * Lê o próximo registro do arquivo de entrada.

C10-HEADINGS-PARA.
* Parágrafo de cabeçalhos.
    MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
    * Move o contador de páginas para a saída.
    WRITE IT-REPORT-OUT FROM WS-HEADING
        AFTER ADVANCING PAGE.
    * Escreve o cabeçalho principal.
    MOVE SPACES TO IT-REPORT-OUT.
    * Limpa a linha de saída.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve uma linha em branco.
    WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
        AFTER ADVANCING 1 LINES.
    * Escreve o cabeçalho da coluna 01.
    WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
        AFTER ADVANCING 1 LINE.
    * Escreve o cabeçalho da coluna 02.
    ADD 3 TO WS-LINES-USED.
    * Incrementa o contador de linhas usadas.
    ADD 1 TO WS-PAGE-COUNT.
    * Incrementa o contador de páginas.

C20-TOTAL-PARA.
* Parágrafo de totais.
    IF WS-LINES-USED >= 57 THEN
        PERFORM C10-HEADINGS-PARA
        MOVE ZEROS TO WS-LINES-USED
    END-IF.
    * Verifica se é necessário escrever novos cabeçalhos.
    MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
    * Move o total de ganhos neste período para a saída.
    MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
    * Move o total do valor do imposto federal para a saída.
    MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
    * Move o total do imposto federal neste período para a saída.
    MOVE SPACES TO IT-REPORT-OUT.
    * Limpa a linha de saída.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve uma linha em branco.
    WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
        AFTER ADVANCING 2 LINE.
    * Escreve a linha de total.

C30-SINGLE-TAX-PARA.
* Parágrafo de imposto para solteiros.
    SET S-INDEX TO 1.
    * Inicializa o índice da tabela de imposto para solteiros.
    SEARCH WS-SING-TAX-ROW
        AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
                         IS <= WS-SING-HIGH (S-INDEX)
        MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Procura a faixa de imposto adequada.
    IF WS-ROW-FOUND THEN
        COMPUTE WS-ANN-TAX-AMT =
            WS-SING-BASE-AMT (S-INDEX) +
            WS-SING-PERCENT (S-INDEX) *
            (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
    * Calcula o valor anual do imposto.
    PERFORM C50-LINE-OUTPUT-PARA.
    * Escreve a linha de saída.

C40-MARRIED-TAX-PARA.
* Parágrafo de imposto para casados.
    SET M-INDEX TO 1.
    * Inicializa o índice da tabela de imposto para casados.
    SEARCH WS-MARR-TAX-ROW
        AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-MARR-LOW (M-INDEX) AND
                         IS <= WS-MARR-HIGH (M-INDEX)
        MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Procura a faixa de imposto adequada.
    IF WS-ROW-FOUND THEN
        COMPUTE WS-ANN-TAX-AMT =
            WS-MARR-BASE-AMT (M-INDEX) +
            WS-MARR-PERCENT (M-INDEX) *
            (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
    * Calcula o valor anual do imposto.
    PERFORM C50-LINE-OUTPUT-PARA.
    * Escreve a linha de saída.

C50-LINE-OUTPUT-PARA.
* Parágrafo de saída de linha.
    DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
    * Calcula o valor do imposto por período.
    ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
    * Adiciona o valor anual do imposto ao total.
    ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
    * Adiciona o valor do imposto por período ao total.
    MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT.
    MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT.
    MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT.
    * Move o número do seguro social para a saída.
    MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT.
    * Move o nome do empregado para a saída.
    MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT.
    * Move o estado civil para a saída.
    MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT.
    * Move o número de isenções para a saída.
    MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT.
    * Move os ganhos neste período para a saída.
    MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT.
    * Move os ganhos anualizados para a saída.
    MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT.
    * Move os ganhos ajustados para a saída.
    MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT.
    * Move o valor do imposto por período para a saída.
    MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT.
    * Move o valor anual do imposto para a saída.
    IF WS-LINES-USED >= 57 THEN
        PERFORM C10-HEADINGS-PARA
        MOVE ZEROS TO WS-LINES-USED
    END-IF.
    * Verifica se é necessário escrever novos cabeçalhos.
    MOVE SPACES TO IT-REPORT-OUT.
    * Limpa a linha de saída.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve uma linha em branco.
    WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
        AFTER ADVANCING 1 LINE.
    * Escreve a linha de detalhe.

C60-TAX-EXEMPT-PARA.
* Parágrafo de isenção de imposto.
    ADD 2 TO WS-LINES-USED.
    * Incrementa o contador de linhas usadas.
    MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
    MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
    MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
    * Move o número do seguro social para a saída.
    MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
    * Move o nome do empregado para a saída.
    MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
    * Move o estado civil para a saída.
    MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
    * Move o número de isenções para a saída.
    MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
    * Move os ganhos neste período para a saída.
    MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Define o indicador de linha encontrada.
    IF WS-ROW-FOUND THEN
        MOVE SPACES TO IT-REPORT-OUT.
        * Limpa a linha de saída.
        WRITE IT-REPORT-OUT
            AFTER ADVANCING 1 LINE.
        * Escreve uma linha em branco.
        WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
            AFTER ADVANCING 1 LINE.
        * Escreve a linha de isenção de imposto.
