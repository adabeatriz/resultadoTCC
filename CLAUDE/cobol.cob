* =====================================================================
* PROGRAMA: CÁLCULO DE IMPOSTO DE RENDA FEDERAL
* DESCRIÇÃO: Sistema para processar folha de pagamento e calcular
*            impostos federais baseado no estado civil e isenções
* AUTOR: [Nome do desenvolvedor]
* DATA: [Data de criação]
* =====================================================================

 IDENTIFICATION DIVISION.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
*    Arquivo de entrada contendo dados da folha de pagamento
     SELECT PAYROLL-FILE-IN
         ASSIGN TO "TW12C.DAT".
*    Arquivo de saída para relatório de impostos
     SELECT INCOME-TAX-REPORT-OUT
         ASSIGN TO "PROG6_OUT.DAT".

 DATA DIVISION.
 FILE SECTION.
*    Definição do arquivo de entrada da folha de pagamento
 FD  PAYROLL-FILE-IN
         RECORD CONTAINS 80 CHARACTERS
         LABEL RECORDS ARE OMITTED
         DATA RECORD IS PR-PAYROLL-REC-IN.
 01  PR-PAYROLL-REC-IN               PIC X(80).

*    Definição do arquivo de saída do relatório de impostos
 FD  INCOME-TAX-REPORT-OUT
         RECORD CONTAINS 132 CHARACTERS
         LABEL RECORDS ARE OMITTED
         DATA RECORD IS IT-REPORT-OUT.
 01  IT-REPORT-OUT                   PIC X(132).

 WORKING-STORAGE SECTION.
*    Switches de controle do programa
 01  WS-SWITCHES.
     05  WS-EOF-SWITCH               PIC X(1). 

*    Controle de paginação do relatório
 01  WS-PAGE-LAYOUT.
     05  WS-LINES-USED               PIC 9(2).  * Linhas utilizadas na página atual
     05  WS-PAGE-COUNT               PIC 9(2).  * Contador de páginas

*    Área de trabalho para cálculos
 01  WS-CALC-WORK-AREA.
     05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99.  * Subtotal de isenções
     05  WS-ANN-EARN                 PIC 9(6)V99.  * Rendimento anual
     05  WS-EARNINGS                 PIC 9(6)V99.  * Rendimento ajustado
     05  WS-ANN-TAX-AMT              PIC 9(6)V99.  * Valor anual do imposto
     05  WS-PER-TAX-AMT              PIC 9(6)V99.  * Valor do imposto por período

*    Controles para busca nas tabelas de impostos
 01  WS-TABLE-CONTROLS.
     05  WS-ROW-FOUND-SWITCH         PIC X(1).
         88  WS-ROW-FOUND            VALUE "Y".    * Linha encontrada na tabela
         88  WS-END-OF-TABLE         VALUE "E".    * Fim da tabela

*    Totalizadores do relatório
 01  WS-TOTALS.
     05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99.  * Total de rendimentos do período
     05  WS-FED-TAX-AMT-TOT         PIC 9(6)V99.   * Total anual de impostos
     05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99.  * Total de impostos do período

*    Dados da tabela de impostos para casados (hard-coded)
*    Formato: Valor Mínimo (5) + Valor Máximo (5) + Base (4) + Percentual (2)
 01  WS-MARR-TAX-DATA.
     05  FILLER               PIC X(16) VALUE "0000002400000000".
     05  FILLER               PIC X(16) VALUE "0240006600000015".
     05  FILLER               PIC X(16) VALUE "0660010900063018".
     05  FILLER               PIC X(16) VALUE "1090015000140421".
     05  FILLER               PIC X(16) VALUE "1500019200226524".
     05  FILLER               PIC X(16) VALUE "1920023600327328".
     05  FILLER               PIC X(16) VALUE "2360028900450532".
     05  FILLER               PIC X(16) VALUE "2890099999620137".

*    Redefinição dos dados como tabela estruturada para casados
 01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
     05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
                              INDEXED BY M-INDEX.
         10  WS-MARR-LOW      PIC 9(5).     * Limite inferior da faixa
         10  WS-MARR-HIGH     PIC 9(5).     * Limite superior da faixa  
         10  WS-MARR-BASE-AMT PIC 9(4).     * Valor base do imposto
         10  WS-MARR-PERCENT  PIC V99.      * Percentual sobre o excesso

*    Dados da tabela de impostos para solteiros (hard-coded)
 01  WS-SING-TAX-DATA.
     05  FILLER               PIC X(16) VALUE "0000001420000000".
     05  FILLER               PIC X(16) VALUE "0142003300000015".
     05  FILLER               PIC X(16) VALUE "0330006800028218".
     05  FILLER               PIC X(16) VALUE "0680010200091221".
     05  FILLER               PIC X(16) VALUE "1020014200162626".
     05  FILLER               PIC X(16) VALUE "1420017200266630".
     05  FILLER               PIC X(16) VALUE "1720022500356634".
     05  FILLER               PIC X(16) VALUE "2250099999536839".

*    Redefinição dos dados como tabela estruturada para solteiros
 01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
     05  WS-SING-TAX-ROW      OCCURS 8 TIMES
                              INDEXED BY S-INDEX.
         10  WS-SING-LOW      PIC 9(5).     * Limite inferior da faixa
         10  WS-SING-HIGH     PIC 9(5).     * Limite superior da faixa
         10  WS-SING-BASE-AMT PIC 9(4).     * Valor base do imposto
         10  WS-SING-PERCENT  PIC V99.      * Percentual sobre o excesso

*    Estrutura do registro de entrada da folha de pagamento
 01  WS-PR-PAYROLL-REC-IN.
     05  WS-PR-REC-CODE-IN           PIC 9(2).      * Código do registro
     05  WS-PR-SSN-IN.                              * CPF/SSN do funcionário
         10  WS-PR-SSN-3-IN          PIC X(3).
         10  WS-PR-SSN-2-IN          PIC X(2).
         10  WS-PR-SSN-4-IN          PIC X(4).
     05  WS-PR-EMPL-NAME-IN          PIC X(18).     * Nome do funcionário
     05  FILLER                      PIC X(20) VALUE SPACES.
     05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99.   * Rendimento do período
     05  FILLER                      PIC X(14) VALUE SPACES.
     05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).      * Estado civil (1=Solteiro, 2=Casado, 3=Isento)
     05  WS-PR-EXEMPTIONS-IN         PIC 9(1).      * Número de isenções
     05  FILLER                      PIC X(8) VALUE SPACES.

*    Cabeçalho principal do relatório
 01  WS-HEADING.
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
     05  WS-PAGE-OUT                 PIC ZZ9.       * Número da página
     05  FILLER                      PIC X(47) VALUE SPACES.

*    Primeira linha do cabeçalho das colunas
 01  WS-COLUMN-HEADING-01.
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

*    Segunda linha do cabeçalho das colunas
 01  WS-COLUMN-HEADING-02.
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

*    Linha de detalhe para funcionários com impostos calculados
 01  WS-DETAIL-LINE.
     05  WS-SSN-OUT.                                * CPF/SSN formatado
         10  WS-SSN-3-OUT            PIC X(3).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-SSN-2-OUT            PIC X(2).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-SSN-4-OUT            PIC X(4).
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-EMPL-NAME-OUT            PIC X(18).     * Nome do funcionário
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-MARITAL-STATUS-OUT       PIC 9.         * Estado civil
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-WITHHOLD-EXEMPT-OUT      PIC 99.        * Número de isenções
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-EARN-THIS-PER-OUT        PIC ZZ,ZZZ.99. * Rendimento do período
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-ANN-EARN-OUT             PIC ZZZ,ZZZ.99.* Rendimento anual
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-ADJ-EARN-OUT             PIC ZZZ,ZZZ.99.* Rendimento ajustado
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-FED-TAX-AMT-OUT          PIC ZZ,ZZZ.99. * Imposto anual
     05  FILLER                      PIC X(4) VALUE SPACES.
     05  WS-FED-TAX-THIS-PER-OUT     PIC Z,ZZZ.99.  * Imposto do período
     05  FILLER                      PIC X(36) VALUE SPACES.

*    Linha especial para funcionários isentos de impostos
 01  WS-TAX-EXEMPT-LINE.
     05  WS-TE-SSN-OUT.                             * CPF/SSN formatado
         10  WS-TE-SSN-3-OUT         PIC X(3).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-TE-SSN-2-OUT         PIC X(2).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-TE-SSN-4-OUT         PIC X(4).
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-EMPL-NAME-OUT         PIC X(18).     * Nome do funcionário
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-MARITAL-STATUS-OUT    PIC 9.         * Estado civil
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-WITHHOLD-EXEMPT-OUT   PIC 99.        * Número de isenções
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-EARN-THIS-PER-OUT     PIC ZZ,ZZZ.99. * Rendimento do período
     05  FILLER                      PIC X(3) VALUE SPACES.
     05  FILLER                      PIC X(3) VALUE "-  ".
     05  FILLER                      PIC X(7) VALUE "  T A X".
     05  FILLER                      PIC X(6) VALUE SPACES.
     05  FILLER                  PIC X(13) VALUE "E X E M P T  ".
     05  FILLER                      PIC X(3) VALUE "  -".

*    Linha de totais do relatório
 01  WS-TOTAL-LINE.
     05  FILLER                      PIC X(20) VALUE SPACES.
     05  FILLER                  PIC X(13) VALUE "T O T A L S :".
     05  FILLER                      PIC X(6) VALUE  SPACES.
     05  WS-TOT-EARN-THIS-PER-OUT    PIC ZZZ,ZZZ.99.* Total rendimentos período
     05  FILLER                      PIC X(25) VALUE SPACES.
     05  WS-TOT-FED-TAX-AMT-OUT      PIC ZZZ,ZZZ.99.* Total impostos anuais
     05  FILLER                      PIC X(3) VALUE SPACES.
     05  WS-TOT-FED-TAX-THIS-PER-OUT PIC ZZ,ZZZ.99. * Total impostos período
     05  FILLER                      PIC X(1) VALUE SPACES.
     05  FILLER                      PIC X(1) VALUE "*".
     05  FILLER                      PIC X(34) VALUE SPACES.

* =====================================================================
* DIVISÃO DE PROCEDIMENTOS
* =====================================================================
 PROCEDURE DIVISION.

*    Parágrafo principal - controla o fluxo geral do programa
 A00-MAINLINE-PARA.
*    Abertura dos arquivos de entrada e saída
     OPEN INPUT PAYROLL-FILE-IN
          OUTPUT INCOME-TAX-REPORT-OUT.
*    Inicialização das variáveis
     PERFORM B10-INIT-PARA.
*    Primeira leitura do arquivo
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
         AT END MOVE "Y" TO WS-EOF-SWITCH.
*    Loop principal de processamento
     PERFORM B20-PROCESS-PARA
         UNTIL WS-EOF-SWITCH = "Y".
*    Impressão dos totais
     PERFORM C20-TOTAL-PARA.
*    Fechamento dos arquivos
     CLOSE PAYROLL-FILE-IN
        INCOME-TAX-REPORT-OUT.
     STOP RUN.

*    Inicialização das variáveis e primeira página
 B10-INIT-PARA.
*    Zera contadores e totalizadores
     MOVE ZEROS TO WS-LINES-USED
                   WS-EARN-THIS-PER-TOT
                   WS-FED-TAX-AMT-TOT
                   WS-FED-TAX-THIS-PER-TOT.
*    Inicializa contador de páginas
     MOVE 1 TO WS-PAGE-COUNT.
*    Imprime cabeçalhos da primeira página
     PERFORM C10-HEADINGS-PARA.

*    Processamento de cada registro da folha de pagamento
 B20-PROCESS-PARA.
*    Calcula valor total das isenções (número de isenções × R$ 1.000)
     MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING 
         WS-EXEMPT-SUB-TOT ROUNDED.
*    Calcula rendimento anual (rendimento período × 26 períodos)
     MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING 
         WS-ANN-EARN.
*    Calcula rendimento tributável (anual - isenções)
     SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
         WS-EARNINGS ROUNDED.
     
*    Processamento baseado no estado civil
*    Estado civil 1 = Solteiro
     IF WS-PR-MARITAL-STATUS-IN = 1 THEN
         PERFORM C30-SINGLE-TAX-PARA
     ELSE
         MOVE "E" TO WS-ROW-FOUND-SWITCH
     END-IF.

*    Estado civil 2 = Casado
     IF WS-PR-MARITAL-STATUS-IN = 2 THEN
         PERFORM C40-MARRIED-TAX-PARA
     ELSE
         MOVE "E" TO WS-ROW-FOUND-SWITCH
     END-IF.

*    Estado civil 3 = Isento de impostos
     IF WS-PR-MARITAL-STATUS-IN = 3 THEN
         PERFORM C60-TAX-EXEMPT-PARA
     END-IF.

*    Atualiza contadores e lê próximo registro
     ADD 2 TO WS-LINES-USED.
     ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
         AT END MOVE "Y" TO WS-EOF-SWITCH.

*    Impressão dos cabeçalhos do relatório
 C10-HEADINGS-PARA.
*    Formata número da página
     MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
*    Imprime cabeçalho principal em nova página
     WRITE IT-REPORT-OUT FROM WS-HEADING
         AFTER ADVANCING PAGE.
*    Linha em branco
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
*    Cabeçalhos das colunas
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
         AFTER ADVANCING 1 LINES.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
         AFTER ADVANCING 1 LINE.
*    Atualiza contadores
     ADD 3 TO WS-LINES-USED.
     ADD 1 TO WS-PAGE-COUNT.

*    Impressão da linha de totais
 C20-TOTAL-PARA.
*    Verifica se precisa de nova página
     IF WS-LINES-USED >= 57 THEN
         PERFORM C10-HEADINGS-PARA
         MOVE ZEROS TO WS-LINES-USED
     END-IF.
*    Move totais para campos de saída
     MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
     MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
     MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
*    Imprime linha em branco e linha de totais
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
         AFTER ADVANCING 2 LINE.

*    Cálculo de impostos para solteiros
 C30-SINGLE-TAX-PARA.
*    Inicializa índice da tabela
     SET S-INDEX TO 1.
*    Busca faixa de imposto correspondente ao rendimento
     SEARCH WS-SING-TAX-ROW
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
         WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
                          IS <= WS-SING-HIGH (S-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*    Se encontrou a faixa, calcula o imposto
     IF WS-ROW-FOUND THEN
         COMPUTE WS-ANN-TAX-AMT =
             WS-SING-BASE-AMT (S-INDEX) +
             WS-SING-PERCENT (S-INDEX) *
             (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
*    Imprime linha de detalhe
     PERFORM C50-LINE-OUTPUT-PARA.

*    Cálculo de impostos para casados
 C40-MARRIED-TAX-PARA.
*    Inicializa índice da tabela
     SET M-INDEX TO 1.
*    Busca faixa de imposto correspondente ao rendimento
     SEARCH WS-MARR-TAX-ROW
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
         WHEN WS-EARNINGS IS >= WS-MARR-LOW (M-INDEX) AND
                          IS <= WS-MARR-HIGH (M-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*    Se encontrou a faixa, calcula o imposto
     IF WS-ROW-FOUND THEN
         COMPUTE WS-ANN-TAX-AMT =
             WS-MARR-BASE-AMT (M-INDEX) +
             WS-MARR-PERCENT (M-INDEX) *
             (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
*    Imprime linha de detalhe
     PERFORM C50-LINE-OUTPUT-PARA.

*    Formatação e impressão da linha de detalhe com impostos
 C50-LINE-OUTPUT-PARA.
*    Calcula imposto do período (anual ÷ 26 períodos)
     DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
*    Acumula nos totalizadores
     ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
     ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
*    Move dados do funcionário para linha de saída
     MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT.
     MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT.
     MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT.
     MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT.
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT.
     MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT.
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT.
     MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT.
     MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT.
     MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT.
     MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT.
*    Verifica se precisa de nova página
     IF WS-LINES-USED >= 57 THEN
         PERFORM C10-HEADINGS-PARA
         MOVE ZEROS TO WS-LINES-USED
     END-IF.
*    Imprime linha em branco e linha de detalhe
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
         AFTER ADVANCING 1 LINE.           

*    Processamento de funcionários isentos de impostos
 C60-TAX-EXEMPT-PARA.
*    Atualiza contador de linhas
     ADD 2 TO WS-LINES-USED.
*    Move dados do funcionário para linha de isenção
     MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
     MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
     MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
     MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
     MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
*    Marca como processado
     MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*    Imprime linha especial para isentos
     IF WS-ROW-FOUND THEN
         MOVE SPACES TO IT-REPORT-OUT.
         WRITE IT-REPORT-OUT
             AFTER ADVANCING 1 LINE.
         WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
             AFTER ADVANCING 1 LINE.