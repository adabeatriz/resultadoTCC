IDENTIFICATION DIVISION.
*---------------------------------------------------------------*
* Programa: PROG6                                               *
* Descrição: Programa para cálculo e relatório de imposto de     *
*            renda federal a partir de um arquivo de folha de    *
*            pagamento                                          *
* Autor: [Seu Nome]                                             *
* Data: [Data]                                                  *
*---------------------------------------------------------------*

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
*---------------------------------------------------------------*
* Definição dos arquivos de entrada e saída                      *
*---------------------------------------------------------------*
     SELECT PAYROLL-FILE-IN          * Arquivo de entrada com dados da folha de pagamento
         ASSIGN TO "TW12C.DAT".
     SELECT INCOME-TAX-REPORT-OUT    * Arquivo de saída com o relatório de imposto de renda
         ASSIGN TO "PROG6_OUT.DAT".

DATA DIVISION.
FILE SECTION.
*---------------------------------------------------------------*
* Definição da estrutura do arquivo de entrada                   *
*---------------------------------------------------------------*
FD  PAYROLL-FILE-IN
         RECORD CONTAINS 80 CHARACTERS
         LABEL RECORDS ARE OMITTED
         DATA RECORD IS PR-PAYROLL-REC-IN.
01  PR-PAYROLL-REC-IN               PIC X(80).

*---------------------------------------------------------------*
* Definição da estrutura do arquivo de saída                     *
*---------------------------------------------------------------*
FD  INCOME-TAX-REPORT-OUT
         RECORD CONTAINS 132 CHARACTERS
         LABEL RECORDS ARE OMITTED
         DATA RECORD IS IT-REPORT-OUT.
01  IT-REPORT-OUT                   PIC X(132).

WORKING-STORAGE SECTION.
*---------------------------------------------------------------*
* Variáveis de controle                                          *
*---------------------------------------------------------------*
01  WS-SWITCHES.
     05  WS-EOF-SWITCH               PIC X(1).  * Indicador de fim de arquivo
         88  WS-EOF                  VALUE "Y". * Valor que indica fim de arquivo

01  WS-PAGE-LAYOUT.
     05  WS-LINES-USED               PIC 9(2).  * Contador de linhas usadas na página
     05  WS-PAGE-COUNT               PIC 9(2).  * Contador de páginas

*---------------------------------------------------------------*
* Área de trabalho para cálculos                                 *
*---------------------------------------------------------------*
01  WS-CALC-WORK-AREA.
     05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99.  * Valor total de isenções
     05  WS-ANN-EARN                 PIC 9(6)V99.  * Ganhos anuais
     05  WS-EARNINGS                 PIC 9(6)V99.  * Ganhos ajustados
     05  WS-ANN-TAX-AMT              PIC 9(6)V99.  * Valor anual do imposto
     05  WS-PER-TAX-AMT              PIC 9(6)V99.  * Valor do imposto no período

*---------------------------------------------------------------*
* Controles para manipulação de tabelas                          *
*---------------------------------------------------------------*
01  WS-TABLE-CONTROLS.
     05  WS-ROW-FOUND-SWITCH         PIC X(1).     * Indicador de linha encontrada
         88  WS-ROW-FOUND            VALUE "Y".    * Linha encontrada
         88  WS-END-OF-TABLE         VALUE "E".    * Fim da tabela

*---------------------------------------------------------------*
* Totais acumulados                                              *
*---------------------------------------------------------------*
01  WS-TOTALS.
     05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99.  * Total de ganhos no período
     05  WS-FED-TAX-AMT-TOT         PIC 9(6)V99.  * Total de imposto federal anual
     05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99.  * Total de imposto federal no período

*---------------------------------------------------------------*
* Tabela de taxas para casados                                   *
* Estrutura: Limite inferior, Limite superior, Valor base, %     *
*---------------------------------------------------------------*
01  WS-MARR-TAX-DATA.
     05  FILLER               PIC X(16) VALUE "0000002400000000". * Faixa 1
     05  FILLER               PIC X(16) VALUE "0240006600000015". * Faixa 2
     05  FILLER               PIC X(16) VALUE "0660010900063018". * Faixa 3
     05  FILLER               PIC X(16) VALUE "1090015000140421". * Faixa 4
     05  FILLER               PIC X(16) VALUE "1500019200226524". * Faixa 5
     05  FILLER               PIC X(16) VALUE "1920023600327328". * Faixa 6
     05  FILLER               PIC X(16) VALUE "2360028900450532". * Faixa 7
     05  FILLER               PIC X(16) VALUE "2890099999620137". * Faixa 8

* Redefinição da tabela de casados para acesso indexado          *
01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
     05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
                              INDEXED BY M-INDEX.
         10  WS-MARR-LOW      PIC 9(5).      * Limite inferior da faixa
         10  WS-MARR-HIGH     PIC 9(5).     * Limite superior da faixa
         10  WS-MARR-BASE-AMT PIC 9(4).     * Valor base do imposto
         10  WS-MARR-PERCENT  PIC V99.      * Percentual da faixa

*---------------------------------------------------------------*
* Tabela de taxas para solteiros                                 *
* Estrutura: Limite inferior, Limite superior, Valor base, %     *
*---------------------------------------------------------------*
01  WS-SING-TAX-DATA.
     05  FILLER               PIC X(16) VALUE "0000001420000000". * Faixa 1
     05  FILLER               PIC X(16) VALUE "0142003300000015". * Faixa 2
     05  FILLER               PIC X(16) VALUE "0330006800028218". * Faixa 3
     05  FILLER               PIC X(16) VALUE "0680010200091221". * Faixa 4
     05  FILLER               PIC X(16) VALUE "1020014200162626". * Faixa 5
     05  FILLER               PIC X(16) VALUE "1420017200266630". * Faixa 6
     05  FILLER               PIC X(16) VALUE "1720022500356634". * Faixa 7
     05  FILLER               PIC X(16) VALUE "2250099999536839". * Faixa 8

* Redefinição da tabela de solteiros para acesso indexado        *
01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
     05  WS-SING-TAX-ROW      OCCURS 8 TIMES
                              INDEXED BY S-INDEX.
         10  WS-SING-LOW      PIC 9(5).      * Limite inferior da faixa
         10  WS-SING-HIGH     PIC 9(5).     * Limite superior da faixa
         10  WS-SING-BASE-AMT PIC 9(4).     * Valor base do imposto
         10  WS-SING-PERCENT  PIC V99.      * Percentual da faixa

*---------------------------------------------------------------*
* Estrutura do registro de entrada (folha de pagamento)          *
*---------------------------------------------------------------*
01  WS-PR-PAYROLL-REC-IN.
     05  WS-PR-REC-CODE-IN           PIC 9(2).      * Código do registro
     05  WS-PR-SSN-IN.                             * Número do seguro social
         10  WS-PR-SSN-3-IN          PIC X(3).     * Parte 1 do SSN
         10  WS-PR-SSN-2-IN          PIC X(2).     * Parte 2 do SSN
         10  WS-PR-SSN-4-IN          PIC X(4).     * Parte 3 do SSN
     05  WS-PR-EMPL-NAME-IN          PIC X(18).    * Nome do empregado
     05  FILLER                      PIC X(20) VALUE SPACES. * Espaços reservados
     05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99. * Ganhos no período
     05  FILLER                      PIC X(14) VALUE SPACES. * Espaços reservados
     05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).    * Estado civil (1=Solteiro, 2=Casado, 3=Isento)
     05  WS-PR-EXEMPTIONS-IN         PIC 9(1).    * Número de isenções
     05  FILLER                      PIC X(8) VALUE SPACES. * Espaços reservados

*---------------------------------------------------------------*
* Cabeçalho do relatório                                         *
*---------------------------------------------------------------*
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
     05  WS-PAGE-OUT                 PIC ZZ9.     * Número da página
     05  FILLER                      PIC X(47) VALUE SPACES.

*---------------------------------------------------------------*
* Cabeçalhos das colunas (linha 1)                              *
*---------------------------------------------------------------*
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

*---------------------------------------------------------------*
* Cabeçalhos das colunas (linha 2)                              *
*---------------------------------------------------------------*
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

*---------------------------------------------------------------*
* Linha de detalhe do relatório                                  *
*---------------------------------------------------------------*
01  WS-DETAIL-LINE.
     05  WS-SSN-OUT.                              * SSN formatado
         10  WS-SSN-3-OUT            PIC X(3).   * Parte 1 do SSN
         10  FILLER                  PIC X(1) VALUE "-". * Separador
         10  WS-SSN-2-OUT            PIC X(2).   * Parte 2 do SSN
         10  FILLER                  PIC X(1) VALUE "-". * Separador
         10  WS-SSN-4-OUT            PIC X(4).   * Parte 3 do SSN
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-EMPL-NAME-OUT            PIC X(18).  * Nome do empregado
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-MARITAL-STATUS-OUT       PIC 9.      * Estado civil
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-WITHHOLD-EXEMPT-OUT      PIC 99.     * Isenções
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-EARN-THIS-PER-OUT        PIC ZZ,ZZZ.99. * Ganhos no período
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-ANN-EARN-OUT             PIC ZZZ,ZZZ.99. * Ganhos anuais
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-ADJ-EARN-OUT             PIC ZZZ,ZZZ.99. * Ganhos ajustados
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-FED-TAX-AMT-OUT          PIC ZZ,ZZZ.99. * Valor anual do imposto
     05  FILLER                      PIC X(4) VALUE SPACES.
     05  WS-FED-TAX-THIS-PER-OUT     PIC Z,ZZZ.99. * Valor do imposto no período
     05  FILLER                      PIC X(36) VALUE SPACES.

*---------------------------------------------------------------*
* Linha para empregados isentos de imposto                       *
*---------------------------------------------------------------*
01  WS-TAX-EXEMPT-LINE.
     05  WS-TE-SSN-OUT.
         10  WS-TE-SSN-3-OUT         PIC X(3).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-TE-SSN-2-OUT         PIC X(2).
         10  FILLER                  PIC X(1) VALUE "-".
         10  WS-TE-SSN-4-OUT         PIC X(4).
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-EMPL-NAME-OUT         PIC X(18).
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-MARITAL-STATUS-OUT    PIC 9.
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-WITHHOLD-EXEMPT-OUT   PIC 99.
     05  FILLER                      PIC X(2) VALUE SPACES.
     05  WS-TE-EARN-THIS-PER-OUT     PIC ZZ,ZZZ.99.
     05  FILLER                      PIC X(3) VALUE SPACES.
     05  FILLER                      PIC X(3) VALUE "-  ".
     05  FILLER                      PIC X(7) VALUE "  T A X".
     05  FILLER                      PIC X(6) VALUE SPACES.
     05  FILLER                  PIC X(13) VALUE "E X E M P T  ".
     05  FILLER                      PIC X(3) VALUE "  -".

*---------------------------------------------------------------*
* Linha de totais do relatório                                   *
*---------------------------------------------------------------*
01  WS-TOTAL-LINE.
     05  FILLER                      PIC X(20) VALUE SPACES.
     05  FILLER                  PIC X(13) VALUE "T O T A L S :".
     05  FILLER                      PIC X(6) VALUE  SPACES.
     05  WS-TOT-EARN-THIS-PER-OUT    PIC ZZZ,ZZZ.99. * Total de ganhos
     05  FILLER                      PIC X(25) VALUE SPACES.
     05  WS-TOT-FED-TAX-AMT-OUT      PIC ZZZ,ZZZ.99. * Total anual de imposto
     05  FILLER                      PIC X(3) VALUE SPACES.
     05  WS-TOT-FED-TAX-THIS-PER-OUT PIC ZZ,ZZZ.99. * Total de imposto no período
     05  FILLER                      PIC X(1) VALUE SPACES.
     05  FILLER                      PIC X(1) VALUE "*".
     05  FILLER                      PIC X(34) VALUE SPACES.

PROCEDURE DIVISION.
*---------------------------------------------------------------*
* Rotina principal do programa                                    *
*---------------------------------------------------------------*
A00-MAINLINE-PARA.
     OPEN INPUT PAYROLL-FILE-IN
          OUTPUT INCOME-TAX-REPORT-OUT.
     PERFORM B10-INIT-PARA.
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
         AT END MOVE "Y" TO WS-EOF-SWITCH.
     PERFORM B20-PROCESS-PARA
         UNTIL WS-EOF-SWITCH = "Y".
     PERFORM C20-TOTAL-PARA.
     CLOSE PAYROLL-FILE-IN
        INCOME-TAX-REPORT-OUT.
     STOP RUN.

*---------------------------------------------------------------*
* Rotina de inicialização                                        *
*---------------------------------------------------------------*
B10-INIT-PARA.
     MOVE ZEROS TO WS-LINES-USED
                   WS-EARN-THIS-PER-TOT
                   WS-FED-TAX-AMT-TOT
                   WS-FED-TAX-THIS-PER-TOT.
     MOVE 1 TO WS-PAGE-COUNT.
     PERFORM C10-HEADINGS-PARA.

*---------------------------------------------------------------*
* Rotina de processamento principal                              *
*---------------------------------------------------------------*
B20-PROCESS-PARA.
* Calcula o valor total das isenções (1000 por isenção)
     MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING 
         WS-EXEMPT-SUB-TOT ROUNDED.
* Calcula os ganhos anuais (assumindo 26 períodos por ano)
     MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING 
         WS-ANN-EARN.
* Calcula os ganhos ajustados (subtraindo as isenções)
     SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
         WS-EARNINGS ROUNDED.
     
* Verifica o estado civil e chama a rotina apropriada
     IF WS-PR-MARITAL-STATUS-IN = 1 THEN
         PERFORM C30-SINGLE-TAX-PARA    * Solteiro
     ELSE
         MOVE "E" TO WS-ROW-FOUND-SWITCH
     END-IF.

     IF WS-PR-MARITAL-STATUS-IN = 2 THEN
         PERFORM C40-MARRIED-TAX-PARA   * Casado
     ELSE
         MOVE "E" TO WS-ROW-FOUND-SWITCH
     END-IF.

     IF WS-PR-MARITAL-STATUS-IN = 3 THEN
         PERFORM C60-TAX-EXEMPT-PARA    * Isento
     END-IF.

* Atualiza contadores e lê próximo registro
     ADD 2 TO WS-LINES-USED.
     ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
         AT END MOVE "Y" TO WS-EOF-SWITCH.

*---------------------------------------------------------------*
* Rotina para imprimir cabeçalhos do relatório                   *
*---------------------------------------------------------------*
C10-HEADINGS-PARA.
     MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
     WRITE IT-REPORT-OUT FROM WS-HEADING
         AFTER ADVANCING PAGE.
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
         AFTER ADVANCING 1 LINES.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
         AFTER ADVANCING 1 LINE.
     ADD 3 TO WS-LINES-USED.
     ADD 1 TO WS-PAGE-COUNT.

*---------------------------------------------------------------*
* Rotina para imprimir totais no final do relatório              *
*---------------------------------------------------------------*
C20-TOTAL-PARA.
     IF WS-LINES-USED >= 57 THEN
         PERFORM C10-HEADINGS-PARA
         MOVE ZEROS TO WS-LINES-USED
     END-IF.
     MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
     MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
     MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
         AFTER ADVANCING 2 LINE.

*---------------------------------------------------------------*
* Rotina para cálculo de imposto para solteiros                  *
*---------------------------------------------------------------*
C30-SINGLE-TAX-PARA.
     SET S-INDEX TO 1.
* Procura na tabela de solteiros a faixa correspondente aos ganhos
     SEARCH WS-SING-TAX-ROW
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
         WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
                          IS <= WS-SING-HIGH (S-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Se encontrou a faixa, calcula o imposto
     IF WS-ROW-FOUND THEN
         COMPUTE WS-ANN-TAX-AMT =
             WS-SING-BASE-AMT (S-INDEX) +
             WS-SING-PERCENT (S-INDEX) *
             (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
     PERFORM C50-LINE-OUTPUT-PARA.

*---------------------------------------------------------------*
* Rotina para cálculo de imposto para casados                    *
*---------------------------------------------------------------*
C40-MARRIED-TAX-PARA.
     SET M-INDEX TO 1.
* Procura na tabela de casados a faixa correspondente aos ganhos
     SEARCH WS-MARR-TAX-ROW
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
         WHEN WS-EARNINGS IS >= WS-MARR-LOW (M-INDEX) AND
                          IS <= WS-MARR-HIGH (M-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Se encontrou a faixa, calcula o imposto
     IF WS-ROW-FOUND THEN
         COMPUTE WS-ANN-TAX-AMT =
             WS-MARR-BASE-AMT (M-INDEX) +
             WS-MARR-PERCENT (M-INDEX) *
             (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
     PERFORM C50-LINE-OUTPUT-PARA.

*---------------------------------------------------------------*
* Rotina para gerar linha de saída no relatório                  *
*---------------------------------------------------------------*
C50-LINE-OUTPUT-PARA.
* Calcula o imposto do período (dividindo o anual por 26)
     DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
* Acumula totais
     ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
     ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
* Prepara os dados para impressão
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
* Verifica se precisa de nova página
     IF WS-LINES-USED >= 57 THEN
         PERFORM C10-HEADINGS-PARA
         MOVE ZEROS TO WS-LINES-USED
     END-IF.
* Imprime a linha
     MOVE SPACES TO IT-REPORT-OUT.
     WRITE IT-REPORT-OUT
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
         AFTER ADVANCING 1 LINE.           

*---------------------------------------------------------------*
* Rotina para empregados isentos de imposto                      *
*---------------------------------------------------------------*
C60-TAX-EXEMPT-PARA.
     ADD 2 TO WS-LINES-USED.
* Prepara os dados para impressão
     MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
     MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
     MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
     MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
     MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
     MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Imprime a linha de isento
     IF WS-ROW-FOUND THEN
         MOVE SPACES TO IT-REPORT-OUT.
         WRITE IT-REPORT-OUT
             AFTER ADVANCING 1 LINE.
         WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
             AFTER ADVANCING 1 LINE.