IDENTIFICATION DIVISION.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    * Define os arquivos utilizados: entrada (folha de pagamento) e saída (relatório de imposto de renda)
    SELECT PAYROLL-FILE-IN
        ASSIGN TO "TW12C.DAT".
    SELECT INCOME-TAX-REPORT-OUT
        ASSIGN TO "PROG6_OUT.DAT".

DATA DIVISION.
FILE SECTION.

    * Declaração do arquivo de entrada
FD  PAYROLL-FILE-IN
        RECORD CONTAINS 80 CHARACTERS
        LABEL RECORDS ARE OMITTED
        DATA RECORD IS PR-PAYROLL-REC-IN.
01  PR-PAYROLL-REC-IN               PIC X(80).  * Registro lido da folha

    * Declaração do arquivo de saída
FD  INCOME-TAX-REPORT-OUT
        RECORD CONTAINS 132 CHARACTERS
        LABEL RECORDS ARE OMITTED
        DATA RECORD IS IT-REPORT-OUT.
01  IT-REPORT-OUT                   PIC X(132). * Linha a ser escrita no relatório

WORKING-STORAGE SECTION.

    * Indicadores de controle
01  WS-SWITCHES.
    05  WS-EOF-SWITCH               PIC X(1).   * Indica fim de arquivo (EOF)

    * Controle de paginação do relatório
01  WS-PAGE-LAYOUT.
    05  WS-LINES-USED               PIC 9(2).   * Linhas usadas na página atual
    05  WS-PAGE-COUNT               PIC 9(2).   * Contador de páginas

    * Área para cálculos de rendimentos e impostos
01  WS-CALC-WORK-AREA.
    05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99. * Total isento (ex: deduções)
    05  WS-ANN-EARN                 PIC 9(6)V99. * Rendimento anualizado
    05  WS-EARNINGS                 PIC 9(6)V99. * Rendimento após isenções
    05  WS-ANN-TAX-AMT              PIC 9(6)V99. * Imposto anual
    05  WS-PER-TAX-AMT              PIC 9(6)V99. * Imposto por período

    * Controle de busca nas tabelas de impostos
01  WS-TABLE-CONTROLS.
    05  WS-ROW-FOUND-SWITCH         PIC X(1).   * Indicador de linha encontrada
        88  WS-ROW-FOUND            VALUE "Y".  * Linha encontrada
        88  WS-END-OF-TABLE         VALUE "E".  * Fim da tabela

    * Totais acumulados para o relatório
01  WS-TOTALS.
    05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99. * Total recebido neste período
    05  WS-FED-TAX-AMT-TOT          PIC 9(6)V99. * Total de impostos federais
    05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99. * Total de impostos deste período

    * Dados de imposto para contribuintes casados (codificados)
01  WS-MARR-TAX-DATA.
    * Cada FILLER contém dados de uma faixa da tabela: renda mínima, máxima, imposto base e percentual

    * Redefine os dados codificados para facilitar acesso por índice
01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
    05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
                             INDEXED BY M-INDEX.
        10  WS-MARR-LOW      PIC 9(5).        * Limite inferior
        10  WS-MARR-HIGH     PIC 9(5).        * Limite superior
        10  WS-MARR-BASE-AMT PIC 9(4).        * Valor base
        10  WS-MARR-PERCENT  PIC V99.         * Percentual de imposto

    * Dados de imposto para contribuintes solteiros
01  WS-SING-TAX-DATA.
    * Estrutura igual à dos casados, mas com faixas diferentes

    * Redefine os dados para acesso estruturado
01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
    05  WS-SING-TAX-ROW      OCCURS 8 TIMES
                             INDEXED BY S-INDEX.
        10  WS-SING-LOW      PIC 9(5).
        10  WS-SING-HIGH     PIC 9(5).
        10  WS-SING-BASE-AMT PIC 9(4).
        10  WS-SING-PERCENT  PIC V99.

    * Estrutura para mapear os dados lidos da folha para variáveis nomeadas
01  WS-PR-PAYROLL-REC-IN.
    05  WS-PR-REC-CODE-IN           PIC 9(2).     * Código do registro
    05  WS-PR-SSN-IN.
        10  WS-PR-SSN-3-IN          PIC X(3).     * Primeira parte do SSN
        10  WS-PR-SSN-2-IN          PIC X(2).
        10  WS-PR-SSN-4-IN          PIC X(4).
    05  WS-PR-EMPL-NAME-IN          PIC X(18).    * Nome do empregado
    05  FILLER                      PIC X(20) VALUE SPACES.
    05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99.  * Rendimento deste período
    05  FILLER                      PIC X(14) VALUE SPACES.
    05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).     * Estado civil
    05  WS-PR-EXEMPTIONS-IN         PIC 9(1).     * Número de isenções
    05  FILLER                      PIC X(8) VALUE SPACES.

    * Cabeçalho da primeira linha do relatório
01  WS-HEADING.
    * Texto fixo com "FEDERAL INCOME TAX REGISTER" e número da página

    * Segunda linha de cabeçalho de colunas
01  WS-COLUMN-HEADING-01.
    * Cabeçalhos de coluna: SSN, Marital Status, Earnings, Tax, etc.

    * Terceira linha de cabeçalho (descrições complementares)
01  WS-COLUMN-HEADING-02.
    * Detalhamento dos cabeçalhos da linha anterior

    * Linha de detalhe de empregado para o relatório
01  WS-DETAIL-LINE.
    * Contém SSN, nome, estado civil, isenções, rendimentos e impostos

    * Linha para empregados isentos de imposto
01  WS-TAX-EXEMPT-LINE.
    * Exibe mensagem "TAX EXEMPT" no lugar dos valores de imposto

    * Linha de totais no fim do relatório
01  WS-TOTAL-LINE.
    * Totais gerais para os campos monetários processados

PROCEDURE DIVISION.

A00-MAINLINE-PARA.
    * Abre os arquivos
    OPEN INPUT PAYROLL-FILE-IN
         OUTPUT INCOME-TAX-REPORT-OUT.

    * Inicializa variáveis e imprime cabeçalhos
    PERFORM B10-INIT-PARA.

    * Lê primeiro registro
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.

    * Processa registros até o fim do arquivo
    PERFORM B20-PROCESS-PARA
        UNTIL WS-EOF-SWITCH = "Y".

    * Imprime totais
    PERFORM C20-TOTAL-PARA.

    * Fecha arquivos
    CLOSE PAYROLL-FILE-IN
       INCOME-TAX-REPORT-OUT.

    * Finaliza o programa
    STOP RUN.

B10-INIT-PARA.
    * Inicializa acumuladores e contador de linhas/páginas
    MOVE ZEROS TO WS-LINES-USED
                  WS-EARN-THIS-PER-TOT
                  WS-FED-TAX-AMT-TOT
                  WS-FED-TAX-THIS-PER-TOT.
    MOVE 1 TO WS-PAGE-COUNT.

    * Imprime cabeçalhos
    PERFORM C10-HEADINGS-PARA.

B20-PROCESS-PARA.
    * Calcula valor total de isenções
    MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING 
        WS-EXEMPT-SUB-TOT ROUNDED.

    * Anualiza rendimento
    MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING 
        WS-ANN-EARN.

    * Subtrai isenções
    SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
        WS-EARNINGS ROUNDED.

    * Determina qual tabela de imposto usar
    IF WS-PR-MARITAL-STATUS-IN = 1 THEN
        PERFORM C30-SINGLE-TAX-PARA
    ELSE
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.

    IF WS-PR-MARITAL-STATUS-IN = 2 THEN
        PERFORM C40-MARRIED-TAX-PARA
    ELSE
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.

    IF WS-PR-MARITAL-STATUS-IN = 3 THEN
        PERFORM C60-TAX-EXEMPT-PARA
    END-IF.

    * Atualiza contagem de linhas e acumuladores
    ADD 2 TO WS-LINES-USED.
    ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.

    * Lê próximo registro
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.

C10-HEADINGS-PARA.
    * Imprime cabeçalhos de página
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

C20-TOTAL-PARA.
    * Se página estiver cheia, imprime novo cabeçalho
    IF WS-LINES-USED >= 57 THEN
        PERFORM C10-HEADINGS-PARA
        MOVE ZEROS TO WS-LINES-USED
    END-IF.

    * Prepara linha de totais
    MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
    MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
    MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
    MOVE SPACES TO IT-REPORT-OUT.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
        AFTER ADVANCING 2 LINE.
        
C30-SINGLE-TAX-PARA.
    * Inicializa o índice para busca na tabela de imposto para solteiros
    SET S-INDEX TO 1.
    
    * Procura a faixa de imposto correspondente aos ganhos ajustados
    SEARCH WS-SING-TAX-ROW
        AT END
            * Se nenhuma faixa for encontrada, marca como fim da tabela
            MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-SING-LOW(S-INDEX) AND
             WS-EARNINGS IS <= WS-SING-HIGH(S-INDEX)
            * Se a faixa for encontrada, marca como encontrada
            MOVE "Y" TO WS-ROW-FOUND-SWITCH
    END-SEARCH.

    * Se foi encontrada a faixa na tabela de solteiros
    IF WS-ROW-FOUND THEN
        * Calcula o valor do imposto anual com base na tabela
        COMPUTE WS-ANN-TAX-AMT =
            WS-SING-BASE-AMT(S-INDEX) +
            (WS-EARNINGS - WS-SING-LOW(S-INDEX)) *
            WS-SING-PERCENT(S-INDEX) ROUNDED

        * Calcula o imposto para o período
        COMPUTE WS-PER-TAX-AMT = WS-ANN-TAX-AMT / 26 ROUNDED

        * Soma ao total de impostos anuais e do período
        ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT
        ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT

        * Move os dados calculados para os campos de saída
        MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT
        MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT
        MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT
        MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT
        MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT
        MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT
        MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT
        MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT
        MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT
        MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT
        MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT

        * Escreve a linha de detalhes no relatório
        WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
            AFTER ADVANCING 1 LINE
    END-IF.

C40-MARRIED-TAX-PARA.
    * Inicializa o índice para busca na tabela de imposto para casados
    SET M-INDEX TO 1.

    * Procura a faixa de imposto correspondente aos ganhos ajustados
    SEARCH WS-MARR-TAX-ROW
        AT END
            * Se nenhuma faixa for encontrada, marca como fim da tabela
            MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-MARR-LOW(M-INDEX) AND
             WS-EARNINGS IS <= WS-MARR-HIGH(M-INDEX)
            * Se a faixa for encontrada, marca como encontrada
            MOVE "Y" TO WS-ROW-FOUND-SWITCH
    END-SEARCH.

    * Se foi encontrada a faixa na tabela de casados
    IF WS-ROW-FOUND THEN
        * Calcula o valor do imposto anual com base na tabela
        COMPUTE WS-ANN-TAX-AMT =
            WS-MARR-BASE-AMT(M-INDEX) +
            (WS-EARNINGS - WS-MARR-LOW(M-INDEX)) *
            WS-MARR-PERCENT(M-INDEX) ROUNDED

        * Calcula o imposto para o período
        COMPUTE WS-PER-TAX-AMT = WS-ANN-TAX-AMT / 26 ROUNDED

        * Soma ao total de impostos anuais e do período
        ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT
        ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT

        * Move os dados calculados para os campos de saída
        MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT
        MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT
        MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT
        MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT
        MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT
        MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT
        MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT
        MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT
        MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT
        MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT
        MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT

        * Escreve a linha de detalhes no relatório
        WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
            AFTER ADVANCING 1 LINE
    END-IF.

C60-TAX-EXEMPT-PARA.
    * Preenche os dados para saída de funcionário isento de imposto
    MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT
    MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT
    MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT
    MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT
    MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT
    MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT
    MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT

    * Escreve a linha de isenção no relatório
    WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
        AFTER ADVANCING 1 LINE.
