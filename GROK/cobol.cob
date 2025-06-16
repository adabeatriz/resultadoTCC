* Divisão de Identificação: Define o propósito do programa
IDENTIFICATION DIVISION.
* Define o nome do programa
PROGRAM-ID. PAYROLL-TAX-REPORT.

* Divisão de Ambiente: Configura o ambiente de execução
ENVIRONMENT DIVISION.
* Seção de Entrada/Saída: Define os arquivos usados pelo programa
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    * Define o arquivo de entrada PAYROLL-FILE-IN associado ao arquivo físico "TW12C.DAT"
    SELECT PAYROLL-FILE-IN
        ASSIGN TO "TW12C.DAT".
    * Define o arquivo de saída INCOME-TAX-REPORT-OUT associado ao arquivo físico "PROG6_OUT.DAT"
    SELECT INCOME-TAX-REPORT-OUT
        ASSIGN TO "PROG6_OUT.DAT".

* Divisão de Dados: Define a estrutura de dados usada no programa
DATA DIVISION.
* Seção de Arquivos: Define a estrutura dos arquivos de entrada e saída
FILE SECTION.
* Descrição do arquivo de entrada PAYROLL-FILE-IN
FD  PAYROLL-FILE-IN
    * Cada registro tem 80 caracteres
    RECORD CONTAINS 80 CHARACTERS
    * Não há rótulos nos registros
    LABEL RECORDS ARE OMITTED
    * Nome do registro de entrada
    DATA RECORD IS PR-PAYROLL-REC-IN.
* Define o registro de entrada com 80 caracteres
01  PR-PAYROLL-REC-IN               PIC X(80).
* Descrição do arquivo de saída INCOME-TAX-REPORT-OUT
FD  INCOME-TAX-REPORT-OUT
    * Cada registro tem 132 caracteres
    RECORD CONTAINS 132 CHARACTERS
    * Não há rótulos nos registros
    LABEL RECORDS ARE OMITTED
    * Nome do registro de saída
    DATA RECORD IS IT-REPORT-OUT.
* Define o registro de saída com 132 caracteres
01  IT-REPORT-OUT                   PIC X(132).

* Seção de Armazenamento de Trabalho: Define variáveis usadas no programa
WORKING-STORAGE SECTION.
* Define switches de controle
01  WS-SWITCHES.
    * Indicador de fim de arquivo (EOF)
    05  WS-EOF-SWITCH               PIC X(1). 
* Define variáveis para controle de layout de página
01  WS-PAGE-LAYOUT.
    * Contador de linhas usadas na página
    05  WS-LINES-USED               PIC 9(2).
    * Contador de páginas do relatório
    05  WS-PAGE-COUNT               PIC 9(2).
* Define área de trabalho para cálculos
01  WS-CALC-WORK-AREA.
    * Subtotal de isenções
    05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99.
    * Ganhos anuais calculados
    05  WS-ANN-EARN                 PIC 9(6)V99.
    * Ganhos ajustados após isenções
    05  WS-EARNINGS                 PIC 9(6)V99.
    * Valor do imposto anual
    05  WS-ANN-TAX-AMT              PIC 9(6)V99.
    * Valor do imposto por período
    05  WS-PER-TAX-AMT              PIC 9(6)V99.
* Define controles para tabelas de impostos
01  WS-TABLE-CONTROLS.
    * Indicador de linha encontrada na tabela
    05  WS-ROW-FOUND-SWITCH         PIC X(1).
        * Valor "Y" indica que uma linha foi encontrada
        88  WS-ROW-FOUND            VALUE "Y".
        * Valor "E" indica fim da tabela
        88  WS-END-OF-TABLE         VALUE "E".
* Define totais acumulados
01  WS-TOTALS.
    * Total de ganhos para este período
    05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99.
    * Total de imposto federal
    05  WS-FED-TAX-AMT-TOT         PIC 9(6)V99.
    * Total de imposto federal para este período
    05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99.
* Dados da tabela de impostos para casados
01  WS-MARR-TAX-DATA.
    * Dados fixos para a tabela de impostos (casados)
    05  FILLER               PIC X(16) VALUE "0000002400000000".
    05  FILLER               PIC X(16) VALUE "0240006600000015".
    05  FILLER               PIC X(16) VALUE "0660010900063018".
    05  FILLER               PIC X(16) VALUE "1090015000140421".
    05  FILLER               PIC X(16) VALUE "1500019200226524".
    05  FILLER               PIC X(16) VALUE "1920023600327328".
    05  FILLER               PIC X(16) VALUE "2360028900450532".
    05  FILLER               PIC X(16) VALUE "2890099999620137".
* Redefine os dados da tabela de impostos para casados
01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
    * Tabela com 8 linhas, indexada por M-INDEX
    05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
                             INDEXED BY M-INDEX.
        * Limite inferior da faixa de renda
        10  WS-MARR-LOW      PIC 9(5).
        * Limite superior da faixa de renda
        10  WS-MARR-HIGH     PIC 9(5).
        * Valor base do imposto
        10  WS-MARR-BASE-AMT PIC 9(4).
        * Percentual do imposto
        10  WS-MARR-PERCENT  PIC V99.
* Dados da tabela de impostos para solteiros
01  WS-SING-TAX-DATA.
    * Dados fixos para a tabela de impostos (solteiros)
    05  FILLER               PIC X(16) VALUE "0000001420000000".
    05  FILLER               PIC X(16) VALUE "0142003300000015".
    05  FILLER               PIC X(16) VALUE "0330006800028218".
    05  FILLER               PIC X(16) VALUE "0680010200091221".
    05  FILLER               PIC X(16) VALUE "1020014200162626".
    05  FILLER               PIC X(16) VALUE "1420017200266630".
    05  FILLER               PIC X(16) VALUE "1720022500356634".
    05  FILLER               PIC X(16) VALUE "2250099999536839".
* Redefine os dados da tabela de impostos para solteiros
01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
    * Tabela com 8 linhas, indexada por S-INDEX
    05  WS-SING-TAX-ROW      OCCURS 8 TIMES
                             INDEXED BY S-INDEX.
        * Limite inferior da faixa de renda
        10  WS-SING-LOW      PIC 9(5).
        * Limite superior da faixa de renda
        10  WS-SING-HIGH     PIC 9(5).
        * Valor base do imposto
        10  WS-SING-BASE-AMT PIC 9(4).
        * Percentual do imposto
        10  WS-SING-PERCENT  PIC V99.
* Estrutura do registro de entrada detalhado
01  WS-PR-PAYROLL-REC-IN.
    * Código do registro
    05  WS-PR-REC-CODE-IN           PIC 9(2).
    * Número de Seguro Social (SSN)
    05  WS-PR-SSN-IN.
        * Primeiros 3 dígitos do SSN
        10  WS-PR-SSN-3-IN          PIC X(3).
        * 2 dígitos intermediários do SSN
        10  WS-PR-SSN-2-IN          PIC X(2).
        * Últimos 4 dígitos do SSN
        10  WS-PR-SSN-4-IN          PIC X(4).
    * Nome do empregado
    05  WS-PR-EMPL-NAME-IN          PIC X(18).
    * Espaço reservado (não utilizado)
    05  FILLER                      PIC X(20) VALUE SPACES.
    * Ganhos para este período
    05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99.
    * Espaço reservado (não utilizado)
    05  FILLER                      PIC X(14) VALUE SPACES.
    * Estado civil (1=solteiro, 2=casado, 3=isento)
    05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).
    * Número de isenções
    05  WS-PR-EXEMPTIONS-IN         PIC 9(1).
    * Espaço reservado (não utilizado)
    05  FILLER                      PIC X(8) VALUE SPACES.
* Cabeçalho do relatório
01  WS-HEADING.
    * Texto "FEDERAL"
    05  FILLER                      PIC X(7) VALUE "FEDERAL".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "INCOME"
    05  FILLER                      PIC X(6) VALUE "INCOME".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "TAX"
    05  FILLER                      PIC X(3) VALUE "TAX".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "REGISTER"
    05  FILLER                      PIC X(8) VALUE "REGISTER".
    * Espaço reservado
    05  FILLER                      PIC X(50) VALUE SPACES.
    * Texto "PAGE"
    05  FILLER                      PIC X(4) VALUE "PAGE".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Número da página
    05  WS-PAGE-OUT                 PIC ZZ9.
    * Espaço reservado
    05  FILLER                      PIC X(47) VALUE SPACES.
* Primeira linha do cabeçalho de colunas
01  WS-COLUMN-HEADING-01.
    * Texto "SOCIAL"
    05  FILLER                      PIC X(6) VALUE "SOCIAL".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "SEC."
    05  FILLER                      PIC X(4) VALUE "SEC.".
    * Espaço reservado
    05  FILLER                      PIC X(22) VALUE SPACES.
    * Indicador de estado civil (M/S)
    05  FILLER                      PIC X(1) VALUE "M".
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Indicador de isenções (WH)
    05  FILLER                      PIC X(2) VALUE "WH".
    * Espaço em branco
    05  FILLER                      PIC X(3) VALUE SPACES.
    * Texto "EARNINGS"
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Texto "ANNUALIZED"
    05  FILLER                      PIC X(10) VALUE "ANNUALIZED".
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Texto "ADJUSTED"
    05  FILLER                      PIC X(8) VALUE "ADJUSTED".
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Texto "FEDERAL"
    05  FILLER                      PIC X(7) VALUE "FEDERAL".
    * Espaço em branco
    05  FILLER                      PIC X(5) VALUE SPACES.
    * Texto "FED"
    05  FILLER                      PIC X(3) VALUE "FED".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "TAX"
    05  FILLER                      PIC X(3) VALUE "TAX".
    * Espaço reservado
    05  FILLER                      PIC X(36) VALUE SPACES.
* Segunda linha do cabeçalho de colunas
01  WS-COLUMN-HEADING-02.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Texto "NUMBER"
    05  FILLER                      PIC X(6) VALUE "NUMBER".
    * Espaço em branco
    05  FILLER                      PIC X(7) VALUE SPACES.
    * Texto "EMPLOYEE"
    05  FILLER                      PIC X(8) VALUE "EMPLOYEE".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "NAME"
    05  FILLER                      PIC X(4) VALUE "NAME".
    * Espaço em branco
    05  FILLER                      PIC X(5) VALUE SPACES.
    * Indicador de estado civil (S)
    05  FILLER                      PIC X(1) VALUE "S".
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Indicador de isenções (EX)
    05  FILLER                      PIC X(2) VALUE "EX".
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Texto "THIS"
    05  FILLER                      PIC X(4) VALUE "THIS".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "PER."
    05  FILLER                      PIC X(4) VALUE "PER.".
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Texto "EARNINGS"
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Texto "EARNINGS"
    05  FILLER                      PIC X(8) VALUE "EARNINGS".
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Texto "TAX"
    05  FILLER                      PIC X(3) VALUE "TAX".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "AMT."
    05  FILLER                      PIC X(4) VALUE "AMT.".
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Texto "THIS"
    05  FILLER                      PIC X(4) VALUE "THIS".
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Texto "PERIOD"
    05  FILLER                      PIC X(6) VALUE "PERIOD".
    * Espaço reservado
    05  FILLER                      PIC X(34) VALUE SPACES.
* Linha de detalhes do relatório
01  WS-DETAIL-LINE.
    * Número de Seguro Social formatado (XXX-XX-XXXX)
    05  WS-SSN-OUT.
        * Primeiros 3 dígitos
        10  WS-SSN-3-OUT            PIC X(3).
        * Hífen
        10  FILLER                  PIC X(1) VALUE "-".
        * 2 dígitos intermediários
        10  WS-SSN-2-OUT            PIC X(2).
        * Hífen
        10  FILLER                  PIC X(1) VALUE "-".
        * Últimos 4 dígitos
        10  WS-SSN-4-OUT            PIC X(4).
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Nome do empregado
    05  WS-EMPL-NAME-OUT            PIC X(18).
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Estado civil
    05  WS-MARITAL-STATUS-OUT       PIC 9.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Número de isenções
    05  WS-WITHHOLD-EXEMPT-OUT      PIC 99.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Ganhos deste período
    05  WS-EARN-THIS-PER-OUT        PIC ZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Ganhos anualizados
    05  WS-ANN-EARN-OUT             PIC ZZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Ganhos ajustados
    05  WS-ADJ-EARN-OUT             PIC ZZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Valor do imposto federal
    05  WS-FED-TAX-AMT-OUT          PIC ZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(4) VALUE SPACES.
    * Imposto federal para este período
    05  WS-FED-TAX-THIS-PER-OUT     PIC Z,ZZZ.99.
    * Espaço reservado
    05  FILLER                      PIC X(36) VALUE SPACES.
* Linha para registros isentos de imposto
01  WS-TAX-EXEMPT-LINE.
    * Número de Seguro Social formatado (XXX-XX-XXXX)
    05  WS-TE-SSN-OUT.
        * Primeiros 3 dígitos
        10  WS-TE-SSN-3-OUT         PIC X(3).
        * Hífen
        10  FILLER                  PIC X(1) VALUE "-".
        * 2 dígitos intermediários
        10  WS-TE-SSN-2-OUT         PIC X(2).
        * Hífen
        10  FILLER                  PIC X(1) VALUE "-".
        * Últimos 4 dígitos
        10  WS-TE-SSN-4-OUT         PIC X(4).
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Nome do empregado
    05  WS-TE-EMPL-NAME-OUT         PIC X(18).
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Estado civil
    05  WS-TE-MARITAL-STATUS-OUT    PIC 9.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Número de isenções
    05  WS-TE-WITHHOLD-EXEMPT-OUT   PIC 99.
    * Espaço em branco
    05  FILLER                      PIC X(2) VALUE SPACES.
    * Ganhos deste período
    05  WS-TE-EARN-THIS-PER-OUT     PIC ZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(3) VALUE SPACES.
    * Indicador de linha
    05  FILLER                      PIC X(3) VALUE "-  ".
    * Texto "T A X"
    05  FILLER                      PIC X(7) VALUE "  T A X".
    * Espaço em branco
    05  FILLER                      PIC X(6) VALUE SPACES.
    * Texto "E X E M P T"
    05  FILLER                      PIC X(13) VALUE "E X E M P T  ".
    * Indicador de linha
    05  FILLER                      PIC X(3) VALUE "  -".
* Linha de totais do relatório
01  WS-TOTAL-LINE.
    * Espaço reservado
    05  FILLER                      PIC X(20) VALUE SPACES.
    * Texto "T O T A L S :"
    05  FILLER                      PIC X(13) VALUE "T O T A L S :".
    * Espaço em branco
    05  FILLER                      PIC X(6) VALUE SPACES.
    * Total de ganhos deste período
    05  WS-TOT-EARN-THIS-PER-OUT    PIC ZZZ,ZZZ.99.
    * Espaço reservado
    05  FILLER                      PIC X(25) VALUE SPACES.
    * Total de imposto federal
    05  WS-TOT-FED-TAX-AMT-OUT      PIC ZZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(3) VALUE SPACES.
    * Total de imposto federal para este período
    05  WS-TOT-FED-TAX-THIS-PER-OUT PIC ZZ,ZZZ.99.
    * Espaço em branco
    05  FILLER                      PIC X(1) VALUE SPACES.
    * Indicador de final de relatório
    05  FILLER                      PIC X(1) VALUE "*".
    * Espaço reservado
    05  FILLER                      PIC X(34) VALUE SPACES.

* Divisão de Procedimentos: Contém a lógica do programa
PROCEDURE DIVISION.
* Parágrafo principal: Controla o fluxo geral do programa
A00-MAINLINE-PARA.
    * Abre o arquivo de entrada e saída
    OPEN INPUT PAYROLL-FILE-IN
         OUTPUT INCOME-TAX-REPORT-OUT.
    * Executa a inicialização
    PERFORM B10-INIT-PARA.
    * Lê o primeiro registro do arquivo de entrada
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.
    * Processa os registros até o fim do arquivo
    PERFORM B20-PROCESS-PARA
        UNTIL WS-EOF-SWITCH = "Y".
    * Gera a linha de totais
    PERFORM C20-TOTAL-PARA.
    * Fecha os arquivos
    CLOSE PAYROLL-FILE-IN
       INCOME-TAX-REPORT-OUT.
    * Encerra o programa
    STOP RUN.

* Parágrafo de inicialização: Configura variáveis iniciais e cabeçalhos
B10-INIT-PARA.
    * Zera contadores e totais
    MOVE ZEROS TO WS-LINES-USED
                  WS-EARN-THIS-PER-TOT
                  WS-FED-TAX-AMT-TOT
                  WS-FED-TAX-THIS-PER-TOT.
    * Inicializa o contador de páginas
    MOVE 1 TO WS-PAGE-COUNT.
    * Escreve os cabeçalhos do relatório
    PERFORM C10-HEADINGS-PARA.

* Parágrafo de processamento: Processa cada registro de entrada
B20-PROCESS-PARA.
    * Calcula o subtotal de isenções (número de isenções * 1000)
    MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING 
        WS-EXEMPT-SUB-TOT ROUNDED.
    * Calcula os ganhos anualizados (ganhos do período * 26)
    MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING 
        WS-ANN-EARN.
    * Calcula os ganhos ajustados (ganhos anualizados - isenções)
    SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
        WS-EARNINGS ROUNDED.
    
    * Verifica se o empregado é solteiro
    IF WS-PR-MARITAL-STATUS-IN = 1 THEN
        PERFORM C30-SINGLE-TAX-PARA
    ELSE
        * Define fim da tabela se não for solteiro
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.

    * Verifica se o empregado é casado
    IF WS-PR-MARITAL-STATUS-IN = 2 THEN
        PERFORM C40-MARRIED-TAX-PARA
    ELSE
        * Define fim da tabela se não for casado
        MOVE "E" TO WS-ROW-FOUND-SWITCH
    END-IF.

    * Verifica se o empregado é isento de impostos
    IF WS-PR-MARITAL-STATUS-IN = 3 THEN
        PERFORM C60-TAX-EXEMPT-PARA
    END-IF.

    * Incrementa o contador de linhas usadas
    ADD 2 TO WS-LINES-USED.
    * Acumula os ganhos do período no total
    ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.
    * Lê o próximo registro
    READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
        AT END MOVE "Y" TO WS-EOF-SWITCH.

* Parágrafo de cabeçalhos: Escreve os cabeçalhos do relatório
C10-HEADINGS-PARA.
    * Move o número da página para o campo de saída
    MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
    * Escreve o cabeçalho principal
    WRITE IT-REPORT-OUT FROM WS-HEADING
        AFTER ADVANCING PAGE.
    * Insere uma linha em branco
    MOVE SPACES TO IT-REPORT-OUT.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve a primeira linha de cabeçalho de colunas
    WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
        AFTER ADVANCING 1 LINES.
    * Escreve a segunda linha de cabeçalho de colunas
    WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
        AFTER ADVANCING 1 LINE.
    * Incrementa o contador de linhas usadas
    ADD 3 TO WS-LINES-USED.
    * Incrementa o contador de páginas
    ADD 1 TO WS-PAGE-COUNT.

* Parágrafo de totais: Escreve a linha de totais no relatório
C20-TOTAL-PARA.
    * Verifica se a página está cheia (57 linhas ou mais)
    IF WS-LINES-USED >= 57 THEN
        PERFORM C10-HEADINGS-PARA
        MOVE ZEROS TO WS-LINES-USED
    END-IF.
    * Move os totais para os campos de saída
    MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
    MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
    MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
    * Insere uma linha em branco
    MOVE SPACES TO IT-REPORT-OUT.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve a linha de totais
    WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
        AFTER ADVANCING 2 LINE.

* Parágrafo de cálculo de imposto para solteiros
C30-SINGLE-TAX-PARA.
    * Inicializa o índice da tabela
    SET S-INDEX TO 1.
    * Pesquisa a faixa de imposto na tabela de solteiros
    SEARCH WS-SING-TAX-ROW
        AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
                         IS <= WS-SING-HIGH (S-INDEX)
        MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Se uma faixa foi encontrada, calcula o imposto
    IF WS-ROW-FOUND THEN
        COMPUTE WS-ANN-TAX-AMT =
            WS-SING-BASE-AMT (S-INDEX) +
            WS-SING-PERCENT (S-INDEX) *
            (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
    * Escreve a linha de detalhes
    PERFORM C50-LINE-OUTPUT-PARA.

* Parágrafo de cálculo de imposto para casados
C40-MARRIED-TAX-PARA.
    * Inicializa o índice da tabela
    SET M-INDEX TO 1.
    * Pesquisa a faixa de imposto na tabela de casados
    SEARCH WS-MARR-TAX-ROW
        AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
        WHEN WS-EARNINGS IS >= WS-MARR-LOW (M-INDEX) AND
                         IS <= WS-MARR-HIGH (M-INDEX)
        MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Se uma faixa foi encontrada, calcula o imposto
    IF WS-ROW-FOUND THEN
        COMPUTE WS-ANN-TAX-AMT =
            WS-MARR-BASE-AMT (M-INDEX) +
            WS-MARR-PERCENT (M-INDEX) *
            (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
    * Escreve a linha de detalhes
    PERFORM C50-LINE-OUTPUT-PARA.

* Parágrafo de saída de linha: Escreve uma linha de detalhes no relatório
C50-LINE-OUTPUT-PARA.
    * Calcula o imposto por período (imposto anual / 26)
    DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
    * Acumula o imposto anual no total
    ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
    * Acumula o imposto do período no total
    ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
    * Move os dados do registro de entrada para os campos de saída
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
    * Verifica se a página está cheia
    IF WS-LINES-USED >= 57 THEN
        PERFORM C10-HEADINGS-PARA
        MOVE ZEROS TO WS-LINES-USED
    END-IF.
    * Insere uma linha em branco
    MOVE SPACES TO IT-REPORT-OUT.
    WRITE IT-REPORT-OUT
        AFTER ADVANCING 1 LINE.
    * Escreve a linha de detalhes
    WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
        AFTER ADVANCING 1 LINE.           

* Parágrafo para registros isentos de imposto
C60-TAX-EXEMPT-PARA.
    * Incrementa o contador de linhas usadas
    ADD 2 TO WS-LINES-USED.
    * Move os dados do registro de entrada para os campos de saída
    MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
    MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
    MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
    MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
    MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
    MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
    MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
    * Define que a linha foi encontrada
    MOVE "Y" TO WS-ROW-FOUND-SWITCH.
    * Se a linha foi encontrada, escreve no relatório
    IF WS-ROW-FOUND THEN
        MOVE SPACES TO IT-REPORT-OUT.
        WRITE IT-REPORT-OUT
            AFTER ADVANCING 1 LINE.
        * Escreve a linha de isenção
        WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
            AFTER ADVANCING 1 LINE.