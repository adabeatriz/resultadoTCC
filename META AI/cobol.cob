IDENTIFICATION DIVISION.
*> Divisão de identificação do programa
 ENVIRONMENT DIVISION.
*> Divisão de ambiente do programa
 INPUT-OUTPUT SECTION.
*> Seção de entrada e saída
 FILE-CONTROL.
*> Controle de arquivos
     SELECT PAYROLL-FILE-IN
*> Arquivo de entrada de folha de pagamento
         ASSIGN TO "TW12C.DAT".
     SELECT INCOME-TAX-REPORT-OUT
*> Arquivo de saída de relatório de imposto de renda
         ASSIGN TO "PROG6_OUT.DAT".
 DATA DIVISION.
*> Divisão de dados
 FILE SECTION.
*> Seção de arquivos
 FD  PAYROLL-FILE-IN
*> Definição do arquivo de entrada de folha de pagamento
         RECORD CONTAINS 80 CHARACTERS
*> Registro contém 80 caracteres
         LABEL RECORDS ARE OMITTED
*> Rótulos de registro são omitidos
         DATA RECORD IS PR-PAYROLL-REC-IN.
*> Registro de dados é PR-PAYROLL-REC-IN
 01  PR-PAYROLL-REC-IN               PIC X(80).
*> Registro de entrada de folha de pagamento

 FD  INCOME-TAX-REPORT-OUT
*> Definição do arquivo de saída de relatório de imposto de renda
         RECORD CONTAINS 132 CHARACTERS
*> Registro contém 132 caracteres
         LABEL RECORDS ARE OMITTED
*> Rótulos de registro são omitidos
         DATA RECORD IS IT-REPORT-OUT.
*> Registro de dados é IT-REPORT-OUT
 01  IT-REPORT-OUT                   PIC X(132).
*> Registro de saída de relatório de imposto de renda

 WORKING-STORAGE SECTION.
*> Seção de armazenamento de trabalho
 01  WS-SWITCHES.
*> Interruptores de trabalho
     05  WS-EOF-SWITCH               PIC X(1). 
*> Interruptor de fim de arquivo

 01  WS-PAGE-LAYOUT.
*> Layout de página
     05  WS-LINES-USED               PIC 9(2).
*> Linhas usadas
     05  WS-PAGE-COUNT               PIC 9(2).
*> Contagem de páginas

 01  WS-CALC-WORK-AREA.
*> Área de trabalho de cálculo
     05  WS-EXEMPT-SUB-TOT           PIC 9(5)V99.
*> Subtotal de isenção
     05  WS-ANN-EARN                 PIC 9(6)V99.
*> Ganhos anuais
     05  WS-EARNINGS                 PIC 9(6)V99.
*> Ganhos
     05  WS-ANN-TAX-AMT              PIC 9(6)V99.
*> Valor do imposto anual
     05  WS-PER-TAX-AMT              PIC 9(6)V99.
*> Valor do imposto por período

 01  WS-TABLE-CONTROLS.
*> Controles de tabela
     05  WS-ROW-FOUND-SWITCH         PIC X(1).
*> Interruptor de linha encontrada
         88  WS-ROW-FOUND            VALUE "Y".
*> Linha encontrada
         88  WS-END-OF-TABLE         VALUE "E".
*> Fim da tabela

 01  WS-TOTALS.
*> Totais
     05  WS-EARN-THIS-PER-TOT        PIC 9(6)V99.
*> Total de ganhos do período
     05  WS-FED-TAX-AMT-TOT         PIC 9(6)V99.
*> Total de imposto federal
     05  WS-FED-TAX-THIS-PER-TOT     PIC 9(6)V99.
*> Total de imposto federal do período

 01  WS-MARR-TAX-DATA.
*> Dados de imposto para casados
     05  FILLER               PIC X(16) VALUE "0000002400000000".
     05  FILLER               PIC X(16) VALUE "0240006600000015".
     05  FILLER               PIC X(16) VALUE "0660010900063018".
     05  FILLER               PIC X(16) VALUE "1090015000140421".
     05  FILLER               PIC X(16) VALUE "1500019200226524".
     05  FILLER               PIC X(16) VALUE "1920023600327328".
     05  FILLER               PIC X(16) VALUE "2360028900450532".
     05  FILLER               PIC X(16) VALUE "2890099999620137".

 01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
*> Tabela de imposto para casados
     05  WS-MARR-TAX-ROW      OCCURS 8 TIMES
*> Linha de imposto para casados
                              INDEXED BY M-INDEX.
*> Índice para a tabela de imposto para casados
         10  WS-MARR-LOW      PIC 9(5).
*> Limite inferior
         10  WS-MARR-HIGH     PIC 9(5).
*> Limite superior
         10  WS-MARR-BASE-AMT PIC 9(4).
*> Valor base
         10  WS-MARR-PERCENT  PIC V99.
*> Percentual

 01  WS-SING-TAX-DATA.
*> Dados de imposto para solteiros
     05  FILLER               PIC X(16) VALUE "0000001420000000".
     05  FILLER               PIC X(16) VALUE "0142003300000015".
     05  FILLER               PIC X(16) VALUE "0330006800028218".
     05  FILLER               PIC X(16) VALUE "0680010200091221".
     05  FILLER               PIC X(16) VALUE "1020014200162626".
     05  FILLER               PIC X(16) VALUE "1420017200266630".
     05  FILLER               PIC X(16) VALUE "1720022500356634".
     05  FILLER               PIC X(16) VALUE "2250099999536839".

 01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
*> Tabela de imposto para solteiros
     05  WS-SING-TAX-ROW      OCCURS 8 TIMES
*> Linha de imposto para solteiros
                              INDEXED BY S-INDEX.
*> Índice para a tabela de imposto para solteiros
         10  WS-SING-LOW      PIC 9(5).
*> Limite inferior
         10  WS-SING-HIGH     PIC 9(5).
*> Limite superior
         10  WS-SING-BASE-AMT PIC 9(4).
*> Valor base
         10  WS-SING-PERCENT  PIC V99.
*> Percentual

 01  WS-PR-PAYROLL-REC-IN.
*> Registro de entrada de folha de pagamento
     05  WS-PR-REC-CODE-IN           PIC 9(2).
*> Código do registro
     05  WS-PR-SSN-IN.
*> Número de segurança social
         10  WS-PR-SSN-3-IN          PIC X(3).
*> Parte 1 do número de segurança social
         10  WS-PR-SSN-2-IN          PIC X(2).
*> Parte 2 do número de segurança social
         10  WS-PR-SSN-4-IN          PIC X(4).
*> Parte 3 do número de segurança social
     05  WS-PR-EMPL-NAME-IN          PIC X(18).
*> Nome do empregado
     05  FILLER                      PIC X(20) VALUE SPACES.
*> Espaços em branco
     05  WS-PR-EARN-THIS-PER-IN      PIC 9(5)V99.
*> Ganhos do período
     05  FILLER                      PIC X(14) VALUE SPACES.
*> Espaços em branco
     05  WS-PR-MARITAL-STATUS-IN     PIC 9(1).
*> Estado civil
     05  WS-PR-EXEMPTIONS-IN         PIC 9(1).
*> Isenções
     05  FILLER                      PIC X(8) VALUE SPACES.
*> Espaços em branco

 01  WS-HEADING.
*> Cabeçalho
     05  FILLER                      PIC X(7) VALUE "FEDERAL".
*> Palavra "FEDERAL"
     05  FILLER                      PIC X(1) VALUE SPACES.
*> Espaço em branco
     05  FILLER                      PIC X(6) VALUE "INCOME".
*> Palavra "INCOME"
     05  FILLER                      PIC X(1) VALUE SPACES.
*> Espaço em branco
     05  FILLER                      PIC X(3) VALUE "TAX".
*> Palavra "TAX"
     05  FILLER                      PIC X(1) VALUE SPACES.
*> Espaço em branco
     05  FILLER                      PIC X(8) VALUE "REGISTER".
*> Palavra "REGISTER"
     05  FILLER                      PIC X(50) VALUE SPACES.
*> Espaços em branco
     05  FILLER                      PIC X(4) VALUE "PAGE".
*> Palavra "PAGE"
     05  FILLER                      PIC X(1) VALUE SPACES.
*> Espaço em branco
     05  WS-PAGE-OUT                 PIC ZZ9.
*> Número da página
     05  FILLER                      PIC X(47) VALUE SPACES.
*> Espaços em branco

 PROCEDURE DIVISION.
*> Divisão de procedimentos
 A00-MAINLINE-PARA.
*> Parágrafo principal
     OPEN INPUT PAYROLL-FILE-IN
*> Abre o arquivo de entrada de folha de pagamento
          OUTPUT INCOME-TAX-REPORT-OUT.
*> Abre o arquivo de saída de relatório de imposto de renda
     PERFORM B10-INIT-PARA.
*> Executa o parágrafo de inicialização
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
*> Lê o registro de entrada de folha de pagamento
         AT END MOVE "Y" TO WS-EOF-SWITCH.
*> Verifica se é o fim do arquivo
     PERFORM B20-PROCESS-PARA
*> Executa o parágrafo de processamento
         UNTIL WS-EOF-SWITCH = "Y".
*> Continua até o fim do arquivo
     PERFORM C20-TOTAL-PARA.
*> Executa o parágrafo de totais
     CLOSE PAYROLL-FILE-IN
*> Fecha o arquivo de entrada de folha de pagamento
        INCOME-TAX-REPORT-OUT.
*> Fecha o arquivo de saída de relatório de imposto de renda
     STOP RUN.
*> Encerra o programa

 B10-INIT-PARA.
*> Parágrafo de inicialização
     MOVE ZEROS TO WS-LINKS-USED
*> Inicializa as variáveis
                   WS-EARN-THIS-PER-TOT
                   WS-FED-TAX-AMT-TOT
                   WS-FED-TAX-THIS-PER-TOT.
     MOVE 1 TO WS-PAGE-COUNT.
*> Inicializa a contagem de páginas
     PERFORM C10-HEADINGS-PARA.
*> Executa o parágrafo de cabeçalhos

 C10-HEADINGS-PARA.
*> Parágrafo de cabeçalhos
     MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
*> Move o número da página para a saída
     WRITE IT-REPORT-OUT FROM WS-HEADING
*> Escreve o cabeçalho
         AFTER ADVANCING PAGE.
*> Avança para a próxima página
     MOVE SPACES TO IT-REPORT-OUT.
*> Limpa a saída
     WRITE IT-REPORT-OUT
*> Escreve uma linha em branco
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
*> Escreve o cabeçalho de coluna 1
         AFTER ADVANCING 1 LINES.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
*> Escreve o cabeçalho de coluna 2
         AFTER ADVANCING 1 LINE.
     ADD 3 TO WS-LINES-USED.
*> Incrementa o número de linhas usadas
     ADD 1 TO WS-PAGE-COUNT.
*> Incrementa a contagem de páginas

 C20-TOTAL-PARA.
*> Parágrafo de totais
     IF WS-LINES-USED >= 57 THEN
*> Verifica se é necessário avançar para a próxima página
         PERFORM C10-HEADINGS-PARA
*> Executa o parágrafo de cabeçalhos
         MOVE ZEROS TO WS-LINES-USED
*> Reinicializa o número de linhas usadas
     END-IF.
     MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
*> Move o total de ganhos do período para a saída
     MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
*> Move o total de imposto federal para a saída
     MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
*> Move o total de imposto federal do período para a saída
     MOVE SPACES TO IT-REPORT-OUT.
*> Limpa a saída
     WRITE IT-REPORT-OUT
*> Escreve uma linha em branco
         AFTER ADVANCING 1 LINE.
     WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
*> Escreve a linha de totais
         AFTER ADVANCING 2 LINE.

 C30-SINGLE-TAX-PARA.
*> Parágrafo de imposto para solteiros
     SET S-INDEX TO 1.
*> Inicializa o índice para a tabela de imposto para solteiros
     SEARCH WS-SING-TAX-ROW
*> Procura a linha de imposto correspondente
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
*> Verifica se a linha foi encontrada
         WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
*> Verifica se o valor de ganhos está dentro do intervalo
                          IS <= WS-SING-HIGH (S-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*> Indica que a linha foi encontrada
     IF WS-ROW-FOUND THEN
*> Verifica se a linha foi encontrada
         COMPUTE WS-ANN-TAX-AMT =
*> Calcula o valor do imposto anual
             WS-SING-BASE-AMT (S-INDEX) +
             WS-SING-PERCENT (S-INDEX) *
             (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
     PERFORM C50-LINE-OUTPUT-PARA.
*> Executa o parágrafo de saída de linha

 C40-MARRIED-TAX-PARA.
*> Parágrafo de imposto para casados
     SET M-INDEX TO 1.
*> Inicializa o índice para a tabela de imposto para casados
     SEARCH WS-MARR-TAX-ROW
*> Procura a linha de imposto correspondente
         AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
*> Verifica se a linha foi encontrada
         WHEN WS-EARNINGS IS >= WS-MARR-LOW  (M-INDEX) AND
*> Verifica se o valor de ganhos está dentro do intervalo
                          IS <= WS-MARR-HIGH (M-INDEX)
         MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*> Indica que a linha foi encontrada
     IF WS-ROW-FOUND THEN
*> Verifica se a linha foi encontrada
         COMPUTE WS-ANN-TAX-AMT =
*> Calcula o valor do imposto anual
             WS-MARR-BASE-AMT (M-INDEX) +
             WS-MARR-PERCENT (M-INDEX) *
             (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
     PERFORM C50-LINE-OUTPUT-PARA.
*> Executa o parágrafo de saída de linha

 C50-LINE-OUTPUT-PARA.
*> Parágrafo de saída de linha
     DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
*> Calcula o valor do imposto por período
     ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
*> Acumula o total de imposto federal
     ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
*> Acumula o total de imposto federal do período
     MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT.
*> Move o número de segurança social para a saída
     MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT.
     MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT.
     MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT.
*> Move o nome do empregado para a saída
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT.
*> Move o estado civil para a saída
     MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT.
*> Move as isenções para a saída
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT.
*> Move os ganhos do período para a saída
     MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT.
*> Move os ganhos anuais para a saída
     MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT.
*> Move os ganhos ajustados para a saída
     MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT.
*> Move o valor do imposto por período para a saída
     MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT.
*> Move o valor do imposto anual para a saída
     IF WS-LINES-USED >= 57 THEN
*> Verifica se é necessário avançar para a próxima página
         PERFORM C10-HEADINGS-PARA
*> Executa o parágrafo de cabeçalhos
         MOVE ZEROS TO WS-LINES-USED
*> Reinicializa o número de linhas usadas
     END-IF.
     MOVE SPACES TO IT-REPORT-OUT.
*> Limpa a saída
     WRITE IT-REPORT-OUT
*> Escreve uma linha em branco
         AFTER
     WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
*> Escreve a linha de detalhe
         AFTER ADVANCING 1 LINE.

 C60-TAX-EXEMPT-PARA.
*> Parágrafo de isenção de imposto
     ADD 2 TO WS-LINES-USED.
*> Incrementa o número de linhas usadas
     MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
*> Move o número de segurança social para a saída
     MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
     MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
     MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
*> Move o nome do empregado para a saída
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
*> Move o estado civil para a saída
     MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
*> Move as isenções para a saída
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
*> Move os ganhos do período para a saída
     MOVE "Y" TO WS-ROW-FOUND-SWITCH.
*> Indica que a linha foi encontrada
     IF WS-ROW-FOUND THEN
*> Verifica se a linha foi encontrada
         MOVE SPACES TO IT-REPORT-OUT.
*> Limpa a saída
         WRITE IT-REPORT-OUT
*> Escreve uma linha em branco
             AFTER ADVANCING 1 LINE.
         WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
*> Escreve a linha de isenção de imposto
             AFTER ADVANCING 1 LINE.