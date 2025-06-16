 IDENTIFICATION DIVISION.
* Este é o cabeçalho de identificação do programa.
 ENVIRONMENT DIVISION.
* Esta seção descreve o ambiente no qual o programa será executado.
 INPUT-OUTPUT SECTION.
* Esta seção define os arquivos de entrada e saída.
 FILE-CONTROL.
* Esta cláusula associa nomes de arquivos internos a nomes de arquivos externos.
     SELECT PAYROLL-FILE-IN
* Define o arquivo de entrada para os dados da folha de pagamento.
         ASSIGN TO "TW12C.DAT".
* Associa o arquivo lógico PAYROLL-FILE-IN ao arquivo físico "TW12C.DAT".
     SELECT INCOME-TAX-REPORT-OUT
* Define o arquivo de saída para o relatório de imposto de renda.
         ASSIGN TO "PROG6_OUT.DAT".
* Associa o arquivo lógico INCOME-TAX-REPORT-OUT ao arquivo físico "PROG6_OUT.DAT".
 DATA DIVISION.
* Esta seção descreve as estruturas de dados usadas pelo programa.
 FILE SECTION.
* Esta seção descreve os registros dos arquivos.
 FD  PAYROLL-FILE-IN
* Descrição do arquivo de entrada da folha de pagamento.
         RECORD CONTAINS 80 CHARACTERS
* Cada registro do arquivo contém 80 caracteres.
         LABEL RECORDS ARE OMITTED
* Não há registros de rótulo (labels) no arquivo.
         DATA RECORD IS PR-PAYROLL-REC-IN.
* O nome do registro de dados é PR-PAYROLL-REC-IN.
 01  PR-PAYROLL-REC-IN             PIC X(80).
* Define o layout do registro de entrada da folha de pagamento como uma string de 80 caracteres.
 FD  INCOME-TAX-REPORT-OUT
* Descrição do arquivo de saída do relatório de imposto de renda.
         RECORD CONTAINS 132 CHARACTERS
* Cada registro do arquivo contém 132 caracteres.
         LABEL RECORDS ARE OMITTED
* Não há registros de rótulo (labels) no arquivo.
         DATA RECORD IS IT-REPORT-OUT.
* O nome do registro de dados é IT-REPORT-OUT.
 01  IT-REPORT-OUT                 PIC X(132).
* Define o layout do registro de saída do relatório como uma string de 132 caracteres.
 WORKING-STORAGE SECTION.
* Esta seção define variáveis e áreas de trabalho internas do programa.
 01  WS-SWITCHES.
* Grupo para armazenar chaves de controle (flags).
     05  WS-EOF-SWITCH             PIC X(1).
* Chave de Fim de Arquivo (End Of File) para o arquivo de entrada.
 01  WS-PAGE-LAYOUT.
* Grupo para variáveis relacionadas ao layout da página do relatório.
     05  WS-LINES-USED             PIC 9(2).
* Contador de linhas usadas na página atual.
     05  WS-PAGE-COUNT             PIC 9(2).
* Contador do número da página.
 01  WS-CALC-WORK-AREA.
* Grupo para variáveis usadas em cálculos.
     05  WS-EXEMPT-SUB-TOT         PIC 9(5)V99.
* Subtotal de isenções (formato 99999.99).
     05  WS-ANN-EARN               PIC 9(6)V99.
* Ganhos anuais (formato 999999.99).
     05  WS-EARNINGS               PIC 9(6)V99.
* Ganhos ajustados para cálculo (formato 999999.99).
     05  WS-ANN-TAX-AMT            PIC 9(6)V99.
* Valor anual do imposto (formato 999999.99).
     05  WS-PER-TAX-AMT            PIC 9(6)V99.
* Valor do imposto por período (formato 999999.99).
 01  WS-TABLE-CONTROLS.
* Grupo para variáveis de controle de tabela.
     05  WS-ROW-FOUND-SWITCH       PIC X(1).
* Chave para indicar se uma linha foi encontrada na tabela de impostos.
         88  WS-ROW-FOUND          VALUE "Y".
* Valor 'Y' indica que a linha foi encontrada.
         88  WS-END-OF-TABLE       VALUE "E".
* Valor 'E' indica fim da tabela.
 01  WS-TOTALS.
* Grupo para armazenar os totais do relatório.
     05  WS-EARN-THIS-PER-TOT      PIC 9(6)V99.
* Total de ganhos neste período (formato 999999.99).
     05  WS-FED-TAX-AMT-TOT        PIC 9(6)V99.
* Total do imposto federal anual (formato 999999.99).
     05  WS-FED-TAX-THIS-PER-TOT   PIC 9(6)V99.
* Total do imposto federal neste período (formato 999999.99).
 01  WS-MARR-TAX-DATA.
* Dados brutos para a tabela de imposto de renda para casados.
     05  FILLER                    PIC X(16) VALUE "0000002400000000".
* Faixa 1: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "0240006600000015".
* Faixa 2: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "0660010900063018".
* Faixa 3: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1090015000140421".
* Faixa 4: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1500019200226524".
* Faixa 5: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1920023600327328".
* Faixa 6: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "2360028900450532".
* Faixa 7: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "2890099999620137".
* Faixa 8: Limite inferior, superior, base e percentual.
 01  WS-MARR-TAX-TABLE REDEFINES WS-MARR-TAX-DATA.
* Redefine a área de dados brutos como uma tabela.
     05  WS-MARR-TAX-ROW           OCCURS 8 TIMES
* Define 8 ocorrências (linhas) na tabela.
                                   INDEXED BY M-INDEX.
* Permite acesso às linhas da tabela usando o índice M-INDEX.
         10  WS-MARR-LOW           PIC 9(5).
* Limite inferior da faixa de renda para casados.
         10  WS-MARR-HIGH          PIC 9(5).
* Limite superior da faixa de renda para casados.
         10  WS-MARR-BASE-AMT      PIC 9(4).
* Valor base do imposto para esta faixa para casados.
         10  WS-MARR-PERCENT       PIC V99.
* Percentual de imposto para esta faixa para casados (implica 0.NN).
 01  WS-SING-TAX-DATA.
* Dados brutos para a tabela de imposto de renda para solteiros.
     05  FILLER                    PIC X(16) VALUE "0000001420000000".
* Faixa 1: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "0142003300000015".
* Faixa 2: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "0330006800028218".
* Faixa 3: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "0680010200091221".
* Faixa 4: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1020014200162626".
* Faixa 5: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1420017200266630".
* Faixa 6: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "1720022500356634".
* Faixa 7: Limite inferior, superior, base e percentual.
     05  FILLER                    PIC X(16) VALUE "2250099999536839".
* Faixa 8: Limite inferior, superior, base e percentual.
 01  WS-SING-TAX-TABLE REDEFINES WS-SING-TAX-DATA.
* Redefine a área de dados brutos como uma tabela.
     05  WS-SING-TAX-ROW           OCCURS 8 TIMES
* Define 8 ocorrências (linhas) na tabela.
                                   INDEXED BY S-INDEX.
* Permite acesso às linhas da tabela usando o índice S-INDEX.
         10  WS-SING-LOW           PIC 9(5).
* Limite inferior da faixa de renda para solteiros.
         10  WS-SING-HIGH          PIC 9(5).
* Limite superior da faixa de renda para solteiros.
         10  WS-SING-BASE-AMT      PIC 9(4).
* Valor base do imposto para esta faixa para solteiros.
         10  WS-SING-PERCENT       PIC V99.
* Percentual de imposto para esta faixa para solteiros (implica 0.NN).
 01  WS-PR-PAYROLL-REC-IN.
* Layout do registro de entrada da folha de pagamento.
     05  WS-PR-REC-CODE-IN         PIC 9(2).
* Código do registro.
     05  WS-PR-SSN-IN.
* Número do Seguro Social (SSN).
         10  WS-PR-SSN-3-IN        PIC X(3).
* Primeiros 3 dígitos do SSN.
         10  WS-PR-SSN-2-IN        PIC X(2).
* Próximos 2 dígitos do SSN.
         10  WS-PR-SSN-4-IN        PIC X(4).
* Últimos 4 dígitos do SSN.
     05  WS-PR-EMPL-NAME-IN        PIC X(18).
* Nome do empregado.
     05  FILLER                    PIC X(20) VALUE SPACES.
* Preenchimento com espaços.
     05  WS-PR-EARN-THIS-PER-IN    PIC 9(5)V99.
* Ganhos deste período (formato 99999.99).
     05  FILLER                    PIC X(14) VALUE SPACES.
* Preenchimento com espaços.
     05  WS-PR-MARITAL-STATUS-IN   PIC 9(1).
* Status civil (1 para solteiro, 2 para casado, 3 para isento).
     05  WS-PR-EXEMPTIONS-IN       PIC 9(1).
* Número de isenções.
     05  FILLER                    PIC X(8) VALUE SPACES.
* Preenchimento com espaços.
 01  WS-HEADING.
* Layout para o cabeçalho do relatório.
     05  FILLER                    PIC X(7) VALUE "FEDERAL".
* Literal "FEDERAL".
     05  FILLER                    PIC X(1) VALUE SPACES.
* Espaço.
     05  FILLER                    PIC X(6) VALUE "INCOME".
* Literal "INCOME".
     05  FILLER                    PIC X(1) VALUE SPACES.
* Espaço.
     05  FILLER                    PIC X(3) VALUE "TAX".
* Literal "TAX".
     05  FILLER                    PIC X(1) VALUE SPACES.
* Espaço.
     05  FILLER                    PIC X(8) VALUE "REGISTER".
* Literal "REGISTER".
     05  FILLER                    PIC X(50) VALUE SPACES.
* Espaços de preenchimento.
     05  FILLER                    PIC X(4) VALUE "PAGE".
* Literal "PAGE".
     05  FILLER                    PIC X(1) VALUE SPACES.
* Espaço.
     05  WS-PAGE-OUT               PIC ZZ9.
* Número da página formatado (ex: "  1", " 10").
     05  FILLER                    PIC X(47) VALUE SPACES.
* Espaços de preenchimento.
 01  WS-COLUMN-HEADING-01.
* Layout para a primeira linha do cabeçalho das colunas.
     05  FILLER                    PIC X(6) VALUE "SOCIAL".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "SEC.".
     05  FILLER                    PIC X(22) VALUE SPACES.
     05  FILLER                    PIC X(1) VALUE "M".
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(2) VALUE "WH".
     05  FILLER                    PIC X(3) VALUE SPACES.
     05  FILLER                    PIC X(8) VALUE "EARNINGS".
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(10) VALUE "ANNUALIZED".
     05  FILLER                    PIC X(4) VALUE SPACES.
     05  FILLER                    PIC X(8) VALUE "ADJUSTED".
     05  FILLER                    PIC X(4) VALUE SPACES.
     05  FILLER                    PIC X(7) VALUE "FEDERAL".
     05  FILLER                    PIC X(5) VALUE SPACES.
     05  FILLER                    PIC X(3) VALUE "FED".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(3) VALUE "TAX".
     05  FILLER                    PIC X(36) VALUE SPACES.
 01  WS-COLUMN-HEADING-02.
* Layout para a segunda linha do cabeçalho das colunas.
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(6) VALUE "NUMBER".
     05  FILLER                    PIC X(7) VALUE SPACES.
     05  FILLER                    PIC X(8) VALUE "EMPLOYEE".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "NAME".
     05  FILLER                    PIC X(5) VALUE SPACES.
     05  FILLER                    PIC X(1) VALUE "S".
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(2) VALUE "EX".
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "THIS".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "PER.".
     05  FILLER                    PIC X(4) VALUE SPACES.
     05  FILLER                    PIC X(8) VALUE "EARNINGS".
     05  FILLER                    PIC X(4) VALUE SPACES.
     05  FILLER                    PIC X(8) VALUE "EARNINGS".
     05  FILLER                    PIC X(4) VALUE SPACES.
     05  FILLER                    PIC X(3) VALUE "TAX".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "AMT.".
     05  FILLER                    PIC X(2) VALUE SPACES.
     05  FILLER                    PIC X(4) VALUE "THIS".
     05  FILLER                    PIC X(1) VALUE SPACES.
     05  FILLER                    PIC X(6) VALUE "PERIOD".
     05  FILLER                    PIC X(34) VALUE SPACES.
 01  WS-DETAIL-LINE.
* Layout para a linha de detalhe do relatório.
     05  WS-SSN-OUT.
* Número do Seguro Social de saída.
         10  WS-SSN-3-OUT          PIC X(3).
* Primeiros 3 dígitos do SSN.
         10  FILLER                PIC X(1) VALUE "-".
* Hífen separador.
         10  WS-SSN-2-OUT          PIC X(2).
* Próximos 2 dígitos do SSN.
         10  FILLER                PIC X(1) VALUE "-".
* Hífen separador.
         10  WS-SSN-4-OUT          PIC X(4).
* Últimos 4 dígitos do SSN.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-EMPL-NAME-OUT          PIC X(18).
* Nome do empregado de saída.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-MARITAL-STATUS-OUT     PIC 9.
* Status civil de saída.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-WITHHOLD-EXEMPT-OUT    PIC 99.
* Isenções de retenção de saída.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-EARN-THIS-PER-OUT      PIC ZZ,ZZZ.99.
* Ganhos deste período formatados (ex: "12,345.67").
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-ANN-EARN-OUT           PIC ZZZ,ZZZ.99.
* Ganhos anuais formatados.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-ADJ-EARN-OUT           PIC ZZZ,ZZZ.99.
* Ganhos ajustados formatados.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-FED-TAX-AMT-OUT        PIC ZZ,ZZZ.99.
* Valor do imposto federal anual formatado.
     05  FILLER                    PIC X(4) VALUE SPACES.
* Espaços.
     05  WS-FED-TAX-THIS-PER-OUT   PIC Z,ZZZ.99.
* Valor do imposto federal deste período formatado.
     05  FILLER                    PIC X(36) VALUE SPACES.
* Espaços de preenchimento.
 01  WS-TAX-EXEMPT-LINE.
* Layout para a linha de isenção de imposto.
     05  WS-TE-SSN-OUT.
* SSN para linha de isenção.
         10  WS-TE-SSN-3-OUT       PIC X(3).
* Primeiros 3 dígitos do SSN.
         10  FILLER                PIC X(1) VALUE "-".
* Hífen separador.
         10  WS-TE-SSN-2-OUT       PIC X(2).
* Próximos 2 dígitos do SSN.
         10  FILLER                PIC X(1) VALUE "-".
* Hífen separador.
         10  WS-TE-SSN-4-OUT       PIC X(4).
* Últimos 4 dígitos do SSN.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-TE-EMPL-NAME-OUT       PIC X(18).
* Nome do empregado para linha de isenção.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-TE-MARITAL-STATUS-OUT  PIC 9.
* Status civil para linha de isenção.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-TE-WITHHOLD-EXEMPT-OUT PIC 99.
* Isenções de retenção para linha de isenção.
     05  FILLER                    PIC X(2) VALUE SPACES.
* Espaços.
     05  WS-TE-EARN-THIS-PER-OUT   PIC ZZ,ZZZ.99.
* Ganhos deste período formatados para linha de isenção.
     05  FILLER                    PIC X(3) VALUE SPACES.
* Espaços.
     05  FILLER                    PIC X(3) VALUE "-  ".
* Caracteres de preenchimento.
     05  FILLER                    PIC X(7) VALUE "  T A X".
* Literal "TAX".
     05  FILLER                    PIC X(6) VALUE SPACES.
* Espaços.
     05  FILLER                    PIC X(13) VALUE "E X E M P T  ".
* Literal "EXEMPT".
     05  FILLER                    PIC X(3) VALUE "  -".
* Caracteres de preenchimento.
 01  WS-TOTAL-LINE.
* Layout para a linha de totais do relatório.
     05  FILLER                    PIC X(20) VALUE SPACES.
* Espaços.
     05  FILLER                    PIC X(13) VALUE "T O T A L S :".
* Literal "T O T A L S :".
     05  FILLER                    PIC X(6) VALUE  SPACES.
* Espaços.
     05  WS-TOT-EARN-THIS-PER-OUT  PIC ZZZ,ZZZ.99.
* Total de ganhos deste período formatado.
     05  FILLER                    PIC X(25) VALUE SPACES.
* Espaços.
     05  WS-TOT-FED-TAX-AMT-OUT    PIC ZZZ,ZZZ.99.
* Total do imposto federal anual formatado.
     05  FILLER                    PIC X(3) VALUE SPACES.
* Espaços.
     05  WS-TOT-FED-TAX-THIS-PER-OUT PIC ZZ,ZZZ.99.
* Total do imposto federal deste período formatado.
     05  FILLER                    PIC X(1) VALUE SPACES.
* Espaço.
     05  FILLER                    PIC X(1) VALUE "*".
* Asterisco (indicando total).
     05  FILLER                    PIC X(34) VALUE SPACES.
* Espaços de preenchimento.
 PROCEDURE DIVISION.
* Esta seção contém a lógica de processamento do programa.
 A00-MAINLINE-PARA.
* Parágrafo principal do programa.
     OPEN INPUT PAYROLL-FILE-IN
* Abre o arquivo de entrada da folha de pagamento.
          OUTPUT INCOME-TAX-REPORT-OUT.
* Abre o arquivo de saída do relatório de imposto de renda.
     PERFORM B10-INIT-PARA.
* Executa o parágrafo de inicialização.
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
* Lê o primeiro registro do arquivo de folha de pagamento para a área de trabalho.
          AT END MOVE "Y" TO WS-EOF-SWITCH.
* Se atingir o fim do arquivo, move "Y" para a chave de EOF.
     PERFORM B20-PROCESS-PARA
* Executa o parágrafo de processamento repetidamente.
          UNTIL WS-EOF-SWITCH = "Y".
* Continua enquanto não for o fim do arquivo.
     PERFORM C20-TOTAL-PARA.
* Executa o parágrafo para imprimir os totais.
     CLOSE PAYROLL-FILE-IN
* Fecha o arquivo de entrada da folha de pagamento.
          INCOME-TAX-REPORT-OUT.
* Fecha o arquivo de saída do relatório de imposto de renda.
     STOP RUN.
* Encerra a execução do programa.
 B10-INIT-PARA.
* Parágrafo de inicialização.
     MOVE ZEROS TO WS-LINES-USED
* Zera o contador de linhas usadas na página.
                         WS-EARN-THIS-PER-TOT
* Zera o total de ganhos deste período.
                         WS-FED-TAX-AMT-TOT
* Zera o total do imposto federal anual.
                         WS-FED-TAX-THIS-PER-TOT.
* Zera o total do imposto federal deste período.
     MOVE 1 TO WS-PAGE-COUNT.
* Inicializa o contador de página com 1.
     PERFORM C10-HEADINGS-PARA.
* Executa o parágrafo para imprimir os cabeçalhos.
 B20-PROCESS-PARA.
* Parágrafo de processamento de cada registro.
     MULTIPLY WS-PR-EXEMPTIONS-IN BY 1000 GIVING
* Multiplica o número de isenções por 1000 para obter o subtotal de isenção.
          WS-EXEMPT-SUB-TOT ROUNDED.
* Armazena o resultado em WS-EXEMPT-SUB-TOT, arredondando.
     MULTIPLY WS-PR-EARN-THIS-PER-IN BY 26 GIVING
* Multiplica os ganhos do período por 26 (assumindo 26 períodos de pagamento por ano) para obter o ganho anual.
          WS-ANN-EARN.
* Armazena o resultado em WS-ANN-EARN.
     SUBTRACT WS-EXEMPT-SUB-TOT FROM WS-ANN-EARN GIVING
* Subtrai o subtotal de isenção do ganho anual para obter os ganhos ajustados.
          WS-EARNINGS ROUNDED.
* Armazena o resultado em WS-EARNINGS, arredondando.

     IF WS-PR-MARITAL-STATUS-IN = 1 THEN
* Se o status civil for 1 (solteiro).
          PERFORM C30-SINGLE-TAX-PARA
* Executa o parágrafo para cálculo de imposto para solteiros.
     ELSE
          MOVE "E" TO WS-ROW-FOUND-SWITCH
* Caso contrário, move "E" para a chave de linha encontrada (indicando que não é solteiro).
     END-IF.

     IF WS-PR-MARITAL-STATUS-IN = 2 THEN
* Se o status civil for 2 (casado).
          PERFORM C40-MARRIED-TAX-PARA
* Executa o parágrafo para cálculo de imposto para casados.
     ELSE
          MOVE "E" TO WS-ROW-FOUND-SWITCH
* Caso contrário, move "E" para a chave de linha encontrada (indicando que não é casado).
     END-IF.

     IF WS-PR-MARITAL-STATUS-IN = 3 THEN
* Se o status civil for 3 (isento de imposto).
          PERFORM C60-TAX-EXEMPT-PARA
* Executa o parágrafo para processar isenção de imposto.
     END-IF.


     ADD 2 TO WS-LINES-USED.
* Adiciona 2 ao contador de linhas usadas na página (para o detalhe).
     ADD WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-TOT.
* Adiciona os ganhos deste período ao total geral de ganhos.
     READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN
* Lê o próximo registro do arquivo de folha de pagamento para a área de trabalho.
          AT END MOVE "Y" TO WS-EOF-SWITCH.
* Se atingir o fim do arquivo, move "Y" para a chave de EOF.
 C10-HEADINGS-PARA.
* Parágrafo para imprimir os cabeçalhos do relatório.
     MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
* Move o número da página para a área de saída formatada.
     WRITE IT-REPORT-OUT FROM WS-HEADING
* Escreve o cabeçalho principal no arquivo de saída.
          AFTER ADVANCING PAGE.
* Avança para uma nova página antes de escrever.
     MOVE SPACES TO IT-REPORT-OUT.
* Limpa a linha de saída com espaços.
     WRITE IT-REPORT-OUT
* Escreve uma linha em branco.
          AFTER ADVANCING 1 LINE.
* Avança 1 linha.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-01
* Escreve a primeira linha do cabeçalho das colunas.
          AFTER ADVANCING 1 LINES.
* Avança 1 linha.
     WRITE IT-REPORT-OUT FROM WS-COLUMN-HEADING-02
* Escreve a segunda linha do cabeçalho das colunas.
          AFTER ADVANCING 1 LINE.
* Avança 1 linha.
     ADD 3 TO WS-LINES-USED.
* Adiciona 3 ao contador de linhas usadas (cabeçalhos e linha em branco).
     ADD 1 TO WS-PAGE-COUNT.
* Incrementa o contador de página.
 C20-TOTAL-PARA.
* Parágrafo para imprimir a linha de totais.
     IF WS-LINES-USED >= 57 THEN
* Se as linhas usadas na página atingirem o limite (57).
          PERFORM C10-HEADINGS-PARA
* Imprime novos cabeçalhos.
          MOVE ZEROS TO WS-LINES-USED
* Zera o contador de linhas usadas.
     END-IF.
     MOVE WS-EARN-THIS-PER-TOT TO WS-TOT-EARN-THIS-PER-OUT.
* Move o total de ganhos do período para a área de saída formatada.
     MOVE WS-FED-TAX-AMT-TOT TO WS-TOT-FED-TAX-AMT-OUT.
* Move o total do imposto federal anual para a área de saída formatada.
     MOVE WS-FED-TAX-THIS-PER-TOT TO WS-TOT-FED-TAX-THIS-PER-OUT.
* Move o total do imposto federal do período para a área de saída formatada.
     MOVE SPACES TO IT-REPORT-OUT.
* Limpa a linha de saída com espaços.
     WRITE IT-REPORT-OUT
* Escreve uma linha em branco.
          AFTER ADVANCING 1 LINE.
* Avança 1 linha.
     WRITE IT-REPORT-OUT FROM WS-TOTAL-LINE
* Escreve a linha de totais.
          AFTER ADVANCING 2 LINE.
* Avança 2 linhas.
 C30-SINGLE-TAX-PARA.
* Parágrafo para calcular o imposto para solteiros.
     SET S-INDEX TO 1.
* Inicializa o índice da tabela de solteiros para 1.
     SEARCH WS-SING-TAX-ROW
* Procura na tabela de imposto para solteiros.
          AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
* Se não encontrar uma correspondência, move "E" para a chave de linha encontrada.
          WHEN WS-EARNINGS IS >= WS-SING-LOW  (S-INDEX) AND
* Quando os ganhos ajustados forem maiores ou iguais ao limite inferior da faixa E
                               IS <= WS-SING-HIGH (S-INDEX)
* E os ganhos ajustados forem menores ou iguais ao limite superior da faixa.
          MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Move "Y" para a chave de linha encontrada.
     IF WS-ROW-FOUND THEN
* Se uma faixa de imposto foi encontrada.
          COMPUTE WS-ANN-TAX-AMT =
* Calcula o imposto anual:
              WS-SING-BASE-AMT (S-INDEX) +
* Valor base da faixa de imposto +
              WS-SING-PERCENT (S-INDEX) *
* Percentual da faixa de imposto *
              (WS-EARNINGS - WS-SING-LOW (S-INDEX)).
* (Ganhos ajustados - Limite inferior da faixa).
     PERFORM C50-LINE-OUTPUT-PARA.
* Executa o parágrafo para formatar e escrever a linha de detalhe.
 C40-MARRIED-TAX-PARA.
* Parágrafo para calcular o imposto para casados.
     SET M-INDEX TO 1.
* Inicializa o índice da tabela de casados para 1.
     SEARCH WS-MARR-TAX-ROW
* Procura na tabela de imposto para casados.
          AT END MOVE "E" TO WS-ROW-FOUND-SWITCH
* Se não encontrar uma correspondência, move "E" para a chave de linha encontrada.
          WHEN WS-EARNINGS IS >= WS-MARR-LOW (M-INDEX) AND
* Quando os ganhos ajustados forem maiores ou iguais ao limite inferior da faixa E
                               IS <= WS-MARR-HIGH (M-INDEX)
* E os ganhos ajustados forem menores ou iguais ao limite superior da faixa.
          MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Move "Y" para a chave de linha encontrada.
     IF WS-ROW-FOUND THEN
* Se uma faixa de imposto foi encontrada.
          COMPUTE WS-ANN-TAX-AMT =
* Calcula o imposto anual:
              WS-MARR-BASE-AMT (M-INDEX) +
* Valor base da faixa de imposto +
              WS-MARR-PERCENT (M-INDEX) *
* Percentual da faixa de imposto *
              (WS-EARNINGS - WS-MARR-LOW (M-INDEX)).
* (Ganhos ajustados - Limite inferior da faixa).
     PERFORM C50-LINE-OUTPUT-PARA.
* Executa o parágrafo para formatar e escrever a linha de detalhe.
 C50-LINE-OUTPUT-PARA.
* Parágrafo para formatar e escrever a linha de detalhe do relatório.
     DIVIDE WS-ANN-TAX-AMT BY 26 GIVING WS-PER-TAX-AMT ROUNDED.
* Divide o imposto anual por 26 (número de períodos de pagamento) para obter o imposto por período, arredondando.
     ADD WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-TOT.
* Adiciona o imposto anual ao total geral do imposto federal anual.
     ADD WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-TOT.
* Adiciona o imposto por período ao total geral do imposto federal deste período.
     MOVE WS-PR-SSN-3-IN TO WS-SSN-3-OUT.
* Move os primeiros 3 dígitos do SSN para a área de saída.
     MOVE WS-PR-SSN-2-IN TO WS-SSN-2-OUT.
* Move os próximos 2 dígitos do SSN para a área de saída.
     MOVE WS-PR-SSN-4-IN TO WS-SSN-4-OUT.
* Move os últimos 4 dígitos do SSN para a área de saída.
     MOVE WS-PR-EMPL-NAME-IN TO WS-EMPL-NAME-OUT.
* Move o nome do empregado para a área de saída.
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-MARITAL-STATUS-OUT.
* Move o status civil para a área de saída.
     MOVE WS-PR-EXEMPTIONS-IN TO WS-WITHHOLD-EXEMPT-OUT.
* Move o número de isenções para a área de saída.
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT.
* Move os ganhos deste período para a área de saída formatada.
     MOVE WS-ANN-EARN TO WS-ANN-EARN-OUT.
* Move os ganhos anuais para a área de saída formatada.
     MOVE WS-EARNINGS TO WS-ADJ-EARN-OUT.
* Move os ganhos ajustados para a área de saída formatada.
     MOVE WS-PER-TAX-AMT TO WS-FED-TAX-THIS-PER-OUT.
* Move o imposto por período para a área de saída formatada.
     MOVE WS-ANN-TAX-AMT TO WS-FED-TAX-AMT-OUT.
* Move o imposto anual para a área de saída formatada.
     IF WS-LINES-USED >= 57 THEN
* Se as linhas usadas na página atingirem o limite (57).
          PERFORM C10-HEADINGS-PARA
* Imprime novos cabeçalhos.
          MOVE ZEROS TO WS-LINES-USED
* Zera o contador de linhas usadas.
     END-IF.
     MOVE SPACES TO IT-REPORT-OUT.
* Limpa a linha de saída com espaços.
     WRITE IT-REPORT-OUT
* Escreve uma linha em branco.
          AFTER ADVANCING 1 LINE.
* Avança 1 linha.
     WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
* Escreve a linha de detalhe.
          AFTER ADVANCING 1 LINE.
* Avança 1 linha.
 C60-TAX-EXEMPT-PARA.
* Parágrafo para processar e escrever a linha de empregados isentos de imposto.
     ADD 2 TO WS-LINES-USED.
* Adiciona 2 ao contador de linhas usadas na página.
     MOVE WS-PR-SSN-3-IN TO WS-TE-SSN-3-OUT.
* Move os primeiros 3 dígitos do SSN para a área de saída da linha de isenção.
     MOVE WS-PR-SSN-2-IN TO WS-TE-SSN-2-OUT.
* Move os próximos 2 dígitos do SSN para a área de saída da linha de isenção.
     MOVE WS-PR-SSN-4-IN TO WS-TE-SSN-4-OUT.
* Move os últimos 4 dígitos do SSN para a área de saída da linha de isenção.
     MOVE WS-PR-EMPL-NAME-IN TO WS-TE-EMPL-NAME-OUT.
* Move o nome do empregado para a área de saída da linha de isenção.
     MOVE WS-PR-MARITAL-STATUS-IN TO WS-TE-MARITAL-STATUS-OUT.
* Move o status civil para a área de saída da linha de isenção.
     MOVE WS-PR-EXEMPTIONS-IN TO WS-TE-WITHHOLD-EXEMPT-OUT.
* Move o número de isenções para a área de saída da linha de isenção.
     MOVE WS-PR-EARN-THIS-PER-IN TO WS-TE-EARN-THIS-PER-OUT.
* Move os ganhos deste período para a área de saída formatada da linha de isenção.
     MOVE "Y" TO WS-ROW-FOUND-SWITCH.
* Define a chave de linha encontrada como 'Y' (pode ser redundante aqui, mas mantém a consistência).
     IF WS-ROW-FOUND THEN
* Se a chave de linha encontrada for 'Y'.
          MOVE SPACES TO IT-REPORT-OUT.
* Limpa a linha de saída com espaços.
          WRITE IT-REPORT-OUT
* Escreve uma linha em branco.
              AFTER ADVANCING 1 LINE.
* Avança 1 linha.
          WRITE IT-REPORT-OUT FROM WS-TAX-EXEMPT-LINE
* Escreve a linha de isenção de imposto.
              AFTER ADVANCING 1 LINE.
* Avança 1 linha.