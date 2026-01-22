TCC - Carlos Henrique

**Previsão de Receitas de Multas de Trânsito do Município do Rio de Janeiro** <!-- substitua pelo título do TCC -->
**Alunos: Carlos Henrique de Oliveira Pereira** <!-- substitua pelos nomes dos alunos -->
**Semestre de Defesa:** <!-- ano-semestre, exemplo: 2025-2 -->

[TCC2_Carlos.pdf](https://github.com/user-attachments/files/24785906/TCC2_Carlos.pdf)

# TL;DR

<!-- Resumo super conciso para quem não quer ler o README e começar a executar o código -->
Para rodar:
```$ pm2 start ecosystem.config.js```


# Descrição Geral
Este trabalho foi realizado com o objetivo de se obter um modelo de dados que seja capaz de prever receitas de multas de trânsito do município do Rio de Janeiro no período de um ano.
Os dados foram disponibilizados pela Secretaria Municipal de Trânsito do Rio de Janeiro.

<!-- Resumo do TCC -->


# Funcionalidades
Na pasta "automacoes" é onde está implementada a parte relevante do trabalho.
Os arquivos rw_mlp.R, rw_knn.R, rw_lstm.R, rw_svm.R, rw_rf.R, rw_conv1d.R, r2_elm.R e wf_arima.R são responsáveis por rodar individualmente os modelos de aprendizado de máquina.


# Arquitetura
<!-- Descreva nessa seção a arquitetura do seu código. Sugestão: use mermaid para inclusão de diagramas que ajudem a entender seu código (https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-… -->


# Dependências
<!-- Apresente a lista de dependências do seu código. Quando necessário, incluia links. Exemplo: -->
* Ambiente R Studio 2024.12.0
* Bibliotecas: Daltoolbox, TSPredit 

# Preparo
O arquivo .csv contendo as receitas mensais deve estar em uma pasta chamada input, dentro de uma pasta de mesmo nome. 
Segue um exemplo a seguir: 
Csv base: receitas_mensais.csv

Caminho do csv base: ./automacoes/receitas_mensais/input/receitas_mensais.csv

# Execução
O arquivo run_ml.R é responsável por rodar automaticamente o test para os modelos inseridos via argumentos.

Exemplo no terminal: Rscript run_ml.R autuacao_mensal2 arima,mlp,

Este comando executa automaticamente a busca em grade para obter o melhor modelo de cada um dos algoritmos desejados(arima, mlp e knn). 
O teste é realizado com um período de 12 meses, com a busca por hiperparâmetros pré-definidos.
Ao final da execução, os resultados dos modelos estarão disponíveis na pasta "./automacoes/receitas_mensais/results", os gráficos em ./automacoes/receitas_mensais/graphics.
