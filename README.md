# TCC – Carlos Henrique de Oliveira Pereira

## Previsão de Receitas de Multas de Trânsito do Município do Rio de Janeiro

**Aluno:** Carlos Henrique de Oliveira Pereira

**Documento do TCC:**
[TCC2_Carlos.pdf](https://github.com/user-attachments/files/24785906/TCC2_Carlos.pdf)

---

## Descrição Geral

Este Trabalho de Conclusão de Curso (TCC) tem como objetivo o desenvolvimento e a avaliação de modelos de aprendizado de máquina e séries temporais capazes de **prever a receita anual proveniente de multas de trânsito** do município do Rio de Janeiro.

Os dados utilizados no estudo foram disponibilizados pela **Secretaria Municipal de Trânsito do Rio de Janeiro** e correspondem a séries temporais de arrecadação mensal. A partir desses dados, foram aplicadas diferentes abordagens de modelagem, incluindo métodos estatísticos clássicos e técnicas modernas de aprendizado de máquina, com foco na comparação de desempenho preditivo.

---

## Funcionalidades

A principal implementação do trabalho encontra-se na pasta **`automacoes`**, que contém os scripts responsáveis pela execução, treinamento e avaliação dos modelos.

Cada arquivo abaixo executa individualmente um modelo específico:

* `wf_mlp.R` – Multilayer Perceptron (MLP)
* `wf_knn.R` – K-Nearest Neighbors (KNN)
* `wf_lstm.R` – Long Short-Term Memory (LSTM)
* `wf_svm.R` – Support Vector Machine (SVM)
* `wf_rf.R` – Random Forest (RF)
* `wf_conv1d.R` – Convolutional Neural Network 1D (Conv1D)
* `wf_elm.R` – Extreme Learning Machine (ELM)
* `wf_arima.R` – Modelo ARIMA

Além disso, o script `run_ml.R` permite a **execução automática de múltiplos modelos**, incluindo busca em grade para seleção de hiperparâmetros.

---

## Arquitetura do Projeto

A estrutura geral do projeto segue a organização abaixo:

```
automacoes/
├── receitas_mensais/
│   ├── input/
│   │   └── receitas_mensais.csv
│   ├── results/
│   ├── graphics/
├── run_ml.R
├── wf_mlp.R
├── wf_knn.R
├── wf_lstm.R
├── wf_svm.R
├── wf_rf.R
├── wf_conv1d.R
├── wf_elm.R
└── wf_arima.R
```

Cada script de modelo é responsável por:

1. Carregar os dados de entrada;
2. Realizar a divisão treino/teste;
3. Executar a busca por hiperparâmetros (quando aplicável);
4. Treinar o modelo;
5. Avaliar o desempenho preditivo;
6. Salvar resultados e gráficos.

---

## Dependências

* **Ambiente:** RStudio 2024.12.0 ou superior
* **Linguagem:** R
* **Bibliotecas principais:**

  * `DALtoolbox`
  * `TSPred`

>  Certifique-se de que todas as bibliotecas estejam corretamente instaladas antes da execução dos scripts.

---

##  Preparo dos Dados

O arquivo `.csv` contendo as **receitas mensais** deve estar localizado em uma pasta chamada `input`, dentro do diretório correspondente ao dataset.

### Exemplo:

* Arquivo base: `receitas_mensais.csv`
* Caminho esperado:

```
./automacoes/receitas_mensais/input/receitas_mensais.csv
```

O nome do dataset informado na execução deve coincidir com o nome da pasta que contém o diretório `input`.

---

##  Execução

O script `run_ml.R` é responsável por executar automaticamente os testes para os modelos especificados via argumentos de linha de comando.

### Exemplo de execução no terminal:

```
Rscript run_ml.R autuacao_mensal2 arima,mlp,knn
```

### Parâmetros:

1. **Nome do dataset** (pasta dentro de `automacoes`)
2. **Lista de modelos**, separados por vírgula

### Modelos disponíveis:

| Argumento | Script correspondente |
| --------- | --------------------- |
| `mlp`     | `wf_mlp.R`            |
| `knn`     | `wf_knn.R`            |
| `lstm`    | `wf_lstm.R`           |
| `svm`     | `wf_svm.R`            |
| `rf`      | `wf_rf.R`             |
| `conv1d`  | `wf_conv1d.R`         |
| `elm`     | `wf_elm.R`            |
| `arima`   | `wf_arima.R`          |

Durante a execução:

* É utilizado um **período de teste de 12 meses**;
* São avaliados hiperparâmetros previamente definidos;
* O melhor modelo de cada abordagem é selecionado com base nas métricas de desempenho.

---

## Resultados

Ao final da execução:

* **Resultados numéricos** estarão disponíveis em:

```
./automacoes/receitas_mensais/results
```

* **Gráficos de previsão e avaliação** estarão disponíveis em:

```
./automacoes/receitas_mensais/graphics
```

Esses arquivos subsidiam a análise comparativa apresentada no trabalho escrito.

---

## 📎 Observações Finais

Este repositório foi desenvolvido com foco acadêmico e experimental, servindo como base para análise de modelos preditivos aplicados a séries temporais econômicas no contexto da administração pública.
