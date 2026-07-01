# Previsão de Receitas de Multas de Trânsito do Rio de Janeiro

**Aluno:** Carlos Henrique de Oliveira Pereira

**TCC:** Previsão de Receitas de Multas de Trânsito do Município do Rio de Janeiro
[PDF do TCC](TCC_Carlos.pdf)

---

## Descrição

Este projeto desenvolve e avalia modelos estatísticos e de aprendizado de máquina para prever a receita mensal de multas de trânsito do município do Rio de Janeiro. Os dados utilizados são da Secretaria Municipal de Trânsito do Rio de Janeiro.

Modelos implementados:
- ARIMA (modelo estatístico clássico)
- KNN (K-Nearest Neighbors)
- MLP (Multilayer Perceptron)
- LSTM (Long Short-Term Memory)
- SVM (Support Vector Machine)
- Random Forest
- Conv1D (Convolutional Neural Network 1D)
- ELM (Extreme Learning Machine)

---

## Estrutura do Projeto

```
.
├── automacoes/             # Scripts R do projeto
│   ├── run_ml.R            # Orquestrador de treinamento
│   ├── predict_ml.R        # Geração de previsões com modelos salvos
│   ├── wf_experiment.R     # Funções core (train_ml, test_ml, run_ml)
│   ├── wf_knn.R            # Workflow KNN
│   ├── wf_mlp.R            # Workflow MLP
│   ├── wf_lstm.R           # Workflow LSTM
│   ├── wf_svm.R            # Workflow SVM
│   ├── wf_rf.R             # Workflow Random Forest
│   ├── wf_conv1d.R         # Workflow Conv1D
│   ├── wf_elm.R            # Workflow ELM
│   ├── wf_arima.R          # Workflow ARIMA
│   ├── get_data.R          # Coleta de dados (BigQuery)
│   ├── data_adjust.R       # Ajuste/transformação de dados
│   └── merge_files.R       # Merge de arquivos de resultado
├── target/                 # Dados de entrada
│   └── autuacao_receita_mensal.csv
├── tcc_carlos.Rproj        # Projeto RStudio
└── README.md
```

### Estrutura de saída (gerada na execução)

```
automacoes/output/
├── models/                 # Modelos treinados salvos (.rds)
├── graphics/               # Gráficos de previsão (.jpg)
├── prediction/             # Previsões geradas pelo predict_ml.R
│   ├── *.csv               # Valores previstos
│   └── *.png               # Gráfico histórico + previsão
├── results/                # DataFrames de resultados (.rdata e .csv)
└── hyper/                  # Hiperparâmetros (vazio na config atual)
```

---

## Dados de Entrada

O arquivo `target/autuacao_receita_mensal.csv` contém uma série temporal mensal com 120 observações (10 anos) em uma única coluna:

```
"valor_pago_total"
16002687.3
13912377.5
...
```

A única coluna numérica é usada como série temporal. O arquivo deve estar em formato CSV com cabeçalho.

---

## Dependências

### R (versão 4.3+)
- R (recomendado: 4.3.1)
- RStudio (opcional)

### Script de instalação completa

Execute o script abaixo no R para instalar todas as dependências:

```r
# ============================================================
# Script completo de instalação das dependências do projeto
# ============================================================

# 1. Pacotes disponíveis no CRAN
install.packages(c(
  "daltoolbox",          # Framework de séries temporais (ML)
  "daltoolboxdp",        # Deep learning (LSTM, Conv1D)
  "stringi",             # Manipulação de strings
  "dplyr",               # Manipulação de dados
  "stringr",             # Manipulação de strings
  "ggplot2",             # Gráficos
  "e1071",               # SVM
  "devtools",            # Ferramentas de desenvolvimento
  "reticulate",          # Interface com Python
  "bigrquery",           # Acesso ao Google BigQuery (opcional)
  "purrr"                # Programação funcional
))

# 2. tspredit (instalar do arquivo local)
#    Baixe o arquivo tspredit_1.2.767.tar.gz para a pasta raiz do projeto
#    e execute:
install.packages(
  "tspredit_1.2.767.tar.gz",
  repos = NULL,
  type = "source"
)

# 3. Verificar instalação
library(daltoolbox)
library(tspredit)
cat("daltoolbox versão:", as.character(packageVersion("daltoolbox")), "\n")
cat("tspredit versão:", as.character(packageVersion("tspredit")), "\n")
```

### Python (para LSTM e Conv1D)

Os modelos LSTM e Conv1D requerem Python com PyTorch:

```bash
pip install torch torchvision torchaudio
```

---

## Como Executar

Navegue até a pasta `automacoes/` e execute os scripts a partir dela.

### 1. Treinamento (`run_ml.R`)

```bash
cd automacoes/

# Executar ARIMA com tamanho de teste 12
Rscript run_ml.R "../target/autuacao_receita_mensal.csv" "arima" 12

# Executar múltiplos modelos
Rscript run_ml.R "../target/autuacao_receita_mensal.csv" "arima,knn,mlp" 12

# Executar todos os modelos
Rscript run_ml.R "../target/autuacao_receita_mensal.csv" "all" 12
```

**Parâmetros:**
1. Caminho para o arquivo CSV de entrada
2. Lista de modelos separados por vírgula, ou `"all"`
3. Número de meses para o conjunto de teste

### 2. Predição (`predict_ml.R`)

```bash
cd automacoes/

# Predição com um modelo específico
Rscript predict_ml.R "../target/autuacao_receita_mensal.csv" "output/models/valor_pago_total_arima.rds" 12

# Predição com todos os modelos disponíveis em output/models/
Rscript predict_ml.R "../target/autuacao_receita_mensal.csv" "all" 12
```

**Parâmetros:**
1. Caminho para o CSV com dados históricos
2. Caminho do modelo `.rds` ou `"all"` para usar todos os modelos em `output/models/`
3. Número de meses a prever

---

## Fluxo de Trabalho

### Treinamento
1. O script lê o CSV de entrada
2. Divide os dados em treino/teste (ex: 108 meses treino, 12 meses teste)
3. Para modelos ML: realiza busca em grade de hiperparâmetros com validação cruzada
4. Seleciona o melhor modelo por maior R²
5. Re-treina o melhor modelo com geração de gráfico
6. Salva o modelo em `output/models/` e o gráfico em `output/graphics/`

### Predição
1. Carrega modelo salvo (`.rds`)
2. Lê os dados históricos do CSV
3. Gera previsões para N passos à frente
4. Salva valores previstos em CSV e gráfico comparativo em PNG

---

## Resultados

### Arquivos gerados

| Tipo | Formato | Localização | Descrição |
|------|---------|-------------|-----------|
| Modelo treinado | .rds | `output/models/` | Modelo serializado para reuso |
| Gráfico treino | .jpg | `output/graphics/` | Previsão vs valores reais no teste |
| Previsão | .csv | `output/prediction/` | Valores previstos (um por linha) |
| Gráfico previsão | .png | `output/prediction/` | Série histórica + previsão futura |
| Resultados | .rdata | `output/results/` | Métricas (R², SMAPE, MSE) formato R |
| Resultados | .csv | `output/results/` | Métricas (R², SMAPE, MSE) formato CSV |

### Métricas de avaliação
- **R²**: Coeficiente de determinação
- **SMAPE**: Symmetric Mean Absolute Percentage Error
- **MSE**: Mean Squared Error

---

## Compatibilidade dos Pacotes

> **ATENÇÃO:** Este código foi desenvolvido e testado com as versões específicas:
> - **daltoolbox** ≥ 1.1.727 (CRAN)
> - **tspredit** 1.2.767 (instalação local via `tspredit_1.2.767.tar.gz`)
>
> Modelos ARIMA funcionam com a versão atual do CRAN. Modelos ML (KNN, MLP, SVM, RF, ELM) foram adaptados para funcionar com `daltoolbox` ≥ 1.1.727 — o `wf_experiment.R` contém correções para compatibilidade, incluindo substituição da função `ts_tune` (removida do CRAN) por `fit()` direto com ajuste manual de parâmetros.

---

## Notas Adicionais

- O script `get_data.R` acessa o Google BigQuery (`rj-smtr`) e requer credenciais do GCP
- LSTM e Conv1D requerem Python com PyTorch configurado e acessível pelo pacote `reticulate`
- O seed do experimento é fixo (`set.seed(120770)`) para reprodutibilidade
