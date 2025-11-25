library(daltoolbox)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(tspredit)

# --- Definição das Funções ---

setwd("~/tcc_carlos/r_scripts/previsoes/validacao")

criar_diretorio_imagens <- function() {
  diretorio <- "imagens"
  if (!dir.exists(diretorio)) {
    dir.create(diretorio)
    # Apenas criamos, sem imprimir status, para manter a saída limpa
  }
}

salvar_plot_fold <- function(plot_obj, fold_index) {
  diretorio <- "imagens"
  nome_arquivo <- sprintf("%s/fold_%02d_predicao.png", diretorio, fold_index)
  ggsave(nome_arquivo, plot_obj, width = 8, height = 5, units = "in")
}

# --- Início do Processamento ---

filename <- "autuacao_mensal.csv"
df <- read.csv(filename) 

if (nrow(df) == 0) {
  stop("O data.frame 'df' está vazio. Não é possível prosseguir.")
}

criar_diretorio_imagens()

test_size <- 12 
sw_size <- 36   
train_size <- sw_size * 2 

fold_index <- 1
inicio_train <- 1
fim_total_janela <- inicio_train + train_size + test_size - 1

# Inicializa a lista para armazenar as métricas de teste de todos os folds
all_test_metrics <- list()

while(fim_total_janela <= length(df$valor_pago))
{
  # Comando de impressão reduzido para indicar o progresso
  cat(sprintf("Processando Fold %d... ", fold_index))
  
  # Seleciona dados
  x <- df$valor_pago[inicio_train : fim_total_janela]
  ts <- ts_data(x, sw_size)
  samp <- ts_sample(ts, test_size = test_size)
  io_train <- ts_projection(samp$train)
  io_test <- ts_projection(samp$test)
  
  # Modelo e Treinamento
  model <- ts_mlp(ts_norm_gminmax(), input_size=1, size=1, decay=0.1, maxit=1000)
  model <- fit(model, x=io_train$input, y=io_train$output)
  
  # Previsão
  adjust <- predict(model, io_train$input)
  prediction <- predict(model, x=io_train$input[nrow(io_train$input), ], steps_ahead=test_size)
  
  # Avaliação do Teste e Armazenamento (Sem impressão aqui)
  ev_test <- evaluate(model, as.vector(io_test$output), as.vector(prediction))
  
  # Adiciona o índice do Fold para rastreamento futuro
  ev_test$Fold <- fold_index
  
  # Armazena as métricas
  all_test_metrics[[fold_index]] <- as.data.frame(ev_test)
  
  # Plotagem e Salvamento
  yvalues <- c(as.vector(io_train$output), as.vector(io_test$output))
  plot_obj <- plot_ts_pred(y=yvalues, yadj=as.vector(adjust), ypre=as.vector(prediction)) + 
    theme(text = element_text(size=16))
  
  salvar_plot_fold(plot_obj, fold_index)
  cat("OK\n") # Confirma o fim do processamento do Fold
  
  # Avanço da Janela
  inicio_train <- inicio_train + test_size
  fim_total_janela <- inicio_train + train_size + test_size - 1
  fold_index <- fold_index + 1
}

# --- Resultado Final Após o Loop ---
cat("\n===================================================\n")
cat("          ✅ RESULTADOS DA VALIDAÇÃO CRUZADA        \n")
cat("===================================================\n")

# Combina as métricas de todos os folds em um data.frame (contém as repetições)
final_metrics_df <- bind_rows(all_test_metrics)

# 🚀 NOVO PASSO: Sumariza as métricas, obtendo uma linha por Fold
metrics_summary_df <- final_metrics_df %>%
  group_by(Fold) %>%
  # Calcula a média das métricas para cada Fold (como são idênticas, 
  # retorna o valor único do Fold). Inclua todas as métricas desejadas.
  summarise(
    mse = mean(mse, na.rm = TRUE),
    smape = mean(smape, na.rm = TRUE),
    R2 = mean(R2, na.rm = TRUE)
    # Adicione outras métricas se houver (e.g., rmse = mean(rmse), mae = mean(mae))
  ) %>%
  ungroup() # Remove o agrupamento para operações futuras

## 📋 Métricas de Cada Fold

cat("\n### 📊 Detalhe das Métricas por Fold\n")
# Imprime o data.frame SUMARIZADO
print(metrics_summary_df) 

## 📈 Média Final das Métricas

# Calcula a média final a partir do data.frame SUMARIZADO
# Excluímos a coluna 'Fold' para o cálculo da média geral
mean_metrics <- colMeans(metrics_summary_df[, names(metrics_summary_df) != "Fold"], na.rm = TRUE)

cat("\n### 📝 Média das Métricas em Todos os Folds\n")
# Imprime o vetor de médias
print(mean_metrics)
cat("---------------------------------------------------\n")