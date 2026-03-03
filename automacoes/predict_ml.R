
#conv1d com erro de compatibilidade

source('./wf_knn.R')

library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(stringi)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(devtools)

# ... (seus imports iniciais permanecem iguais)

args <- commandArgs(trailingOnly = TRUE)
target_path <- args[1]
model_path <- args[2] # Pode ser o caminho de um arquivo ou "all"
predict_size <- as.numeric(args[3])

# Função interna para processar e salvar a previsão
processar_previsao <- function(caminho_modelo, tag_algoritmo, target_path, predict_size) {
  model <- readRDS(caminho_modelo)
  window_size <- model$input_size
  
  df <- read.csv(target_path)
  idx_numerico <- which(sapply(df, is.numeric))[1]
  dados_serie <- df[, idx_numerico]
  
  # Preparação dos dados
  ultimos_dados <- tail(dados_serie, window_size + predict_size - 1)
  ts_input <- ts_data(ultimos_dados, window_size)
  ts_input <- ts_projection(ts_input)
  
  # Predição
  prediction <- predict(model, x=ts_input$input[1,], steps_ahead = predict_size)
  prediction_vector <- as.vector(prediction)
  
  # Criar diretório se não existir
  if (!file.exists('output/prediction')){
    dir.create('output/prediction', recursive = TRUE)
  }
  
  # Salvar CSV com data e nome do algoritmo
  data_hoje <- format(Sys.Date(), "%Y-%m-%d")
  nome_base <- paste0("pred_", tag_algoritmo, "_", data_hoje)
  
  write.table(prediction_vector, paste0("output/prediction/", nome_base, ".csv"), 
              sep = ",", row.names = FALSE, col.names = FALSE)
  
  # Plotagem
  n_hist <- min(36, length(dados_serie))
  df_plot <- rbind(
    data.frame(x = 1:n_hist, y = tail(dados_serie, n_hist), tipo = "Histórico"),
    data.frame(x = (n_hist + 1):(n_hist + predict_size), y = prediction_vector, tipo = "Previsão")
  )
  
  plot_previsao <- ggplot(df_plot, aes(x = x, y = y, color = tipo)) +
    geom_line(linewidth = 1) + geom_point() +
    scale_color_manual(values = c("Histórico" = "black", "Previsão" = "green")) +
    theme_minimal() +
    labs(title = paste("Previsão -", toupper(tag_algoritmo)),
         subtitle = paste("Modelo:", basename(caminho_modelo)),
         x = "Tempo", y = "Valor")
  
  ggsave(paste0("output/prediction/", nome_base, ".png"), plot_previsao, width = 10, height = 6)
  message(paste("Sucesso para:", tag_algoritmo))
}

# --- Lógica de Seleção de Modelos ---

if (model_path == "all") {
  pasta_modelos <- "output/models"
  arquivos <- list.files(pasta_modelos, pattern = "\\.rds$", full.names = TRUE)
  
  if(length(arquivos) == 0) stop("Nenhum arquivo .rds encontrado em output/models")
  
  # Criar dataframe com info dos arquivos para ordenar por data
  info_arquivos <- file.info(arquivos)
  info_arquivos$path <- rownames(info_arquivos)
  info_arquivos$nome <- basename(info_arquivos$path)
  
  # Lista de algoritmos para buscar
  algoritmos <- c("knn", "mlp", "arima", "lstm", "elm", "rf", "conv1d", "svm") # Adicionei ELM que estava no seu exemplo

  for (alg in algoritmos) {
    # Filtra arquivos que contém o nome do algoritmo e pega o com mtime (modificação) mais recente
    modelos_alg <- info_arquivos[grepl(alg, info_arquivos$nome, ignore.case = TRUE), ]
    
    if (nrow(modelos_alg) > 0) {
      modelo_recente <- modelos_alg[order(modelos_alg$mtime, decreasing = TRUE), ][1, "path"]
      tryCatch({
        processar_previsao(modelo_recente, alg, target_path, predict_size)
      }, error = function(e) {
        warning(paste("Erro ao processar", alg, ":", e$message))
      })
    }
  }
} else {
  # Processamento individual (caso original)
  tag_simples <- str_extract(basename(model_path), "(knn|mlp|arima|lstm|elm|rf|elm|conv1d|svm)")
  processar_previsao(model_path, ifelse(is.na(tag_simples), "model", tag_simples), target_path, predict_size)
}