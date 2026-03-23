
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

if (length(Sys.getenv("usuario_carlos_rscript")) > 1){
  use_python("C:/Users/carlo/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)
}

# ... (seus imports iniciais permanecem iguais)

args <- commandArgs(trailingOnly = TRUE)
target_path <- args[1]
model_path <- args[2] # Pode ser o caminho de um arquivo ou "all"
predict_size <- as.numeric(args[3])

carregar_modelo <- function(caminho_rds) {
  library(reticulate)
  library(daltoolboxdp)
  
  KERAS_MODELS <- c("lstm", "conv1d")
  tag      <- tolower(basename(caminho_rds))
  is_keras <- any(sapply(KERAS_MODELS, function(k) grepl(k, tag)))
  
  model <- readRDS(caminho_rds)
  
  if (is_keras) {
    base      <- sub("\\.rds$", "", caminho_rds)
    pt_path   <- paste0(base, ".pt")
    meta_path <- paste0(base, "_meta.rds")
    
    if (!file.exists(pt_path))   stop(sprintf("Pesos não encontrados: %s", pt_path))
    if (!file.exists(meta_path)) stop(sprintf("Metadados não encontrados: %s", meta_path))
    
    meta  <- readRDS(meta_path)
    torch <- import("torch")
    
    # Carrega as funções Python da biblioteca correta
    if (grepl("conv1d", tag)) {
      py_file <- system.file("python", "ts_conv1d.py", package="daltoolboxdp")
      source_python(py_file)
      message(sprintf("Recriando arquitetura Conv1D para input_size=%d ...", meta$input_size))
      model$model <- ts_conv1d_create(meta$input_size, meta$input_size)
    } else {
      py_file <- system.file("python", "ts_lstm.py", package="daltoolboxdp")
      source_python(py_file)
      message(sprintf("Recriando arquitetura LSTM para input_size=%d ...", meta$input_size))
      model$model <- ts_lstm_create(meta$input_size, meta$input_size)
    }
    
    message("class(model$model) após create: ", paste(class(model$model), collapse=", "))
    
    # Carrega os pesos reais por cima da arquitetura recriada
    state_dict <- torch$load(pt_path, map_location="cpu")
    model$model$load_state_dict(state_dict)
    model$model$eval()
    message("Pesos carregados com sucesso.")
    
    # Restaura metadados
    model$input_size <- meta$input_size
    model$sw_size    <- meta$sw_size
  }
  
  return(model)
}

# Função interna para processar e salvar a previsão
processar_previsao <- function(caminho_modelo, tag_algoritmo, target_path, predict_size) {
  
  model <- carregar_modelo(caminho_modelo)
  window_size <- model$input_size
  
  #Verificar se é modelo aprendizado profundo
  model_dp <- Filter(function(k) grepl(k, caminho_modelo, ignore.case=TRUE), c("lstm"))
  model_dp <- if (length(model_dp) > 0) model_dp[1] else NA
  if (!is.na(model_dp)) window_size <- window_size + 1 # verifica se é modelo de aprendizado profundo

  df <- read.csv(target_path)
  idx_numerico <- which(sapply(df, is.numeric))[1]
  dados_serie <- df[, idx_numerico]
  
  if (!is.null(window_size) && window_size != 0) {
    ultimos_dados <- tail(dados_serie, window_size + predict_size - 1)
    ts_input  <- ts_data(ultimos_dados, window_size)
    ts_input  <- ts_projection(ts_input)
  } else {
    # arima
    ultimos_dados <- tail(dados_serie, predict_size - 1)
    ts_input  <- ts_data(ultimos_dados)
    ts_input  <- ts_projection(ts_input)
  }
  
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
  # Pegamos todos os arquivos .rds
  arquivos <- list.files(pasta_modelos, pattern = "\\.rds$", full.names = TRUE)
  
  if(length(arquivos) == 0) stop("Nenhum arquivo .rds encontrado em output/models")
  
  info_arquivos <- file.info(arquivos)
  info_arquivos$path <- rownames(info_arquivos)
  info_arquivos$nome <- basename(info_arquivos$path)
  
  # --- PASSO CRUCIAL: Filtrar para ignorar arquivos que terminam com _meta.rds ---
  # Isso garante que você só processe os modelos principais
  info_arquivos <- info_arquivos[!grepl("_meta\\.rds$", info_arquivos$nome), ]
  
  algoritmos <- c("knn", "mlp", "arima", "lstm", "elm", "rf", "conv1d", "svm")
  
  for (alg in algoritmos) {
    modelos_alg <- info_arquivos[grepl(alg, info_arquivos$nome, ignore.case = TRUE), ]
    
    if (nrow(modelos_alg) > 0) {
      # Agora o modelo_recente será obrigatoriamente um arquivo sem "_meta"
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