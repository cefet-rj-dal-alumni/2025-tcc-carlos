library(daltoolbox)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo) 

#' @title Realiza validação cruzada para séries temporais com janela deslizante.
#'
#' @param serie_temporal Objeto de série temporal (ex: ts_data).
#' @param datas_ts Vetor de datas correspondente à série temporal.
#' @param tam_janela_treino Número de observações na janela de treinamento.
#' @param tam_janela_teste Número de observações na janela de teste (horizonte de previsão).
#' @param step O número de observações para deslizar a janela a cada iteração. Geralmente igual a tam_janela_teste.
#' @param modelo_func Uma função que retorna uma nova instância do modelo a ser treinado (ex: function() { ts_arima() }).
#'
#' @return Uma lista contendo:
#'         - 'metricas_por_fold': Um dataframe com as métricas de avaliação para cada fold.
#'         - 'media_metricas': As métricas médias de todos os folds.
#'         - 'predicoes_consolidadas': Um dataframe com os valores reais, previstos, datas e o ID do fold.
validacao_cruzada_ts <- function(serie_temporal, datas_ts, tam_janela_treino, tam_janela_teste, step, modelo_func) {
  
  resultados_folds <- list()
  predicoes_lista <- list()
  n_total <- length(serie_temporal)
  
  pos_inicial <- 1
  fold <- 1
  
  cat("Iniciando a validação cruzada...\n")
  
  while ((pos_inicial + tam_janela_treino + tam_janela_teste - 1) <= n_total) {
    
    # Índices para treino e teste
    fim_treino <- pos_inicial + tam_janela_treino - 1
    inicio_teste <- fim_treino + 1
    fim_teste <- inicio_teste + tam_janela_teste - 1
    
    cat(sprintf("Fold %d: Treino [índices %d-%d], Teste [índices %d-%d]\n", fold, pos_inicial, fim_treino, inicio_teste, fim_teste))
    
    # Separação dos dados
    ts_treino <- serie_temporal[pos_inicial:fim_treino]
    ts_teste <- serie_temporal[inicio_teste:fim_teste]
    
    # Projeção para formato input/output
    io_train <- ts_projection(ts_treino)
    io_test <- ts_projection(ts_teste)
    
    # Treinamento do modelo (usando a função para criar uma nova instância)
    modelo <- modelo_func()
    modelo <- fit(modelo, x = io_train$input, y = io_train$output)
    
    # Predição (replicando seu método)
    predicao <- predict(modelo, x = io_test$input, steps_ahead = length(io_test$output))
    predicao <- as.vector(predicao)
    output_real <- as.vector(io_test$output)
    
    # Avaliação
    metricas <- evaluate(modelo, output_real, predicao)
    metricas$fold <- fold
    resultados_folds[[fold]] <- metricas
    
    cat(sprintf(" -> Resultado Fold %d: MSE = %.2f, RMSE = %.2f, MAPE = %.4f\n", fold, metricas$mse, metricas$rmse, metricas$mape))
    
    # Armazenamento das predições para plotagem
    predicoes_lista[[fold]] <- data.frame(
      data = datas_ts[inicio_teste:fim_teste],
      valor_real = output_real,
      valor_predito = predicao,
      fold = fold
    )
    
    # Desliza a janela
    pos_inicial <- pos_inicial + step
    fold <- fold + 1
  }
  
  # Agrupamento dos resultados
  metricas_consolidadas <- dplyr::bind_rows(resultados_folds)
  
  # CONVERSÃO DE COLUNAS PARA NUMÉRICO (ADICIONAR ESTA PARTE)
  # Identifique as colunas que devem ser numéricas (excluindo 'fold')
  cols_numericas <- names(metricas_consolidadas)[names(metricas_consolidadas) != "fold"]
  
  # Aplique a conversão para numérico, tratando vírgulas como decimais se for o caso
  for (col in cols_numericas) {
    if (is.character(metricas_consolidadas[[col]])) {
      # Substitui vírgulas por pontos se necessário (comum em PT-BR)
      metricas_consolidadas[[col]] <- gsub(",", ".", metricas_consolidadas[[col]])
      metricas_consolidadas[[col]] <- as.numeric(metricas_consolidadas[[col]])
    }
  }
  
  # Agora o cálculo da média deve funcionar
  media_geral <- metricas_consolidadas %>%
    select(-fold) %>%
    summarise_all(mean, na.rm = TRUE) # Adicione na.rm = TRUE para ignorar NAs se houver
  
  predicoes_consolidadas <- dplyr::bind_rows(predicoes_lista)
  
  cat("\n--- Validação Cruzada Concluída ---\n")
  
  return(
    list(
      metricas_por_fold = metricas_consolidadas,
      media_metricas = media_geral,
      predicoes_consolidadas = predicoes_consolidadas
    )
  )
}