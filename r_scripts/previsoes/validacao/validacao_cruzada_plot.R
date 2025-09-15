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
#' @param zoom_dias_antes_previsao Número de dias para incluir no zoom antes do início da previsão.
#'
#' @return Uma lista contendo:
#'         - 'metricas_por_fold': Um dataframe com as métricas de avaliação para cada fold.
#'         - 'media_metricas': As métricas médias de todos os folds.
#'         - 'predicoes_consolidadas': Um dataframe com os valores reais, previstos, datas e o ID do fold.
#'         - 'plots_folds': Uma lista com os objetos ggplot para cada fold.
validacao_cruzada_plot_ts <- function(serie_temporal, datas_ts, tam_janela_treino, tam_janela_teste, step, modelo_func, zoom_dias_antes_previsao = 30) {
  
  resultados_folds <- list()
  predicoes_lista <- list()
  ajustes_lista <- list() 
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
    
    # Geração dos valores ajustados para o conjunto de treino
    ajuste <- predict(modelo, x = io_train$input, y = io_train$output) 
    ajuste <- as.vector(ajuste)
    
    # Predição para o conjunto de teste
    predicao <- predict(modelo, x = io_test$input, steps_ahead = length(io_test$output))
    predicao <- as.vector(predicao)
    output_real <- as.vector(io_test$output)
    
    # Avaliação
    metricas <- evaluate(modelo, output_real, predicao)
    metricas$fold <- fold
    resultados_folds[[fold]] <- metricas
    
    cat(sprintf(" -> Resultado Fold %d: MSE = %.2f, RMSE = %.2f, MAPE = %.4f\n", fold, metricas$mse, metricas$rmse, metricas$mape))
    
    # Armazenamento dos ajustes e predições para plotagem
    ajustes_lista[[fold]] <- data.frame(
      data = datas_ts[pos_inicial:fim_treino],
      valor_real = as.vector(io_train$output),
      valor_ajustado = ajuste,
      fold = fold
    )
    
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
  cols_numericas <- names(metricas_consolidadas)[names(metricas_consolidadas) != "fold"]
  
  for (col in cols_numericas) {
    if (is.character(metricas_consolidadas[[col]])) {
      metricas_consolidadas[[col]] <- gsub(",", ".", metricas_consolidadas[[col]])
      metricas_consolidadas[[col]] <- as.numeric(metricas_consolidadas[[col]])
    }
  }
  
  media_geral <- metricas_consolidadas %>%
    select(-fold) %>%
    summarise_all(mean, na.rm = TRUE)
  
  predicoes_consolidadas <- dplyr::bind_rows(predicoes_lista)
  ajustes_consolidados <- dplyr::bind_rows(ajustes_lista)
  
  # Vamos gerar os plots aqui mesmo para garantir que cada fold tenha seu gráfico
  plots_folds <- list()
  for (f in unique(predicoes_consolidadas$fold)) {
    data_treino_f <- ajustes_consolidados %>% filter(fold == f)
    data_teste_f <- predicoes_consolidadas %>% filter(fold == f)
    
    # Combine os dados para o gráfico
    yvalues_full <- c(data_treino_f$valor_real, data_teste_f$valor_real)
    dates_full <- c(data_treino_f$data, data_teste_f$data)
    
    adjust_full <- c(data_treino_f$valor_ajustado, rep(NA, length(data_teste_f$valor_predito)))
    prediction_full <- c(rep(NA, length(data_treino_f$valor_ajustado)), data_teste_f$valor_predito)
    
    plot_data_fold <- data.frame(
      data = dates_full,
      yvalues = yvalues_full,
      adjust = adjust_full,
      prediction = prediction_full
    )
    
    # --- Modificação aqui para o zoom ---
    data_inicio_zoom <- min(data_teste_f$data) - days(zoom_dias_antes_previsao)
    data_fim_zoom <- max(data_teste_f$data)
    

    
    p <- ggplot(plot_data_fold, aes(x = data)) +
      geom_point(aes(y = yvalues), color = "#888") +
      geom_line(aes(y = yvalues), color = "#111") + 
      geom_line(aes(y = adjust, color = "Ajustado"), linetype = "dashed") +
      geom_line(aes(y = prediction, color = "Previsto"), linetype = "dashed") +
      
      scale_color_manual(
        name = "Legenda",
        values = c("Ajustado" = "blue", "Previsto" = "green"),
        labels = c("Valores Ajustados", "Previsões")
      ) +
      labs(title = paste0("Série Temporal - Fold ", f),
           x = "Período", y = "Valor") +
      theme_minimal() +
      coord_cartesian(xlim = c(data_inicio_zoom, data_fim_zoom)) + # Zoom focado na previsão
      theme(text = element_text(size=10),
            legend.position = "bottom")
    
    plots_folds[[f]] <- p
  }
  
  cat("\n--- Validação Cruzada Concluída ---\n")
  
  return(
    list(
      metricas_por_fold = metricas_consolidadas,
      media_metricas = media_geral,
      predicoes_consolidadas = predicoes_consolidadas,
      ajustes_consolidados = ajustes_consolidados, 
      plots_folds = plots_folds 
    )
  )
}

# Função auxiliar para exibir os plots (opcional, você pode acessá-los diretamente pela lista)
plotar_todos_os_folds <- function(lista_plots) {
  for (i in seq_along(lista_plots)) {
    print(lista_plots[[i]])
  }
}