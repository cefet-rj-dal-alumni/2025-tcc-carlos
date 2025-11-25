library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(stringi)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(devtools)

#' @title Realiza validação cruzada para séries temporais com janela deslizante.
#'
#' @param serie_temporal Vetor numérico da série temporal.
#' @param datas_ts Vetor de datas correspondente à série temporal.
#' @param tam_janela_treino Número de observações na janela de treinamento (sw_size).
#' @param tam_janela_teste Número de observações na janela de teste (test_size).
#' @param step Número de observações para deslizar a janela.
#' @param modelo_func Função que retorna nova instância do modelo.
#'
#' @return Lista com métricas, predições e plots.
validacao_cruzada_plot_ts <- function(serie_temporal, datas_ts, tam_janela_treino, 
                                      tam_janela_teste, step, modelo_func) {
  
  resultados_folds <- list()
  predicoes_lista <- list()
  ajustes_lista <- list() 
  
  # Remover NAs
  serie_temporal <- na.omit(serie_temporal)
  n_total <- length(serie_temporal)
  
  pos_inicial <- 1
  fold <- 1
  
  cat("Iniciando a validação cruzada...\n")
  cat(sprintf("Total de observações: %d\n", n_total))
  cat(sprintf("Janela de treino: %d, Janela de teste: %d, Passo: %d\n\n", 
              tam_janela_treino, tam_janela_teste, step))
  
  while ((pos_inicial + tam_janela_treino + tam_janela_teste - 1) <= n_total) {
    
    # Definir índices
    train_size <- tam_janela_treino
    test_pos <- pos_inicial + train_size
    test_size <- tam_janela_teste
    
    fim_treino <- pos_inicial + train_size - 1
    inicio_teste <- test_pos
    fim_teste <- test_pos + test_size - 1
    
    cat(sprintf("Fold %d: Treino [%d-%d], Teste [%d-%d]\n", 
                fold, pos_inicial, fim_treino, inicio_teste, fim_teste))
    
    tryCatch({
      # Extrair dados de treino E teste para criar as janelas
      # O ts_data precisa de TODOS os dados (treino+teste) para gerar as janelas
      x_completo <- serie_temporal[pos_inicial:fim_teste]
      
      # Windowing - cria janelas deslizantes de tamanho tam_janela_treino
      xw <- ts_data(as.vector(x_completo), tam_janela_treino)
      xw <- na.omit(xw)
      
      # Separar treino e teste APÓS criar as janelas
      samp <- ts_sample(xw, test_size = tam_janela_teste)
      xy <- ts_projection(samp$train)
      xyt <- ts_projection(samp$test)
      
      cat(sprintf("  -> Treino: input=%s, output=%d\n", 
                  paste(dim(xy$input), collapse="x"), length(xy$output)))
      cat(sprintf("  -> Teste: input=%s, output=%d\n", 
                  paste(dim(xyt$input), collapse="x"), length(xyt$output)))
      
      # Treinar modelo
      model <- modelo_func()
      model <- fit(model, x = xy$input, y = xy$output)
      
      # Ajuste no conjunto de treino
      adjust <- as.vector(predict(model, xy$input))
      output_train <- as.vector(xy$output)
      
      cat(sprintf("  -> Ajuste: %d valores\n", length(adjust)))
      
      # Avaliar ajuste
      ev_adjust <- evaluate(model, output_train, adjust)
      
      # Predição steps ahead (usando primeira linha do teste)
      predicao <- as.vector(predict(model, x = xyt$input[1,], steps_ahead = tam_janela_teste))
      output_test <- as.vector(xyt$output)
      
      cat(sprintf("  -> Predição: %d valores\n", length(predicao)))
      
      # Garantir mesmo tamanho
      min_test_size <- min(length(predicao), length(output_test), tam_janela_teste)
      predicao <- predicao[1:min_test_size]
      output_test <- output_test[1:min_test_size]
      
      # Avaliar predição
      ev_prediction <- evaluate(model, output_test, predicao)
      
      # Armazenar métricas
      metricas <- data.frame(
        fold = fold,
        mse = ev_prediction$mse,
        rmse = ev_prediction$rmse,
        mae = ev_prediction$mae,
        mape = ev_prediction$mape,
        smape = ev_prediction$smape * 100,
        R2 = ev_prediction$R2
      )
      
      resultados_folds[[fold]] <- metricas
      
      cat(sprintf("  -> MSE: %.2f, RMSE: %.2f, MAPE: %.4f, SMAPE: %.2f%%\n\n", 
                  metricas$mse, metricas$rmse, metricas$mape, metricas$smape))
      
      # Armazenar ajustes (alinhados com as datas corretas)
      tamanho_ajuste <- length(adjust)
      inicio_ajuste <- fim_treino - tamanho_ajuste + 1
      
      if (tamanho_ajuste > 0 && inicio_ajuste >= 1 && inicio_ajuste <= length(datas_ts)) {
        ajustes_lista[[fold]] <- data.frame(
          data = datas_ts[inicio_ajuste:fim_treino],
          valor_real = output_train,
          valor_ajustado = adjust,
          fold = fold,
          stringsAsFactors = FALSE
        )
      }
      
      # Armazenar predições
      if (min_test_size > 0) {
        predicoes_lista[[fold]] <- data.frame(
          data = datas_ts[inicio_teste:(inicio_teste + min_test_size - 1)],
          valor_real = output_test,
          valor_predito = predicao,
          fold = fold,
          stringsAsFactors = FALSE
        )
      }
      
    }, error = function(e) {
      cat(sprintf("  -> ERRO no Fold %d: %s\n\n", fold, e$message))
      
      # Criar dados vazios em caso de erro
      metricas <- data.frame(
        fold = fold,
        mse = NA, rmse = NA, mae = NA, mape = NA, smape = NA, R2 = NA
      )
      resultados_folds[[fold]] <<- metricas
    })
    
    # Deslizar janela
    pos_inicial <- pos_inicial + step
    fold <- fold + 1
  }
  
  cat("--- Consolidando resultados ---\n")
  
  # Consolidar métricas
  metricas_consolidadas <- dplyr::bind_rows(resultados_folds)
  
  # Converter colunas para numérico (caso necessário)
  cols_numericas <- names(metricas_consolidadas)[names(metricas_consolidadas) != "fold"]
  for (col in cols_numericas) {
    if (is.character(metricas_consolidadas[[col]])) {
      metricas_consolidadas[[col]] <- gsub(",", ".", metricas_consolidadas[[col]])
      metricas_consolidadas[[col]] <- as.numeric(metricas_consolidadas[[col]])
    }
  }
  
  # Calcular médias
  media_geral <- metricas_consolidadas %>%
    select(-fold) %>%
    summarise_all(mean, na.rm = TRUE)
  
  # Consolidar predições e ajustes
  predicoes_consolidadas <- NULL
  ajustes_consolidados <- NULL
  
  if (length(predicoes_lista) > 0) {
    predicoes_consolidadas <- dplyr::bind_rows(predicoes_lista)
  }
  
  if (length(ajustes_lista) > 0) {
    ajustes_consolidados <- dplyr::bind_rows(ajustes_lista)
  }
  
  # Gerar plots para cada fold
  plots_folds <- list()
  
  if (!is.null(predicoes_consolidadas) && nrow(predicoes_consolidadas) > 0) {
    for (f in unique(predicoes_consolidadas$fold)) {
      
      data_treino_f <- NULL
      data_teste_f <- NULL
      
      if (!is.null(ajustes_consolidados)) {
        data_treino_f <- ajustes_consolidados %>% filter(fold == f)
      }
      
      data_teste_f <- predicoes_consolidadas %>% filter(fold == f)
      
      if (nrow(data_teste_f) == 0) next
      
      # Combinar dados completos
      if (!is.null(data_treino_f) && nrow(data_treino_f) > 0) {
        yvalues_full <- c(data_treino_f$valor_real, data_teste_f$valor_real)
        dates_full <- c(data_treino_f$data, data_teste_f$data)
        adjust_full <- c(data_treino_f$valor_ajustado, rep(NA, nrow(data_teste_f)))
        prediction_full <- c(rep(NA, nrow(data_treino_f)), data_teste_f$valor_predito)
      } else {
        yvalues_full <- data_teste_f$valor_real
        dates_full <- data_teste_f$data
        adjust_full <- rep(NA, nrow(data_teste_f))
        prediction_full <- data_teste_f$valor_predito
      }
      
      plot_data_fold <- data.frame(
        data = dates_full,
        yvalues = yvalues_full,
        adjust = adjust_full,
        prediction = prediction_full,
        stringsAsFactors = FALSE
      )
      
      # Criar plot SEM zoom (mostra toda a série)
      p <- ggplot(plot_data_fold, aes(x = data)) +
        geom_point(aes(y = yvalues), color = "#888888", size = 2, alpha = 0.6) +
        geom_line(aes(y = yvalues), color = "#000000", linewidth = 0.8) + 
        geom_line(aes(y = adjust, color = "Ajustado"), linewidth = 1.2, na.rm = TRUE) +
        geom_line(aes(y = prediction, color = "Previsto"), linewidth = 1.2, na.rm = TRUE) +
        scale_color_manual(
          name = "",
          values = c("Ajustado" = "#0066CC", "Previsto" = "#00CC66"),
          labels = c("Valores Ajustados (Treino)", "Previsões (Teste)")
        ) +
        labs(
          title = paste0("Série Temporal - Fold ", f),
          subtitle = sprintf("Treino: %d obs | Teste: %d obs", 
                             ifelse(!is.null(data_treino_f), nrow(data_treino_f), 0), 
                             nrow(data_teste_f)),
          x = "Período", 
          y = "Valor"
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 11),
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 10, color = "#666666"),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          panel.grid.minor = element_blank()
        )
      
      plots_folds[[f]] <- p
    }
  }
  
  cat("--- Validação Cruzada Concluída ---\n\n")
  
  # Retornar resultados
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

#' @title Plota todos os folds da validação cruzada
#' @param lista_plots Lista de objetos ggplot retornada pela validação cruzada
plotar_todos_os_folds <- function(lista_plots) {
  if (length(lista_plots) == 0) {
    cat("Nenhum plot disponível.\n")
    return(invisible(NULL))
  }
  
  for (i in seq_along(lista_plots)) {
    cat(sprintf("\nExibindo Fold %d...\n", i))
    print(lista_plots[[i]])
  }
}