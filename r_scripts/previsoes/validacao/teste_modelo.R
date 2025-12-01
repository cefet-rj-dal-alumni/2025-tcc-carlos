library(daltoolbox)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(tspredit)
library(scales)
# --- Definição das Funções ---

setwd("~/tcc_carlos/r_scripts/previsoes/validacao")

criar_diretorio_imagens <- function() {
  diretorio <- "imagem_teste"
  if (!dir.exists(diretorio)) {
    dir.create(diretorio)
    # Apenas criamos, sem imprimir status, para manter a saída limpa
  }
}

salvar_plot <- function(plot_obj, nome) {
  diretorio <- "imagem_teste"
  nome_arquivo <- sprintf("%s/%s_predicao.png", diretorio, nome)
  ggsave(nome_arquivo, plot_obj, width = 16, height = 10, units = "in")
}
#---------------------------------


model <- readRDS("modelo_mlp_sw-36_size-1_decay-01_maxit_1000.rds")
df <- read.csv("autuacao_mensal.csv")

window_size <- 36
steps_ahead <- 12 

len_input <- length(df$valor_pago_total)

x <- df$valor_pago_total[(len_input - window_size) : (len_input - 1)]

ts <- ts_data(x, window_size)
projection <- ts_projection(ts)

input_dates <- df$mes_pagamento[(len_input - window_size) : (len_input - 1)]

predicted_values <- predict(model, x=projection$input[1,], steps_ahead=steps_ahead)


last_historical_date <- as.Date(input_dates_chr[length(input_dates_chr)], format = "%Y-%m-%d")

future_dates <- seq(last_historical_date + months(1), by = "month", length.out = steps_ahead)

df_history <- data.frame(
  Data = as.Date(input_dates_chr, format = "%Y-%m-%d"),
  Valor = x,
  Tipo = "Histórico"
)

df_forecast <- data.frame(
  Data = future_dates,
  Valor = as.vector(predicted_values),
  Tipo = "Previsão"
)

df_plot <- bind_rows(df_history, df_forecast)

criar_diretorio_imagens()

plot_obj <- ggplot(df_plot, aes(x = Data, y = Valor, color = Tipo)) +
  geom_line(data = df_history, aes(x = Data, y = Valor), color = "black", linewidth = 1.0) +
  geom_line(data = df_forecast, aes(x = Data, y = Valor), color = "#226600", linetype = "dotted", linewidth = 1.0) +
  geom_point(data = df_history, aes(x = Data, y = Valor), color = "black", size = 3) +
  geom_point(data = df_forecast, aes(x = Data, y = Valor), color = "#22cc00", size = 3) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M", big.mark = ".")
  )
  labs(
    title = "Previsão de Valor Pago Total (MLP)",
    subtitle = paste0("Horizonte de Previsão: ", steps_ahead, " meses"),
    x = "Data",
    y = "Valor Pago Total",
    color = "Série"
  ) +
  #theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_color_manual(values = c("Histórico" = "black", "Previsão" = "#00cc88"))

# Salva o plot
salvar_plot(plot_obj, "previsao_final")

