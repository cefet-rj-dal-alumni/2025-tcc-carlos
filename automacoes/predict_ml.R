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

args <- commandArgs(trailingOnly = TRUE)

target_path <- args[1]
model_path <- args[2]
predict_size <- as.numeric(args[3])

janela <- sub(".*sw-([0-9]+).*", "\\1", model_path) 
window_size <- as.integer(janela)
print(window_size)
window_size <- 35

#----------------------------------------


model <- readRDS(model_path)
df <- read.csv(target_path)

idx_numerico <- which(sapply(df, is.numeric))[1]
dados_serie <- df[, idx_numerico]

ultimos_dados <- tail(dados_serie, window_size)
ts_input <- ts_data(ultimos_dados, window_size)

prediction <- predict(model, ts_input, steps_ahead = predict_size)
prediction_vector <- as.vector(prediction)

if (!file.exists('output/prediction')){
  dir.create('output/prediction', recursive = TRUE)
}

write.table(prediction_vector, "output/prediction/prediction_result.csv", 
            sep = ",", row.names = FALSE, col.names = FALSE)

n_hist <- min(36, length(dados_serie))
historico_final <- tail(dados_serie, n_hist)

df_hist <- data.frame(
  x = 1:n_hist,
  y = historico_final,
  tipo = "Histórico"
)

df_pred <- data.frame(
  x = (n_hist + 1):(n_hist + predict_size),
  y = prediction_vector,
  tipo = "Previsão"
)

df_plot <- rbind(df_hist, df_pred)

plot_previsao <- ggplot(df_plot, aes(x = x, y = y, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("Histórico" = "black", "Previsão" = "green")) +
  theme_minimal() +
  labs(title = "Série Histórica Recente vs Previsão",
       subtitle = paste("Baseado nos últimos", n_hist, "registros"),
       x = "Pontos no Tempo", y = "Valor", color = "Legenda")

ggsave("output/prediction/plot_previsao.png", plot_previsao, width = 10, height = 6, dpi = 300)
print(model$model)
print("Previsão gerada com sucesso:")
print(prediction_vector)