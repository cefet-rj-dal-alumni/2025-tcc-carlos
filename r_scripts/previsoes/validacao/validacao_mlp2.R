library(daltoolbox)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(tspredit)

setwd("~/tcc_carlos/r_scripts/previsoes/validacao")

test_size <- 12
sw_size <- 36

filename <- "autuacao_mensal.csv"
df <- read.csv(filename) 

if (nrow(df) == 0) {
  stop("O data.frame 'df' está vazio. Não é possível prosseguir.")
}

criar_diretorio_imagens <- function() {
  diretorio <- "imagens2"
  if (!dir.exists(diretorio)) {
    dir.create(diretorio)
    # Apenas criamos, sem imprimir status, para manter a saída limpa
  }
}

salvar_plot <- function(plot_obj, fold_index) {
  diretorio <- "imagens2"
  nome_arquivo <- sprintf("%s/%s_predicao.png", diretorio, fold_index)
  ggsave(nome_arquivo, plot_obj, width = 16, height = 10, units = "in")
}

# Seleciona dados
x <- df$valor_pago
ts <- ts_data(x, sw_size)
samp <- ts_sample(ts, test_size = test_size)
io_train <- ts_projection(samp$train)
io_test <- ts_projection(samp$test)

# Modelo e Treinamento
model <- ts_mlp(ts_norm_gminmax(), input_size=sw_size, size=1, decay=0.1, maxit=1000)
model <- fit(model, x=io_train$input, y=io_train$output)

# Previsão
adjust <- predict(model, io_train$input)
prediction <- predict(model, x=io_test$input[1, ], steps_ahead=test_size)

# Avaliação do Teste e Armazenamento (Sem impressão aqui)
ev_test <- evaluate(model, as.vector(io_test$output), as.vector(prediction))
print(ev_test)

# Plotagem e Salvamento
yvalues <- c(as.vector(io_train$output), as.vector(io_test$output))
plot_obj <- plot_ts_pred(y=yvalues, yadj=as.vector(adjust), ypre=as.vector(prediction)) + 
  theme(text = element_text(size=16))

fold_index <- 1

salvar_plot(plot_obj, "all")

saveRDS(model, file = "modelo_mlp_sw-36_size-1_decay-01_maxit_1000.rds")
