library(daltoolbox)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(tspredit)

create_directories <- function(folder_name) {
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
}

save_plot <- function(plot_obj, filename) {
  nome_arquivo <- sprintf("%s.jpeg", filename)
  ggsave(nome_arquivo, plot_obj, width = 16, height = 10, units = "in")
}

save_plot2 <- function(xvalues, yvalues, adjust, predict){
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
  
  return(plot_obj)
}

salvar_plot <- function(plot_obj, folder, nome) {
  nome_arquivo <- sprintf("%s/%s.png", folder, nome)
  ggsave(nome_arquivo, plot_obj, width = 10, height = 6, units = "in")
}

train_model <- function(){
  
}

test_model <- function(){
  
}