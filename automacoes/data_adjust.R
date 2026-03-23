
args <- commandArgs(trailingOnly = TRUE)

dataset_name <- args[1]
column_name <- args[2]
output_name <- args[3]

#antes de rodar, lembrar de importar os data frame d_autuacao em ~/dados/autuacoes_9var_full_data
df <- data.frame(df_autuacao)

# Converte o tipo das datas
df <- df %>%
  mutate(
    data = as.Date(data) 
  )


df_ordenado_valor_pago_total_mes <- df %>%
  filter(
    !is.na(valor_pago) &
      valor_pago > 0.0 &
      data_pagamento > "2014-01-01"
  mutate(
    mes_pagamento = floor_date(data_pagamento, "month")
  ) %>%
  group_by(mes_pagamento) %>%
  summarise(
    valor_pago_total = sum(valor_pago, na.rm = TRUE)
  ) %>%
  select(mes_pagamento, valor_pago_total)  

df_ordenado_valor_pago_total_mes$mes_pagamento <- as.Date(df_ordenado_valor_pago_total_mes$mes_pagamento)

ts_valor_pago_total_mes <- ts_data(df_ordenado_valor_pago_total_mes$valor_pago_total, 6)
samp <- ts_sample(ts_valor_pago_total_mes, test_size = 12)

io_train <- ts_projection(samp$train)
io_test <- ts_projection(samp$test)


