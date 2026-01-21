# 1. Carregar o pacote necessário
library(tidyverse)

# 2. Ler o arquivo original
dados <- read.csv("autuacao_mensal.csv")

# 3. Converter a coluna para Data e filtrar
# Usamos >= para incluir o dia primeiro de janeiro de 2015 em diante
dados_filtrados <- dados %>%
  mutate(mes_pagamento = as.Date(mes_pagamento)) %>%
  filter(mes_pagamento >= "2015-01-01")

# 4. Salvar o novo CSV
write.csv(dados_filtrados, "autuacao_mensal_filtrada.csv", row.names = FALSE)