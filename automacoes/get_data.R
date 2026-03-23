library(bigrquery)

project_id <- 'rj-smtr'

# Consulta otimizada: Filtra, trunca a data e agrupa direto no BigQuery
sql_query_agregada <- "
SELECT 
    DATE_TRUNC(data_pagamento, MONTH) AS mes_pagamento,
    SUM(valor_pago) AS valor_pago_total
FROM `rj-smtr.transito.autuacao`
WHERE valor_pago IS NOT NULL 
  AND valor_pago > 0.0 
  AND data_pagamento > '2026-01-01'
  AND data_pagamento < '2026-05-01'
GROUP BY 1
ORDER BY 1 ASC
"

# Executa e baixa o resultado já processado
results <- bq_project_query(project_id, sql_query_agregada)
df_ordenado_valor_pago_total_mes <- bq_table_download(results)