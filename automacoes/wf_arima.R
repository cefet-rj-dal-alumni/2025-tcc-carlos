source('./wf_experiment.R')

datasets <- c('autuacao_mensal')
test_size <- 12

results2 <- list()
for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*', '', ds), ds))
  for (ts in colnames(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'arima')
    print(filename)
    novo_resultado <- run_ml(df[[ts]], filename, ts_arima(), test_size=test_size, stgy=list(sa=TRUE, ro=FALSE, image=TRUE))
    results2 <- rbind(results2, novo_resultado)
  }
}

