source('./wf_experiment.R')

datasets <- c('autuacao_mensal')
test_size <- 12

for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*', '', ds), ds))
  for (ts in colnames(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'arima')
    print(filename)
    run_ml(df[[ts]], filename, ts_arima(), test_size=test_size)
  }
}

