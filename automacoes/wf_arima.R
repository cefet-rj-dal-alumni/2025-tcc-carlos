source('./wf_experiment.R')

wf_arima <- function(test_size, datasets){

  results2 <- list()
  for (ds in datasets) {
    create_directories('output')
    df <- read.csv(ds)
    for (ts in colnames(df)) {
      filename <- sprintf('%s/%s_%s', sub('-.*', '', 'output'), ts, 'arima')
      print(filename)
      novo_resultado <- run_ml(df[[ts]], filename, ts_arima(), test_size=test_size, stgy=list(sa=TRUE, ro=FALSE, image=TRUE), save_model = TRUE)
      results2 <- rbind(results2, novo_resultado)
    }
  }
}

