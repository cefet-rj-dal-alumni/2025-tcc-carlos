source('./wf_experiment.R')

datasets <- c('autuacao_mensal')
test_size <- 12

sw_size <- c(6, 12 ,24, 36, 48, 60)
preprocess <- list(ts_norm_an(), ts_norm_ean(), ts_norm_gminmax(), ts_norm_swminmax(), ts_norm_diff())
augment <- list(ts_aug_none())
ranges <- list(epochs=1000, lr=c(0.01,0.025,0.04))
params <- list(sw_size=sw_size, preprocess=preprocess, augment=augment, ranges=ranges)

results2 <- list()

for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*', '', ds), ds))
  for (ts in colnames(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'lstm')
    print(filename)
    run_ml(df[[ts]], filename, ts_lstm(), test_size=test_size, params=params)
    
    cases <- get_combinations(params)
    for (i in 1:length(cases)) {
      name <- get_names(filename, cases[[i]])
      print(name)
      tryCatch({
        novo_resultado <- run_ml(df[[ts]], name, ts_lstm(), test_size=test_size, params=cases[[i]], stgy=list(sa=TRUE, ro=FALSE))
        results2 <- rbind(results2, novo_resultado)
      }, error = function(e) {
        print('erro')
        error_dir <- sprintf('./error/%s', sub('-.*', '', ds))
        if (!dir.exists(error_dir))
          dir.create(error_dir, recursive = TRUE)
        error_file <- sprintf('./error/%s', name)
        writeLines(as.character(e), error_file)
      })
    }
    save(results2, file=sprintf('%s.rdata', sub('/', '/results/', filename)))
  }
}
