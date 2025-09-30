source('./wf_experiment.R')

datasets <- c('autuacao_semanal')
test_size <- 12

sw_size <- c(6,8,10)
preprocess <- list(ts_norm_an(), ts_norm_ean(), ts_norm_gminmax(), ts_norm_swminmax(), ts_norm_diff())
augment <- list(ts_aug_none())
ranges <- list(kernel=c('radial','linear'), epsilon=seq(0,0.8,0.2), cost=seq(20,100,20))
params <- list(sw_size=sw_size, preprocess=preprocess, augment=augment, ranges=ranges)

for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*', '', ds), ds))
  for (ts in colnames(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'svr')
    print(filename)
    run_ml(df[[ts]], filename, ts_svm(), test_size=test_size, params=params)
    
    #cases <- get_combinations(params)
    #for (i in 1:length(cases)) {
    #  name <- get_names(filename, cases[[i]])
    #  print(name)
    #  tryCatch({
    #    run_ml(df[[ts]], name, ts_svm(), test_size=test_size, params=cases[[i]])
    #  }, error = function(e) {
    #    print('erro')
    #    error_dir <- sprintf('./error/%s', sub('-.*', '', ds))
    #    if (!dir.exists(error_dir))
    #      dir.create(error_dir, recursive = TRUE)
    #    error_file <- sprintf('./error/%s', name)
    #    writeLines(as.character(e), error_file)
    #  })
    #}
  }
}
