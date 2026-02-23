source('./wf_experiment.R')

wf_rf <- function(test_size, datasets){
  
  sw_size <- c(48, 52)
  #preprocess <- list(ts_norm_an(), ts_norm_ean(), ts_norm_gminmax(), ts_norm_swminmax(), ts_norm_diff())
  preprocess <- list(ts_norm_an(), ts_norm_ean()) 
  augment <- list(ts_aug_none())
  #ranges <- list(nodesize=1:10, ntree=seq(100, 500, 100))
  ranges <- list(nodesize=c(10, 20), ntree=100)
  params <- list(sw_size=sw_size, preprocess=preprocess, augment=augment, ranges=ranges)
  
  results2 <- list()
  
  for (ds in datasets) {
    
    create_directories('output')
    df <- read.csv(ds)
    
    for (ts in colnames(df)) {
      
      base <- basename(ds)
      base <- sub("\\.csv$", "", base)
      
      filename <- sprintf('%s/%s_%s', sub('-.*', '', 'output'), ts, 'rf')
      print(filename)
      cases <- get_combinations(params)
      
      for (i in 1:length(cases)) {
        name <- get_names(filename, cases[[i]])
        print(name)
        tryCatch({
          novo_resultado <- run_ml(df[[ts]], name, ts_rf(), test_size=test_size, params=cases[[i]], stgy=list(sa=TRUE, ro=FALSE, image=FALSE), save_model=FALSE)
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
      
      # Ordenar results2 por R² decrescente e pegar os 3 melhores
      if (nrow(results2) > 0) {
        # Ordenar por R² decrescente
        results2_ordenado <- results2[order(-results2$r2), ]
        
        melhor_modelo <- head(results2_ordenado, 1)
        
        # Rodar run_ml_with_image para os 3 melhores
        cases_melhor_3 <- list()
        
        for (j in 1:nrow(melhor_modelo)) {
          cases_melhor_3 <- get_params_from_name(melhor_modelo$instance[j])
          
          sw_size2 <- c(cases_melhor_3$params$sw_size)
          
          preprocess2 <- switch(
            cases_melhor_3$params$ranges$norm,
            "an" = list(ts_norm_an()),
            "ean" = list(ts_norm_ean()),
            "gminmax" = list(ts_norm_gminmax()),
            "swminmax" = list(ts_norm_swminmax()),
            "diff" = list(ts_norm_diff()),
            "default" = list(ts_norm_none())
          )
          
          augment2 <- list(ts_aug_none())
          
          ranges2 <- list(nodesize = as.numeric(cases_melhor_3$params$ranges$nodesize),
                          ntree = as.numeric(cases_melhor_3$params$ranges$ntree))

          
          params2 <- list(sw_size=sw_size2, preprocess=preprocess2, augment=augment2, ranges=ranges2)
          
          name2 <- get_names(filename, params2)
          
          run_ml(df[[ts]], name2, ts_rf(), test_size=test_size, params=params2, stgy=list(sa=TRUE, ro=FALSE, image=TRUE), save_model=TRUE)
        }
      }
      
      save(results2, file=sprintf('%s.rdata', sub('/', '/results/', filename)))
    }
  }
}