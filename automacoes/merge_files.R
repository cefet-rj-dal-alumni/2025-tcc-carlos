library(purrr)
library(dplyr)


merge_files <- function(path, model=NULL){
  
  current_path <- getwd()
  datasets <- c('autuacao_semanal')
  df <- list()
  
  for (ds in datasets) {
    merge_path <- paste(current_path, ds, path, sep="/")
    filename <- sprintf('%s/%s/%s_combined_%s.rdata', current_path, ds, ds, path)
    if (!is.null(model)) filename <- gsub('.rdata', sprintf('_%s.rdata', gsub('[^a-zA-Z0-9]', '', model)), filename)
    print(ds)
    if (file.exists(filename)) file.remove(filename)
    
    pattern <- ifelse(!is.null(model), paste0('.*', model, '.*\\.rdata'), '.rdata')
    all_files <- list.files(merge_path, full.names=TRUE, recursive=TRUE, pattern=pattern) %>%
      map_df(~ get(load(file=.x)))
    
    df[[ds]] <- all_files
  }
  
  df <- bind_rows(df)
  filename <- sprintf('%s/combined_%s.rdata', current_path, path)
  if (!is.null(model)) filename <- gsub('.rdata', sprintf('_%s.rdata', gsub('[^a-zA-Z0-9]', '', model)), filename)
  save(df, file = filename)
  write.csv2(df, file = gsub('rdata', 'csv', filename), row.names=FALSE)
  print(filename)
}


merge_files('results')
#merge_files('hyper', model='rewts-')
#merge_files('hyper', model='rew_')