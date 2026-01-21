# Rscript run_ml.R filname mlp,arima,knn 12

args <- commandArgs(trailingOnly = TRUE)

datasets <- args[1]
models <- strsplit(args[2], ",")[[1]]
test_size <- as.numeric(args[3])

cat("Datasets:", datasets, "\n")
cat("Models:\n")
print(models)
cat("Test Size:", test_size, "\n")

for (i in seq_along(models)) {
  
  model <- tolower(models[i])
  cat("Executando modelo:", model, "\n")
  
  switch(model,
         
         "arima" = {
           env <- new.env()
           env$datasets  <- datasets
           source("wf_arima.R", local = env)
         },
         
         "mlp" = {
           env <- new.env()
           env$datasets  <- datasets
           source("wf_mlp.R", local = env)
         },
         
         "knn" = {
           env <- new.env()
           env$datasets  <- datasets
           source("wf_knn.R", local = env)
         },
         
         "lstm" = {

         },
         
         "conv1d" = {
           
         },
         
         "elm" = {
           
         },
         
         "svm" = {
           
         },

         "rf" = {
           
         },
         
         {
           warning(paste("Modelo desconhecido:", model))
         }
  )
}