# Rscript run_ml.R filname mlp,arima,knn 12

source('./wf_knn.R')
source('./wf_mlp.R')


args <- commandArgs(trailingOnly = TRUE)

input_file_path <- args[1]
models <- strsplit(args[2], ",")[[1]]
test_size <- as.numeric(args[3])

cat("Datasets:", input_file_path, "\n")
cat("Models:\n")
print(models)
cat("Test Size:", test_size, "\n")

for (i in seq_along(models)) {
  
  model <- tolower(models[i])
  cat("Executando modelo:", model, "\n")
  
  switch(model,
         

         "mlp" = {
           wf_mlp(test_size, input_file_path)
         },

         "knn" = {
           wf_knn(test_size, input_file_path)
         },
         

         
         {
           warning(paste("Modelo desconhecido:", model))
         }
  )
}