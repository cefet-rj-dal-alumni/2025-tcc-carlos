# Para rodar: Rscript run_ml.R filename.csv mlp,arima,knn 12

source('./wf_knn.R')
source('./wf_mlp.R')
source('./wf_lstm.R')
source('./wf_elm.R')
source('./wf_rf.R')
source('./wf_arima.R')
source('./wf_svm.R')
source('./wf_conv1d.R')

args <- commandArgs(trailingOnly = TRUE)

# Verificação básica de argumentos
if (length(args) < 3) {
  stop("Uso correto: Rscript run_ml.R <arquivo> <modelos ou 'all'> <test_size>")
}

input_file_path <- args[1]
models <- unlist(strsplit(tolower(args[2]), ",")) # Converte logo para minúsculo e limpa a lista
test_size <- as.numeric(args[3])

cat("Datasets:", input_file_path, "\n")
cat("Models:", paste(models, collapse = ", "), "\n")
cat("Test Size:", test_size, "\n\n")

if ("all" %in% models) {
  cat("Executando todos os modelos...\n")
  wf_mlp(test_size, input_file_path)
  wf_knn(test_size, input_file_path)
  wf_elm(test_size, input_file_path)
  wf_lstm(test_size, input_file_path)
  wf_rf(test_size, input_file_path)
  wf_arima(test_size, input_file_path)
  wf_conv1d(test_size, input_file_path)
  wf_svm(test_size, input_file_path)
} else {
  for (model in models) {
    cat("Executando modelo:", model, "\n")
    
    switch(model,
           "mlp"    = wf_mlp(test_size, input_file_path),
           "knn"    = wf_knn(test_size, input_file_path),
           "elm"    = wf_elm(test_size, input_file_path),
           "lstm"   = wf_lstm(test_size, input_file_path),
           "rf"     = wf_rf(test_size, input_file_path),
           "arima"  = wf_arima(test_size, input_file_path),
           "conv1d" = wf_conv1d(test_size, input_file_path),
           "svm"    = wf_svm(test_size, input_file_path),
           {
             warning(paste("Modelo desconhecido:", model))
           }
    )
  }
}