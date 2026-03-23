#-----------------------------------------------
#------------------ LIBRARIES ------------------
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(stringi)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(devtools)
set.seed(120770)
#-----------------------------------------------


#-----------------------------------------------
#-------------- GENERAL FUNCTIONS --------------
create_directories <- function(path) {
  dir_name <- c('graphics', 'hyper', 'results', 'models')
  for (name in sprintf('%s/%s', path, dir_name)) {
    if (!file.exists(name))
      dir.create(name, recursive = TRUE)
  }
}


describe <- function(obj) {
  if (is.null(obj))
    return('')
  else
    return(as.character(class(obj)[1]))
}

get_preprocess <- function(preprocess) {
  func_map <- list( ts_norm_gminmax  = ts_norm_gminmax,
                    ts_norm_swminmax = ts_norm_swminmax,
                    ts_norm_an       = ts_norm_an,
                    ts_norm_ean      = ts_norm_ean,
                    ts_norm_diff     = ts_norm_diff )
  return(func_map[[preprocess]]) 
}


get_combinations <- function(params=NULL) {
  combinations <- expand.grid(sw_size=params$sw_size, preprocess=params$preprocess, augment=params$augment)
  ml_combinations <- do.call(expand.grid, params$ranges)
  result <- list()
  for (i in 1:nrow(combinations)) {
    for (j in 1:nrow(ml_combinations)) {
      ranges <- list()
      for (param in names(ml_combinations))
        ranges[[param]] <- ml_combinations[[param]][j]
      params <- list()
      for (param in names(combinations))
        params[[param]] <- combinations[[param]][i]
      params$ranges <- ranges
      result <- append(result, list(params))
    }
  }
  return(result)
}


get_names <- function(filename, params) {
  sw <- sprintf('sw-%s', params$sw_size)
  dn <- gsub('_', '-', gsub('ts_', '', describe(params$preprocess[[1]])))
  da <- gsub('_', '-', gsub('ts_', '', describe(params$augment[[1]])))
  ml <- ''
  for (p in names(params$ranges)) {
    ml <- ifelse( ml == '',
                  sprintf('%s-%s', gsub('[^a-zA-Z0-9]', '', p), params$ranges[[p]]),
                  paste(ml, sprintf('%s-%s', gsub('[^a-zA-Z0-9]', '', p), params$ranges[[p]]), sep='_') )
  }
  filename <- paste(filename, sw, dn, da, ml, sep='_')
  return (filename)
}


wf_experiment <- function( filename,
                           base_model,
                           sw_size = c(0),
                           input_size = c(0),
                           preprocess = list(ts_norm_none()),
                           augment = list(ts_aug_none()),
                           ranges = list() ) {
  obj <- dal_transform()
  obj$filename <- filename
  obj$base_model <- base_model
  obj$sw_size <- sw_size
  obj$input_size <- input_size
  obj$preprocess <- preprocess
  obj$augment <- augment
  obj$ranges <- ranges
  class(obj) <- append('wf_experiment', class(obj))
  return(obj)
}


compute_performance <- function(model, true, pred) {
  test_size <- length(pred)
  mse <- smape <- r2 <- NULL
  
  performance <- evaluate(model, true[1:test_size], pred[1:test_size])
  
  #calcula a media para o test size
  return(list(mse = performance$mse, 
              smape = 100 * performance$smape, 
              r2 = performance$R2))
  
  #estava fazendo a media acumulada dos pontos futuros
  #for (j in 1:test_size) {
  #  performance <- evaluate(model, true[1:j], pred[1:j])
  #  mse <- c(mse, performance$mse)
  #  smape <- c(smape, 100*performance$smape)
  #  r2 <- c(r2, performance$R2)
  #}
  
  #return(list(mse=mse, smape=smape, r2=r2))
}


save_image <- function(obj, strategy) {
  jpeg(sprintf('%s_%s.jpg', sub('/', '/graphics/', obj$filename), strategy), width = 1280, height = 720)
  yvalues <- c(obj$train, obj$test)
  xlabels <- 1:length(yvalues)
  if(!is.null(names(yvalues)))
    xlabels <- as.integer(names(yvalues))
  grf <- plot_ts_pred(x = xlabels, y = yvalues, yadj=obj$adjust, ypre=obj$prediction)
  plot(grf)
  dev.off()
}
#-----------------------------------------------


#-----------------------------------------------
#----------------- MLM & ARIMA -----------------
run_ml <- function( x,
                    filename,
                    base_model,
                    test_size,
                    params=list(sw_size=c(0), preprocess=list(ts_norm_none())),
                    stgy = list(ro=TRUE, sa=TRUE, image=FALSE),
                    save_model = FALSE
                    ) {
  # fit ----
  train_size <- length(x)-test_size
  model <- wf_experiment(filename, base_model)
  best_model <- train_ml(model, x[1:train_size], params)
  
  if (is.null(best_model)) {
    #params$preprocess <- list(ts_norm_gminmax())
    #params$sw_size <- c(10)
    #best_model <- train_ml(model, x[1:train_size], params)
  }
  # Predict
  results <- NULL
  #rolling origin (ro) ------------------------------------------
  if (stgy$ro == TRUE){
    for (j in 1:test_size) {
      result <- test_ml(best_model, x, test_pos=train_size+1, test_size=j, steps_ahead=1)
      results <- rbind(results, result$df)
    }
    save_image(result$model, 'ro')
  }
  #steps ahead (sa) -------------------------------------------
  if (stgy$sa == TRUE){
    result <- test_ml(best_model, x, test_pos=train_size+1, test_size=test_size, steps_ahead=test_size, save_model)
    results <- rbind(results, result$df)
    if (stgy$image == TRUE){
      print(sprintf("Criando gráfico para '%s'.", filename))
      save_image(result$model, 'sa')
    }
  }
  
  # Results
  return(results)
  #save(results, file=sprintf('%s.rdata', sub('/', '/results/', filename)))
}

#-------------------- Train --------------------
train_ml <- function(obj, x, params) {
  # Initialization
  best_model <- hyperparameters <- NULL
  best_error <- 200
  x <- na.omit(x)
  for (sw in params$sw_size) {
    # Windowing
    obj$sw_size <- sw
    obj$train <- x[obj$sw_size:length(x)]
    obj$obs <- as.character(obj$train)
    if (length(names(obj$train)) > 0)
      obj$obs <- names(obj$train)[length(obj$train)]
    xw <- ts_data(as.vector(x), obj$sw_size)
    xw <- na.omit(xw)
    xy <- ts_projection(xw)
    for (dn in params$preprocess) {
      # Set parameters
      input_size <- sw #sw_size[sw_size <= sw]
      if (as.character(describe(dn)) == 'ts_norm_diff') input_size <- input_size - 1
      if (as.character(describe(obj$base_model)) == 'ts_conv1d') input_size <- input_size - 1
      if (as.character(describe(obj$base_model)) == 'ts_lstm') input_size <- input_size - 1
      ranges <- list(input_size=input_size, preprocess=list(dn), augment=params$augment, ranges=params$ranges)
      obj <- set_params(obj, ranges)
      
      # Tuning
      if (obj$sw_size != 0) {
        tune <- ts_integtune(obj$input_size, obj$base_model, folds=10, range=obj$ranges, preprocess=obj$preprocess, augment=obj$augment)
        tryCatch({
          obj$model <- fit(tune, xy$input, xy$output, obj$ranges)
          obj$input_size <- obj$model$input_size
          obj$preprocess <- obj$model$preprocess
          obj$augment <- attr(obj$model, 'augment')
          attr(obj$model, 'hyperparameters')$window_size <- obj$sw_size
          hyperparameters <- rbind(hyperparameters, attr(obj$model, 'hyperparameters'))
        }, error = function(e) {
          return(NULL)
        })
      } else {
        obj$model <- fit(obj$base_model, x=xy$input, y=xy$output)
        obj$input_size <- 0
        obj$preprocess <- ts_norm_none()
        obj$augment <- ts_aug_none()
      }
      if (!is.null(obj$model)) {
        # Evaluation
        obj$adjust <- as.vector(predict(obj$model, xy$input))
        obj$ev_adjust <- evaluate(obj$model, as.vector(xy$output), obj$adjust)
        error <- obj$ev_adjust$smape*100
        if (!is.na(error) && error < best_error) {
          best_error <- error
          best_model <- obj
        }
      }
    }
  }
  
  #Save parameters
  params <- attr(best_model$model, 'params')
  if (!is.null(hyperparameters)) {
    #salvar hiperparametros
    #save(hyperparameters, file=sprintf('%s_hparams.rdata', sub('/', '/hyper/', obj$filename)))
    params$window_size <- best_model$sw_size
  }
  #salvar parametros
  #write.csv(params, file=sprintf('%s_params.csv', sub('/', '/hyper/', obj$filename)), row.names=FALSE)
  
  #Return optimized model
  return(best_model)
}

#------------------- Predict -------------------
test_ml <- function(obj, x, test_pos, test_size, steps_ahead=1, save_model = FALSE) {
  x <- na.omit(x)
  # Windowing
  obj$test <- x[test_pos:(test_pos+test_size-1)]
  if (obj$sw_size != 0) {
    xwt <- ts_data(as.vector(x[(test_pos-obj$sw_size+1):(test_pos+test_size-1)]), obj$sw_size)
  } else
    xwt <- ts_data(as.vector(x[test_pos:(test_pos+test_size-1)]), obj$sw_size)
  xwt <- na.omit(xwt)
  xyt <- ts_projection(xwt)    
  output <- as.vector(xyt$output)
  # Testing
  if (steps_ahead == 1)  {
    strategy <- 'ro'
    obj$prediction <- predict(obj$model, x=xyt$input, steps_ahead=steps_ahead)
    index <- test_size
  } else {
    strategy <- 'sa'
    obj$prediction <- predict(obj$model, x=xyt$input[1,], steps_ahead=steps_ahead)
    index <- 1:test_size
    output <- output[index]
  }
  # Evaluation
  obj$prediction <- as.vector(obj$prediction)
  obj$ev_prediction <- compute_performance(obj$model, obj$test, obj$prediction)
  
  # Results
  experiment <- strsplit(obj$filename, '_')[[1]]
  
  if (FALSE) {
    encoder <- obj$model$encoder
    result <- data.frame( name = as.character(describe(obj$base_model)),
                          instance = sub('^[^_]*_[^_]*_', '', obj$filename),
                          dataset = sub('/.*', '', experiment[2]),
                          ts = sub('^.*?/', '', experiment[1]),
                          model = experiment[3],
                          preprocess = sub('.*_(.*)', '\\1', describe(obj$preprocess)),
                          augment = sub('.*_(.*)', '\\1', describe(obj$augment)),
                          window_size = obj$sw_size,
                          test_size = index,
                          strategy = strategy,
                          true = output[index],
                          pred = obj$prediction[index],
                          mse = obj$ev_prediction$mse[index],
                          smape = obj$ev_prediction$smape[index],
                          r2 = obj$ev_prediction$r2[index] )
  } else {
    result <- data.frame( 
      name = as.character(describe(obj$base_model)),
      instance = sub('^[^_]*_[^_]*_', '', obj$filename),
      dataset = sub('/.*', '', experiment[2]),
      ts = sub('^.*?/', '', experiment[1]),
      model = experiment[3],
      preprocess = sub('.*_(.*)', '\\1', describe(obj$preprocess)),
      augment = sub('.*_(.*)', '\\1', describe(obj$augment)),
      window_size = obj$sw_size,
      test_size = test_size,
      strategy = strategy,
      mse = mean(obj$ev_prediction$mse, na.rm = TRUE),
      smape = mean(obj$ev_prediction$smape, na.rm = TRUE),
      r2 = mean(obj$ev_prediction$r2, na.rm = TRUE)
    )
  }
  
  KERAS_MODELS <- c("lstm", "conv1d")
  
  if (save_model == TRUE) {
    folder <- "output/models"
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    
    clean_filename <- basename(obj$filename)
    final_path     <- file.path(folder, paste0(clean_filename, ".rds"))
    
    tag_algoritmo <- tolower(clean_filename)
    is_keras      <- any(sapply(KERAS_MODELS, function(k) grepl(k, tag_algoritmo)))
    
    if (is_keras) {
      library(reticulate)
      torch <- import("torch")
      
      pt_path <- file.path(folder, paste0(clean_filename, ".pt"))
      torch$save(obj$model$model$state_dict(), pt_path)
      message(sprintf("Pesos PyTorch salvos em: %s", pt_path))
      
      # Salva metadados para reconstrução na carga
      meta_path <- file.path(folder, paste0(clean_filename, "_meta.rds"))
      meta <- list(
        input_size  = obj$model$input_size,
        sw_size     = obj$sw_size,
        preprocess  = obj$preprocess,
        augment     = obj$augment,
        model_class = class(obj$model)
      )
      saveRDS(meta, file = meta_path)
      message(sprintf("Metadados salvos em: %s", meta_path))
    }
    
    saveRDS(obj$model, file = final_path)
    print(sprintf("Modelo %s salvo com sucesso.", obj$filename))
  }
  
  return(list(df=result, model=obj))
}








get_params_from_name <- function(filename_full) {
  # Divide o nome por "_"
  parts <- strsplit(filename_full, "_")[[1]]
  
  # Função interna para buscar valor por prefixo (ex: "sw-")
  extract_val <- function(prefix) {
    match <- parts[grep(paste0("^", prefix), parts)]
    if(length(match) > 0) return(gsub(prefix, "", match[1]))
    return(NULL)
  }
  
  # Extrai os fixos de forma segura
  sw_size   <- as.numeric(extract_val("sw-"))
  norm_val  <- extract_val("norm-")
  aug_val   <- extract_val("aug-")
  
  # Extrai todo o resto que tiver "-" como parâmetros do ranges
  ranges <- list()
  # Pega partes que tem "-" mas não são os fixos acima
  others <- parts[grep("-", parts)]
  others <- others[!grepl("^(sw-|norm-|aug-|dn-|da-)", others)]
  
  for (item in others) {
    kv <- strsplit(item, "-")[[1]]
    if(length(kv) == 2) ranges[[kv[1]]] <- kv[2]
  }
  
  # Garante que o 'norm' esteja no ranges para o seu switch ler
  ranges$norm <- norm_val
  
  return(list(
    params = list(
      sw_size = sw_size,
      ranges = ranges
    )
  ))
}
#-----------------------------------------------
