rw_fit <- function(obj, x, y = NULL, ...) {
  obj$model <- forecast::auto.arima(x, allowdrift = TRUE, allowmean = TRUE)
  obj$p <- 0
  obj$d <- 1
  obj$q <- 0
  obj$drift <- (NCOL(obj$model$xreg) == 1) && is.element("drift", names(obj$model$coef))
  params <- list(p = obj$p, d = obj$d, q = obj$q, drift = obj$drift)
  attr(obj, "params") <- params
  
  return(obj)
}