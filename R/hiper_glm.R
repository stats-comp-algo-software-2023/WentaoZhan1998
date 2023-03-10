#' Sequence optimize
#'
#' Do sequence optimize
#'
#' @details
#'
#' @param design matrix, the design matrix X for the predictors.
#' @param outcome vector, the output Y for the response.
#' @param model the model to be used. The default is set as 'linear' to use the linear model;
#' otherwise use 'logit' for
#'
#' @return A list including the information of the fitted model
#'
#' @export
hiper_glm <- function(design, outcome, model = "linear") {
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  int = F
  if (model == 'linear'){
    if(sum(apply(design,2, var)<=1e-10)==0){
      design = cbind(rep(1, nrow(design)), design)
      int = T
    }
    if(sum(apply(design,2, var)<=1e-10)>1){
      stop(sprintf("Too many intercept rows"))
    }
    XtX_inv = solve(crossprod(design))
    beta = XtX_inv %*% crossprod(design, outcome)
    offsets = crossprod(t(design), beta)
    res = outcome - offsets
    beta = as.vector(beta)
    if(int){
      names(beta) = c('Intercept', paste0('x', as.character(1:(ncol(design)-1))))
    }
    else{
      names(beta) = paste0('x', as.character(1:ncol(design)))
    }
  }
  if (model == 'logit'){
    warning("`hiper_glm` for logit is yet to be implemented.")
  }
  warning("`hiper_glm` is yet to be implemented.")
  hglm_out <- list()
  hglm_out$model = model
  hglm_out$coefficients = beta
  hglm_out$residuals = as.vector(res)
  hglm_out$fitted.values = as.vector(offsets)
  hglm_out$XtX_inv = XtX_inv
  hglm_out$df_res = nrow(design) - ncol(design)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
