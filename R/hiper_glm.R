#' Gradient for the likelihood under linear model
#'
#' Calculate the gradient
#'
#' @details
#'
#' @param design matrix, the design matrix X for the predictors.
#' @param outcome vector, the output Y for the response.
#' @param beta vector, the potent8ial
#'
#' @return Gradient for the likelihood under linear model

grad_linear = function(design, outcome, beta, noise_var = 1){
  -(crossprod(design, outcome) - crossprod(crossprod(design), beta))/noise_var
}

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
hiper_glm <- function(design, outcome, model = "linear", option = NULL) {
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  int = F
  if (model == 'linear'){
    if(sum(apply(design,2, var)<=1e-10)==0){
      #design = cbind(rep(1, nrow(design)), design)
      int = T
    }
    if(sum(apply(design,2, var)<=1e-10)>1){
      stop(sprintf("Too many intercept rows"))
    }
    if(is.null(option$mle_solver)){
      beta = solve(crossprod(design), crossprod(design, outcome))
    }
    else if(option$mle_solver == 'BFGS'){
     log_lkl = function(beta, noise_var = 1){
       .5*crossprod(outcome - crossprod(t(design), beta))[1,1]/noise_var
     }
     grad = function(beta, noise_var = 1){
       grad_linear(design, outcome, beta = beta, noise_var = noise_var)
     }
     beta = optim(rep(0, ncol(design)), log_lkl, grad)$par
    }
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
  hglm_out <- list()
  hglm_out$model = model
  hglm_out$coefficients = beta
  hglm_out$residuals = as.vector(res)
  hglm_out$fitted.values = as.vector(offsets)
  #hglm_out$XtX_inv = XtX_inv
  hglm_out$df_res = nrow(design) - ncol(design)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
