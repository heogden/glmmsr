#' Maximize the approximated log-likelihood
#'
#' Find the approximate maximum likelihood estimate, and finds
#' a normal approximation to the likelihood surface
#' @param lfun the approximated loglikelihood function
#' @param p_beta the number of covariates
#' @param p_theta the number of random effects
#' @param init_na a normal approximation to the log-likelihood surface
#' @param verbose integer. If verbose=0, nothing is printed. If verbose=1, a
#'  dot is printed for each likelihood evaluation. If verbose=2, the values of
#'  the parameters and the likelihood are printed for each evaluation.
#' @return A list, containing the parameter estimate and variance matrix
optimizeGlmm <- function(lfun, p_beta, p_theta, init_na = NULL,
                         verbose = 0L){
  p <- p_theta + p_beta
  if(length(init_na) > 0){
    mu <- init_na$mu
    Sigma <- init_na$Sigma
    if(any(eigen(Sigma, only.values = TRUE)$values < 1e-5)) {
      Sigma <- matrix(0, nrow = p, ncol = p)
      Sigma[row(Sigma) == col(Sigma)] <- 1
    }
  }else{
    mu <- c(rep(0.5, p_theta), rep(0, p_beta))
    Sigma <- matrix(0, nrow = p, ncol = p)
    Sigma[row(Sigma) == col(Sigma)] <- 1
  }
  hess <- solve(Sigma / 2)
  eigen_hess <- eigen(hess, symmetric=TRUE)
  hess_eigen_sqrt <- t(eigen_hess$vectors %*%
                         diag(sqrt(eigen_hess$values), nrow = p, ncol = p))
  A_inv <- hess_eigen_sqrt
  A <- solve(A_inv)
  devfun_ext <- function(param){
    result <- tryCatch(-2 * lfun(param),
                       error = function(e) {
                         warning(e)
                         Inf
                       })
    if(verbose==1L){
      cat(".")
    }
    if(verbose>1L){
      cat(sprintf("%7.4f",param), " : ", sprintf("%7.4f",result), "\n")
    }
    result
  }
  devfun_std <- function(param_std){
    param <- as.numeric(A %*% param_std + mu)
    return(devfun_ext(param))
  }
  par0 <- rep(0, p)
  out_std <- optim(par0, devfun_std, hessian=TRUE, method="BFGS",
                   control = list(maxit = 500))
  if(verbose > 0L){
    cat("\n")
  }
  hess_std <- out_std$hessian
  estim_std <- out_std$par
  estim = as.numeric(A %*% estim_std + mu)
  hess <- t(A_inv) %*% hess_std %*% A_inv

  Sigma <- 2 * solve(hess)
  if(min(abs(eigen(Sigma, only.values=TRUE)$values)) < 1e-6) {
    warning("May have problem in convergence: Sigma has a very small eigenvalue")
  }
  result <- list(estim = estim, Sigma = Sigma)
  return(result)
}
