#' Maximize the approximated log-likelihood
#'
#' Find the approximate maximum likelihood estimate, and finds
#' a normal approximation to the likelihood surface
#' @param lfun the approximated loglikelihood function
#' @param p_beta the number of covariates
#' @param p_theta the number of random effects
#' @inheritParams glmm
#' @return A list, containing the parameter estimate and variance matrix
optimizeGlmm <- function(lfun, p_beta, p_theta, prev_fit = NULL,
                         verbose = 1L){
  p <- p_theta + p_beta
  if(length(prev_fit) > 0){
    mu <- prev_fit$estim
    Sigma <- prev_fit$Sigma
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
    result <- tryCatch(-2 * lfun(param), error = function(e) { Inf })
    if(verbose == 1L) {
      count <<- count + 1
       if(count > print_gap) {
         cat(".")
         count <<- 0
       }
    }
    if(verbose == 2L & started) {
      cat(sprintf("%7.4f", param), " : ", sprintf("%7.4f", -result / 2), "\n")
    }
    result
  }
  devfun_std <- function(param_std){
    param <- as.numeric(A %*% param_std + mu)
    return(devfun_ext(param))
  }

  time_threshold <- 0.1
  time_interval <- 1
  print_gap <- 100
  count <- 0
  started <- FALSE

  par0 <- rep(0, p)
  t0 <- system.time(d0 <- devfun_std(par0))[[1]]
  print_gap <- floor(time_interval / t0)

  if(verbose > 0L && t0 > time_threshold)
    cat("Approximating the likelihood at each point takes", t0, "seconds. \n")
  if(!is.finite(d0))
    stop("Could not approximate the likelihood at the starting parameters for optimization",
         call. = FALSE)

  started <- TRUE
  if(verbose > 0L)
    cat("Fitting the model.")
  if(verbose == 2L) {
    cat("\n")
  }
  out_std <- optim(par0, devfun_std, hessian=TRUE, method="BFGS",
                   control = list(maxit = 500))
  if(verbose > 0L){
    cat(" done.\n")
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
