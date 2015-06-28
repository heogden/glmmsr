#' Make the deviance function
#' @inheritParams lme4::mkGlmerDevfun
#' @inheritParams glmerSR
#' @export
mkGlmerDevfunSR <- function(fr, X, reTrms, family, nAGQ = 1L, k = 0L,
                            verbose = 0L, control = glmmsrControl(), ...)
{
  devfun_lme4 <- mkGlmerDevfun(fr = fr, X = X, reTrms = reTrms, family = family,
                          nAGQ = nAGQ, verbose = verbose,
                          control = control, ...)
  devfun_lme4 <- updateGlmerDevfun(devfun_lme4, reTrms, nAGQ = 1)
  if(k == 0L) {
    return(devfun_lme4)
  } else {
    if(requireNamespace("rgraphpass", quietly = TRUE)) {
      modfr <- list(fr = fr, X = X, reTrms = reTrms, family = family)
      n_fixed <- ncol(X)
      factorization_terms <- find_factorization_terms(modfr)
      calibration_pars <- rgraphpass::calibration_parameters()
      calibration_pars$n_quadrature_points <- nAGQ
      calibration_pars$n_sparse_levels <- k
      calibration_pars$family <- family$family
      calibration_pars$link <- family$link
      tree <- rgraphpass::cluster_tree(factorization_terms)
      #beliefs_init <- rgraphpass::beliefs(factorization_terms, tree)
      devfun <- function(pars) {
        beliefs <- rgraphpass::beliefs(factorization_terms, tree)
        theta_size <- length(pars) - n_fixed
        calibration_pars$theta <- pars[1:theta_size]
        calibration_pars$beta <- pars[-(1:theta_size)]
        #normal_approx <- compute_normal_approx(pars, devfun_lme4)
        #beliefs$set_normal_approx(normal_approx$mean, normal_approx$precision)
        #beliefs$calibrate(calibration_pars, tree, TRUE)
        beliefs$calibrate_forward(calibration_pars, tree, FALSE)
        return(-2 * beliefs$log_normalizing_constant(calibration_pars, tree))
      }
      return(devfun)
    } else {
      stop("you must install rgraphpass to use k > 0", call. = FALSE)
    }
  }
}

compute_normal_approx <- function(pars, devfun_lme4)
{
  devfun_lme4(pars)
  PR <- get("pp", environment(devfun_lme4))
  L_tot <- Matrix::expand(PR$L())
  L <- L_tot$L
  P <- L_tot$P
  precision <- Matrix::t(P)%*%Matrix::tcrossprod(L)%*%P
  mean <- PR$delu
  return(list(mean = mean, precision = precision))
}
