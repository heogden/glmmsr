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
  devfun_lme4 <- updateGlmerDevfun(devfun, reTrms, nAGQ)
  if(k == 0L) {
    return(devfun_lme4)
  } else {
    if(!requireNameSpace("rgraphpass", quietly = TRUE)) {
      modfr <- list(fr = fr, X = X, reTrms = reTrms, family = family)
      n_re <- nrow(reTrms$Zt)
      factorization_terms <- find_factorization_terms(modfr)
      calibration_pars <- rgraphpass_parameters()
      calibration_pars$n_quadrature_points <- nAGQ
      calibration_pars$n_sparse_levels <- k
      calibration_pars$family <- family$family
      calibration_pars$link <- family$link
      tree <- rgraphpass::cluster_tree(factorization_terms)
      belief_init <- rgraphpass::beliefs(factorization_terms)
      devfun <- function(pars) {
        beliefs <- belief_init
        calibration_pars$theta <- pars[1:n_re]
        calibration_pars$beta <- pars[-(1:n_re)]
        normal_approx <- compute_normal_approx(pars, devfun_lme4)
        beliefs$set_normal_approx(normal_approx$mean, normal_approx$precision)
        beliefs$calibrate(calibration_pars, tree, TRUE)
        beliefs$calibrateForward(calibration_pars, tree, FALSE)
        return(-2 * beliefs$log_normalizing_constant())
      }
      return(devfun)
    } else {
      stop("you must install rgraphpass to use k > 0", call. = FALSE)
    }
  }
}
