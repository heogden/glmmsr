#' Make the deviance function
#' @inheritParams lme4::mkGlmerDevfun
#' @inheritParams glmm
#' @export
mkGlmmDevfun <- function(fr, X, reTrms, family, control = glmmControl(), ...)
{
  devfun_lme4 <- mkGlmerDevfun(fr = fr, X = X, reTrms = reTrms, family = family,
                               verbose = control$verbose,
                               control = control, ...)
  nAGQ_lme4 <- ifelse(control$method == "lme4", control$nAGQ, 1)
  devfun_lme4 <- updateGlmerDevfun(devfun_lme4, reTrms, nAGQ = nAGQ_lme4)
  switch(control$method,
         lme4 = devfun_lme4,
         SR = mkGlmmDevfunSR(fr, X, reTrms, family, devfun_lme4,
                             n_sparse_levels = control$n_sparse_levels,
                             nAGQ = control$nAGQ),
         stop(cat("method", method, "not available"))
         )
}

mkGlmmDevfunSR <- function(fr, X, reTrms, family, devfun_lme4,
                           n_sparse_levels, nAGQ) {
  modfr <- list(fr = fr, X = X, reTrms = reTrms, family = family)
  n_fixed <- ncol(X)
  factorization_terms <- find_factorization_terms(modfr)
  calibration_pars <- graphpass::calibration_parameters()
  calibration_pars$n_quadrature_points <- nAGQ
  calibration_pars$n_sparse_levels <- n_sparse_levels
  calibration_pars$family <- family$family
  calibration_pars$link <- family$link
  beliefs_base <- graphpass::cluster_graph(factorization_terms)
  devfun <- function(pars) {
    beliefs <- graphpass::cluster_graph(factorization_terms)
    theta_size <- length(pars) - n_fixed
    calibration_pars$theta <- pars[1:theta_size]
    calibration_pars$beta <- pars[-(1:theta_size)]
    beliefs$set_parameters(calibration_pars)

    normal_approx <- compute_normal_approx(pars, devfun_lme4)
    beliefs$set_normal_approx(normal_approx$mean, normal_approx$precision)
    beliefs$calibrate()
    -2 * beliefs$log_normalizing_constant
  }
  devfun
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
