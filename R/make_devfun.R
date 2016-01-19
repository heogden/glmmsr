#' Make the deviance function
#' @inheritParams lme4::mkGlmerDevfun
#' @inheritParams glmm
#' @export
mkGlmmDevfun <- function(fr, X, reTrms, family, control = glmmControl(), ...)
{
  devfun_lme4 <- mkGlmerDevfun(fr = fr, X = X, reTrms = reTrms, family = family,
                               verbose = control$verbose,
                               control = control, ...)
  devfun_lme4 <- updateGlmerDevfun(devfun_lme4, reTrms, nAGQ = 1)
  method <- control$method
  switch(method,
         lme4 = devfun_lme4,
         SR = mkGlmmDevfunSR(fr, X, reTrms, family, devfun_lme4, control, ...),
         stop(cat("method", method, "not available"))
         )
}

mkGlmmDevfunSR <- function(fr, X, reTrms, family, devfun_lme4, control, ...) {
  modfr <- list(fr = fr, X = X, reTrms = reTrms, family = family)
  n_fixed <- ncol(X)
  factorization_terms <- find_factorization_terms(modfr)
  calibration_pars <- rgraphpass::calibration_parameters()
  calibration_pars$n_quadrature_points <- nAGQ
  calibration_pars$n_sparse_levels <- k
  calibration_pars$family <- family$family
  calibration_pars$link <- family$link
  tree <- rgraphpass::cluster_tree(factorization_terms)
  devfun <- function(pars) {
    beliefs <- rgraphpass::tree_beliefs_continuous(tree, factorization_terms)
    theta_size <- length(pars) - n_fixed
    calibration_pars$theta <- pars[1:theta_size]
    calibration_pars$beta <- pars[-(1:theta_size)]
    normal_approx <- compute_normal_approx(pars, devfun_lme4)
    normal_beliefs <- rgraphpass::tree_beliefs_normal(tree, normal_approx$mean, normal_approx$precision)
    normal_beliefs$calibrate(tree)
    beliefs$calibrate_forward(tree, normal_beliefs, calibration_pars)
    -2 * beliefs$log_normalizing_constant(tree, normal_beliefs, calibration_pars)
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
