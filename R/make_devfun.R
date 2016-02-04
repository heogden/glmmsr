#' Make the deviance function
#' @inheritParams lme4::mkGlmerDevfun
#' @inheritParams glmm
#' @export
mkGlmmDevfun <- function(modfr, method, control)
{
  devfun_lme4 <- lme4::mkGlmerDevfun(fr = modfr$fr, X = modfr$X,
                                     reTrms = modfr$reTrms, family = modfr$family,
                                     control = lme4_control())
  nAGQ_lme4 <- ifelse(method == "AGQ", control$nAGQ, 1)
  devfun_lme4 <- lme4::updateGlmerDevfun(devfun_lme4, modfr$reTrms, nAGQ = nAGQ_lme4)
  switch(method,
         Laplace = devfun_lme4,
         AGQ = devfun_lme4,
         SR = mkGlmmDevfunSR(modfr, devfun_lme4,
                             nSL = control$nSL,
                             nAGQ = control$nAGQ),
         stop(cat("method", method, "not available"))
         )
}

mkGlmmDevfunSR <- function(modfr, devfun_lme4, nSL, nAGQ) {
  n_fixed <- ncol(modfr$X)
  factorization_terms <- find_factorization_terms(modfr)
  calibration_pars <- calibration_parameters()
  calibration_pars$n_quadrature_points <- nAGQ
  calibration_pars$n_sparse_levels <- nSL
  calibration_pars$family <- modfr$family$family
  calibration_pars$link <- modfr$family$link
  beliefs <- cluster_graph(factorization_terms)

  complexity_limit <- 5e5
  if(beliefs$width^(2 * nSL) > complexity_limit) {
    stop(paste("The sequential reduction approximation with", nSL,
               "sparse levels is too difficult to compute in this case.",
               "Consider reducing nSL, or using a different approximation method."),
         call. = FALSE)
  }

  devfun <- function(pars) {
    theta_size <- length(pars) - n_fixed
    calibration_pars$theta <- pars[1:theta_size]
    calibration_pars$beta <- pars[-(1:theta_size)]
    normal_approx <- compute_normal_approx(pars, devfun_lme4)
    -2 * beliefs$compute_log_normalizing_constant(normal_approx$mean,
                                                  normal_approx$precision,
                                                  calibration_pars)
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
