find_M <- function(pars, modfr, normal_approx) {
  H_hat_inv <- solve(normal_approx$precision)
  Zt <- modfr$reTrms$Zt
  N <- ncol(Zt)
  d <- nrow(Zt)
  M <- matrix(0, nrow = N, ncol = N)
  LambdatThetaZt <- find_LambdatThetaZt(pars, modfr)

  Matrix::t(LambdatThetaZt) %*% H_hat_inv %*% LambdatThetaZt
}

find_epsilon_1 <- function(pars, modfr, devfun_laplace_1) {
  normal_approx <- compute_normal_approx(pars, devfun_laplace_1)

  modfr_family <- extended_family(modfr$family$family, modfr$family$link)

  u_hat <- normal_approx$mean
  XBeta <- find_XBeta(pars, modfr)
  LambdatThetaZt <- find_LambdatThetaZt(pars, modfr)
  eta_hat <- find_eta(u_hat, XBeta, LambdatThetaZt)

  response <- model.response(modfr$fr)
  weights <- model.weights(modfr$fr)
  if(length(weights) == 0)
    weights <- rep(1, length(response))

  gamma_3 <- -modfr_family$evaluate_d3(eta_hat, response, weights)
  gamma_4 <- -modfr_family$evaluate_d4(eta_hat, response, weights)

  M <- find_M(pars, modfr, normal_approx)
  diag_M <- M[row(M) == col(M)]

  kappa_4 <- sum(gamma_4 * diag_M^2)
  kappa_13_2 <- sum(tcrossprod(gamma_3 * diag_M) * M)
  kappa_23_2 <- sum(tcrossprod(gamma_3) * M^3)

  - kappa_4 / 8 + (2 * kappa_23_2 + 3 * kappa_13_2) / 24
}

find_delta_1 <- function(pars, modfr, devfun_laplace_1) {
  numDeriv::grad(find_epsilon_1, pars, modfr = modfr, devfun_laplace_1 = devfun_laplace_1)
}

find_gamma_1 <- function(pars, modfr, devfun_laplace_1) {
  numDeriv::hessian(find_epsilon_1, pars, modfr = modfr, devfun_laplace_1 = devfun_laplace_1)
}

find_laplace_divergence <- function(fit_laplace_1, modfr, devfun_laplace_1) {
  delta_1 <- find_delta_1(fit_laplace_1$estim, modfr, devfun_laplace_1)
  gamma_1 <- find_gamma_1(fit_laplace_1$estim, modfr, devfun_laplace_1)

  Sigma_1 <- fit_laplace_1$Sigma
  J_1 <- solve(Sigma_1)
  Sigma_2 <- solve(J_1 - gamma_1)
  estim_2_1 <- fit_laplace_1$estim + as.numeric(crossprod(Sigma_2, delta_1))
  diff <- as.numeric(crossprod(Sigma_2, delta_1))

  p <- length(fit_laplace_1$estim)
  log_det_ratio <- determinant(Sigma_1, logarithm = TRUE)$modulus - determinant(Sigma_2, logarithm = TRUE)$modulus
  ratio <- J_1 %*% Sigma_2
  trace_ratio <- sum(ratio[row(ratio) == col(ratio)])
  KL <- 0.5 * (log_det_ratio - p + trace_ratio + sum(diff * crossprod(J_1, diff)))
  #W <- sum(delta_1 * crossprod(fit_laplace_1$Sigma, delta_1))
  attr(KL, "logarithm") <- NULL
  KL
}

