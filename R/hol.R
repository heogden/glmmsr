find_M <- function(pars, modfr, normal_approx) {
  H_hat <- normal_approx$precision
  LambdatThetaZt <- find_LambdatThetaZt(pars, modfr)
  Matrix::crossprod(LambdatThetaZt, Matrix::solve(H_hat, LambdatThetaZt))
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
  diag_M <- Matrix::diag(M)

  kappa_4 <- sum(gamma_4 * diag_M^2)
  gamma_3_diag_M <- gamma_3 * diag_M
  kappa_13_2 <- as.numeric(Matrix::crossprod(gamma_3_diag_M, Matrix::crossprod(M, gamma_3_diag_M)))
  kappa_23_2 <- as.numeric(Matrix::crossprod(gamma_3, Matrix::crossprod(M^3, gamma_3)))

  - kappa_4 / 8 + (2 * kappa_23_2 + 3 * kappa_13_2) / 24
}

find_delta_1 <- function(pars, modfr, devfun_laplace_1) {
  numDeriv::grad(find_epsilon_1, pars, modfr = modfr, devfun_laplace_1 = devfun_laplace_1)
}

find_gamma_1 <- function(pars, modfr, devfun_laplace_1) {
  numDeriv::hessian(find_epsilon_1, pars, modfr = modfr, devfun_laplace_1 = devfun_laplace_1)
}

find_laplace_divergence <- function(fit_laplace_1, modfr, devfun_laplace_1, use_second_derivatives = FALSE) {
  delta_1 <- find_delta_1(fit_laplace_1$estim, modfr, devfun_laplace_1)
  Sigma_1 <- fit_laplace_1$Sigma
  if(use_second_derivatives) {
    gamma_1 <- find_gamma_1(fit_laplace_1$estim, modfr, devfun_laplace_1)
    J_1 <- solve(Sigma_1)
    Sigma_2 <- solve(J_1 - gamma_1)
    estim_2_1 <- fit_laplace_1$estim + as.numeric(crossprod(Sigma_2, delta_1))
    diff <- as.numeric(crossprod(Sigma_2, delta_1))

    p <- length(fit_laplace_1$estim)
    log_det_ratio <- determinant(Sigma_1, logarithm = TRUE)$modulus - determinant(Sigma_2, logarithm = TRUE)$modulus
    ratio <- J_1 %*% Sigma_2
    trace_ratio <- sum(ratio[row(ratio) == col(ratio)])
    KL <- 0.5 * (log_det_ratio - p + trace_ratio + sum(diff * crossprod(J_1, diff)))
    attr(KL, "logarithm") <- NULL
  } else {
    KL <- 0.5 * sum(delta_1 * crossprod(Sigma_1, delta_1))
  }
  KL
}

