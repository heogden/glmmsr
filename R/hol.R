find_M <- function(pars, modfr, normal_approx) {
    H_hat_inv <- solve(normal_approx$precision)
    Zt <- modfr$reTrms$Zt
    N <- ncol(Zt)
    d <- nrow(Zt)
    M <- matrix(nrow = N, ncol = N)
    LambdatThetaZt <- find_LambdatThetaZt(pars, modfr)

    for(i in 1:N) {
      Zti = matrix(LambdatThetaZt[,i], nrow = d, ncol = d, byrow = TRUE)
      for(j in 1:N) {
        Ztj = matrix(LambdatThetaZt[,j], nrow = d, ncol = d)
        M[i, j] <- sum(Zti * H_hat_inv * Ztj)
      }
    }
   M
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

  kappa_4 <- sum(weights * gamma_4 * diag_M^2)
  kappa_13_2 <- sum(tcrossprod(weights * gamma_3 * diag_M) * M)
  kappa_23_2 <- sum(tcrossprod(weights * gamma_3) * M^3)

  - kappa_4 / 8 + (2 * kappa_23_2 + 3 * kappa_13_2) / 24
}
