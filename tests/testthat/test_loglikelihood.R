library(glmmsr)
context("Log-likelihood")

test_that("error for wrong length parameters", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster), data = two_level,
                           family = binomial)
  lfun_SR <- find_lfun_glmm(modfr, method = "SR")

  expect_error(lfun_SR(0.1),
               "cannot compute loglikelihood for parameter of length 1 != 3")
  expect_error(lfun_SR(rep(0.1, 4)),
               "cannot compute loglikelihood for parameter of length 4 != 3")
})

test_that("can compute SR log-likelihood for three-level model", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster) + (1 | group),
                           data = three_level, family = binomial)
  lfun_SR <- find_lfun_glmm(modfr, method = "SR")
  lfun_SR(c(0.5, 0.5, 0 , 0))
})

test_that("log-likelihoods match for two-level model", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
  lfun_Laplace <- find_lfun_glmm(modfr, method = "Laplace")
  lfun_AGQ <- find_lfun_glmm(modfr, method = "AGQ")
  lfun_SR <- find_lfun_glmm(modfr, method = "SR")

  set.seed(1)
  lfun_IS <- find_lfun_glmm(modfr, method = "IS", control = list(nIS = 1e3))

  pars <- c(1, 0.5, -0.5)
  lfun_Laplace_pars <- lfun_Laplace(pars)
  lfun_AGQ_pars <- lfun_AGQ(pars)
  lfun_SR_pars <- lfun_SR(pars)
  lfun_IS_pars <- lfun_IS(pars)

  lfun_L2 <- find_lfun_glmm(modfr, method = "Laplace", control = list(order = 2))
  lfun_L2_pars <- lfun_L2(pars)


  expect_true(abs(lfun_AGQ_pars - lfun_SR_pars) < 1e-3)
  expect_true(abs(lfun_AGQ_pars - lfun_IS_pars) < 0.1)
  expect_true(abs(lfun_L2_pars - lfun_Laplace_pars) > abs(lfun_L2_pars - lfun_AGQ_pars))

})

test_that("All methods work with a single random effect",{
  y <- c(rep(1, 10), rep(0, 10))
  i <- rep(1, 20)
  modfr <- find_modfr_glmm(y ~ (1 | i), family = "binomial")
  lfun_L_2 <- find_lfun_glmm(modfr, method = "Laplace", control = list(order = 2))
  lfun_L_2(c(1, 1))
  lfun_AGQ_5 <- find_lfun_glmm(modfr, method = "AGQ", control = list(nAGQ = 5))
  lfun_AGQ_5(c(1, 1))
  lfun_SR_3 <- find_lfun_glmm(modfr, method = "SR", control = list(nSL = 3))
  lfun_SR_3(c(1, 1))
  lfun_IS_100 <- find_lfun_glmm(modfr, method = "IS", control = list(nIS = 100))
  lfun_IS_100(c(1, 1))
})
