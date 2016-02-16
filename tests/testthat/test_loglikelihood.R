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
