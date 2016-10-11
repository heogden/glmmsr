library(glmmsr)
context("Weights")

test_that("model fitting uses weights correctly", {
  two_level_double <- list(response = rep(two_level$response, 2),
                           covariate = rep(two_level$covariate, 2),
                           cluster = rep(two_level$cluster, 2))
  mod_double_Laplace <-  glmm(response ~ covariate + (1 | cluster),
                              data = two_level_double, family = binomial,
                              method = "Laplace",
                              control = list(check_Laplace = TRUE,
                                             divergence_threshold = 1e6),
                              verbose = 0)
  mod_w2_Laplace <- glmm(response ~ covariate + (1 | cluster),
                         data = two_level, family = binomial,
                         method = "Laplace",
                         control = list(check_Laplace = TRUE,
                                        divergence_threshold = 1e6),
                         weights = rep(2, length(two_level$response)),
                         verbose = 0)
  expect_equal(mod_double_Laplace$estim, mod_w2_Laplace$estim)
  expect_equal(mod_double_Laplace$laplace_divergence, mod_w2_Laplace$laplace_divergence)

  mod_double_SR <-  glmm(response ~ covariate + (1 | cluster),
                         data = two_level_double, family = binomial,
                         method = "SR", verbose = 0)
  mod_w2_SR <- glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = binomial,
                    method = "SR", weights = rep(2, length(two_level$response)),
                    verbose = 0)
  expect_true(sum(abs(mod_double_SR$estim - mod_w2_SR$estim)) < 0.01)

  expect_error(glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = binomial,
                    method = "SR", weights = rep(2.1, length(two_level$response)),
                    verbose = 0),
               "non-integer weights")
})
