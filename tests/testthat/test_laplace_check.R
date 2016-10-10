library("glmmsr")
context("Checking first-order Laplace")

test_that("Wald and LR tests for Laplace-2 first step estimator roughly agree", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
  devfun_laplace_1 <- find_devfun_laplace_1(modfr)
  fit_laplace_1 <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, method = "Laplace",
                        control = list(order = 1, check_Laplace = FALSE), verbose = 0)
  p_Wald <- approximate_estim_2_p_value(fit_laplace_1, modfr, devfun_laplace_1, type = "Wald")
  p_LR <- approximate_estim_2_p_value(fit_laplace_1, modfr, devfun_laplace_1, type = "LR")
  expect_true((p_Wald / p_LR) > 0.5 && (p_Wald / p_LR) < 2)
})

# duplicate data from two-level model
two_level_d<- list()
two_level_d$response <- rep(two_level$response, 2)
two_level_d$covariate <- rep(two_level$covariate, 2)
two_level_d$cluster   <- c(two_level$cluster, max(two_level$cluster) + two_level$cluster)

test_that("Laplace has more problem for duplicated data", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
  devfun_laplace_1 <- find_devfun_laplace_1(modfr)

  modfr_d <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level_d, family = binomial)
  devfun_laplace_1_d <- find_devfun_laplace_1(modfr_d)

  expect_warning(
  fit_laplace <- glmm(response ~ covariate + (1 | cluster),
                      data = two_level, family = binomial, method = "Laplace",
                      control = list(order = 1), verbose = 0),
  "Inference using the first-order Laplace approximation may be unreliable in this case")

  expect_warning(
  fit_laplace_d <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level_d, family = binomial, method = "Laplace",
                        control = list(order = 1), verbose = 0),
  "Inference using the first-order Laplace approximation may be unreliable in this case")

  estim_2_p_value <- approximate_estim_2_p_value(fit_laplace, modfr, devfun_laplace_1)
  estim_2_p_value_d <- approximate_estim_2_p_value(fit_laplace_d, modfr_d, devfun_laplace_1_d)

  expect_true(estim_2_p_value < estim_2_p_value_d)

})

