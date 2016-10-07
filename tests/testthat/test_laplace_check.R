library("glmmsr")
context("Checking first-order Laplace")

test_that("Wald and LR tests for Laplace-2 first step estimator roughly agree", {
  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
  devfun_laplace_1 <- lme4::mkGlmerDevfun(fr = modfr$fr, X = modfr$X,
                                          reTrms = modfr$reTrms, family = modfr$family,
                                          control = lme4_control())
  devfun_laplace_1 <- lme4::updateGlmerDevfun(devfun_laplace_1, modfr$reTrms, nAGQ = 1)
  fit_laplace_1 <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, method = "Laplace",
                        control = list(order = 1), verbose = 0)
  p_Wald <- approximate_estim_2_p_value(fit_laplace_1, modfr, devfun_laplace_1, type = "Wald")
  p_LR <- approximate_estim_2_p_value(fit_laplace_1, modfr, devfun_laplace_1, type = "LR")
  expect_true((p_Wald / p_LR) > 0.75 && (p_Wald / p_LR) < 1.5)
})
