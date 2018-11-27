library("glmmsr")
context("Laplace approximations")

test_that("Laplace order handled correctly", {

  mod_Laplace <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                      data = three_level, family = binomial,
                      method = "Laplace",  control = list(check_Laplace = FALSE),
                      verbose = 0)

  mod_Laplace_1 <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                        data = three_level, family = binomial,
                        method = "Laplace",
                        control = list(order = 1, check_Laplace = FALSE),
                        verbose = 0)
  expect_true(sum(abs(mod_Laplace$estim - mod_Laplace_1$estim)) < 0.01)
  expect_error(glmm(response ~ covariate + (1 | cluster) + (1 | group),
                    data = three_level, family = binomial,
                    method = "Laplace",
                    control = list(order = 3),
                    verbose = 0),
               "order-3 Laplace approximation not yet implemented")

  expect_error(glmm(response ~ covariate + (1 | cluster) + (1 | group),
                    data = three_level, family = binomial("probit"),
                    method = "Laplace",
                    control = list(order = 2),
                    verbose = 0),
               "order-2 Laplace approximation not yet implemented for non-canonical link functions")
})

test_that("First step of approx Fisher scoring with Laplace-2 moves in right direction", {
  mod_1 <- glmm(response ~ covariate + (1 | cluster),
                data = two_level, family = binomial, method = "Laplace",
                control = list(order = 1, check_Laplace = FALSE), verbose = 0)
  estim_1 <- mod_1$estim

  mod_2 <- glmm(response ~ covariate + (1 | cluster),
                data = two_level, family = binomial, method = "Laplace",
                control = list(order = 2), verbose = 0)
  estim_2 <- mod_2$estim


  modfr <- find_modfr_glmm(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
  # approximate error-in-score for first-order Laplace
  devfun_laplace_1 <- lme4::mkGlmerDevfun(fr = modfr$fr, X = modfr$X,
                                          reTrms = modfr$reTrms, family = modfr$family,
                                          control = set_lme4_control())
  devfun_laplace_1 <- lme4::updateGlmerDevfun(devfun_laplace_1, modfr$reTrms, nAGQ = 1)
  delta_1 <- find_delta_1(estim_1, modfr, devfun_laplace_1)

  # approx first step of Fisher scoring
  move_1 <- as.numeric(crossprod(mod_1$Sigma, delta_1))

  #actual move to estim_2
  move_real <- estim_2 - estim_1

  expect_true(all(sign(move_1) == sign(move_real)))
})


test_that("Laplace error larger for duplicated data", {
  # duplicate data from two-level model
  two_level_d<- list()
  two_level_d$response <- rep(two_level$response, 2)
  two_level_d$covariate <- rep(two_level$covariate, 2)
  two_level_d$cluster   <- c(two_level$cluster, max(two_level$cluster) + two_level$cluster)

  expect_warning(
  fit_laplace <- glmm(response ~ covariate + (1 | cluster),
                      data = two_level, family = binomial, method = "Laplace",
                      control = list(order = 1, check_Laplace = TRUE), verbose = 0),
  "Inference using the first-order Laplace approximation may be unreliable in this case")

  expect_warning(
  fit_laplace_d <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level_d, family = binomial, method = "Laplace",
                        control = list(order = 1, check_Laplace = TRUE), verbose = 0),
  "Inference using the first-order Laplace approximation may be unreliable in this case")

  expect_true(fit_laplace$laplace_divergence < fit_laplace_d$laplace_divergence)
})


test_that("Laplace divergence is always positive", {
  mod_Laplace <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                      data = three_level, family = binomial, method = "Laplace",
                      control = list(divergence_threshold = 1e6, check_Laplace = TRUE),
                      verbose = 0)
  expect_true(mod_Laplace$laplace_divergence > 0)
}
)
