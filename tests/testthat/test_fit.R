library(glmmsr)
context("Model fitting")

test_that("fits a two-level model correctly", {
  mod_15_glmer <- lme4::glmer(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, nAGQ = 15)
  estim_15_glmer <- c(mod_15_glmer@theta, mod_15_glmer@beta)
  mod_15 <- glmm(response ~ covariate + (1 | cluster),
                 data = two_level, family = binomial, method = "AGQ",
                 control = list(nAGQ = 15), verbose = 0)
  estim_15 <- mod_15$estim
  expect_true(sum(abs(estim_15_glmer - estim_15)) < 0.001)

  mod_3_SR <- glmm(response ~ covariate + (1 | cluster),
                   data = two_level, family = binomial, method = "SR",
                   control = list(nSL = 3), verbose = 0)

  estim_3_SR <- mod_3_SR$estim

  expect_true(sum(abs(estim_3_SR - estim_15)) < 0.001)

  set.seed(1)
  mod_IS_100 <- glmm(response ~ covariate + (1 | cluster),
                     data = two_level, family = binomial, method = "IS",
                     control = list(nIS = 100), verbose = 0)
  estim_IS_100 <- mod_IS_100$estim
  expect_true(sum(abs(estim_IS_100 - estim_15)) < 0.2)

  mod_Laplace_1 <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, method = "Laplace",
                        control = list(order = 1, check_Laplace = FALSE),
                        verbose = 0)
  estim_Laplace_1 <- mod_Laplace_1$estim
  error_Laplace_1 <- sum(abs(estim_Laplace_1 - estim_15))

  mod_Laplace_2 <- glmm(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, method = "Laplace",
                        control = list(order = 2), verbose = 0)
  estim_Laplace_2 <- mod_Laplace_2$estim
  error_Laplace_2 <- sum(abs(estim_Laplace_2 - estim_15))
  expect_true(error_Laplace_2 < error_Laplace_1)
})

test_that("coef.glmmFit and vcov.glmmFit give correct names", {
  mod <- glmm(response ~ covariate + (1 | cluster),
              data = two_level, family = binomial, method = "Laplace",
              verbose = 0)
  coef_mod <- coef(mod)
  expect_true(sum(abs(coef_mod - mod$estim)) < 1e-6)

  expect_equal(names(coef_mod),
               c("RE(Intercept)", "(Intercept)", "covariate"))

  vcov_mod <- vcov(mod)
  expect_equal(rownames(vcov_mod), names(coef_mod))
  expect_equal(colnames(vcov_mod), names(coef_mod))

})

test_that("Can handle settings with no covariates", {

  mod_3_SR <- glmm(response ~ 0 + (1 | cluster),
                   data = two_level, family = binomial, method = "SR",
                   control = list(nSL = 3), verbose = 0)

  estim_3_SR <- mod_3_SR$estim

  set.seed(1)
  mod_IS_100 <- glmm(response ~ 0 + (1 | cluster),
                     data = two_level, family = binomial, method = "IS",
                     control = list(nIS = 100), verbose = 0)
  estim_IS_100 <- mod_IS_100$estim
  expect_true(sum(abs(estim_IS_100 - estim_3_SR)) < 0.2)
})

test_that("nSL = 0 gives similar result to Laplace", {
  mod_SR_0 <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                   data = three_level, family = binomial, method = "SR",
                   control = list(nSL = 0), verbose = 0)
  mod_Laplace <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                      data = three_level, family = binomial,
                      method = "Laplace",
                      control = list(check_Laplace = FALSE),
                      verbose = 0)

  estim_SR_0 <- mod_SR_0$estim
  estim_Laplace <- mod_Laplace$estim

  expect_true(sum(abs(estim_SR_0 - estim_Laplace)) < 1e-3)

})

test_that("factor response handled correctly", {
  two_level_factor <- two_level
  two_level_factor$response <- factor(c("N", "Y")[two_level$response + 1],
                                      levels = c("N", "Y"))
  mod_num <- glmm(response ~ covariate + (1 | cluster),
                  data = two_level, family = binomial, method = "SR",
                  control = list(nSL = 3), verbose = 0)
  mod_fac <- glmm(response ~ covariate + (1 | cluster),
                  data = two_level_factor, family = binomial, method = "SR",
                  control = list(nSL = 3), verbose = 0)

  expect_true(sum(abs(mod_num$estim - mod_fac$estim)) < 0.001)
})

test_that("warns about unused control parameters", {
  expect_warning(
    glmm(response ~ covariate + (1 | cluster),
         data = two_level, family = binomial, method = "SR",
         control = list(nSR = 3), verbose = 0),
    "unknown names"
  )
  expect_warning(
    glmm(response ~ covariate + (1 | cluster),
         data = two_level, family = binomial, method = "Laplace",
         control = list(check_Laplace = FALSE, nAGQ = 10), verbose = 0),
    "parts of control were ignored"
  )
})


test_that("checks family", {
  expect_error(glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = poisson, method = "SR", verbose = 0),
               "Only binomial family currently implemented")
  expect_error(glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = binomial(link = "cauchit"),
                    method = "SR", verbose = 0),
               "Only logit and probit links")
})

test_that("prev_fit doesn't affect the result", {
  library(BradleyTerry2)
  result <- rep(1, nrow(flatlizards$contests))
  flatlizards_glmmsr <- c(list(result = result,
                               winner = flatlizards$contests$winner,
                               loser = flatlizards$contests$loser),
                          flatlizards$predictors)
  fit_1 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                data = flatlizards_glmmsr, family = binomial(link = "probit"),
                method = "Laplace", control = list(check_Laplace = FALSE),
                verbose = 0)
  fit_2 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                data = flatlizards_glmmsr, family = binomial(link = "probit"),
                method = "Laplace",  control = list(check_Laplace = FALSE),
                verbose = 0, prev_fit = fit_1)
  fit_3 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                data = flatlizards_glmmsr, family = binomial(link = "probit"),
                method = "Laplace",  control = list(check_Laplace = FALSE),
                verbose = 0, prev_fit = fit_2)
  expect_true(sum(abs(fit_1$estim - fit_2$estim)) < 0.01)
  expect_true(sum(abs(fit_1$Sigma - fit_2$Sigma)) < 0.01)
  expect_true(sum(abs(fit_1$Sigma - fit_3$Sigma)) < 0.01)
  fit_SR_2 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                data = flatlizards_glmmsr, family = binomial(link = "probit"),
                method = "SR", control = list(nSL = 2), verbose = 0,
                prev_fit = fit_1)
  fit_SR_2_2 <- glmm(result ~ 0 + Sub(ability[winner] - ability[loser]),
                     ability[liz] ~ 0 + SVL[liz] + (1 | liz),
                     data = flatlizards_glmmsr, family = binomial(link = "probit"),
                     method = "SR", control = list(nSL = 2), verbose = 0,
                     prev_fit = fit_SR_2)
  expect_true(sum(abs(fit_SR_2$estim - fit_SR_2_2$estim)) < 0.01)
  expect_true(sum(abs(fit_SR_2$Sigma - fit_SR_2_2$Sigma)) < 0.01)
})

test_that("family = gaussian handled correctly", {
  expect_error(glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = gaussian,
                    method = "Laplace", verbose = 0),
               "glmmsr can't yet handle family = gaussian")
})

problem_two_level <- list()
problem_two_level$response <- c(1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1,
                                0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0,
                                1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1,
                                0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0,
                                1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0,
                                1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1,
                                1, 1, 1, 1)
problem_two_level$cluster <- factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                                    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
                                    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
                                    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
                                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                                    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
                                    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
                                    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50))
problem_two_level$X <- matrix(c(-1.902157, -0.06429479, -1.331167, -1.819992, 0.1626697,
                                0.5313963, 0.295519, 0.02061298, -0.311266, 1.841481,
                                -0.6561465, 1.520367, 0.05396, -0.7570783, -1.858833,
                                1.079192, 1.355274, -0.4354448, 0.1429884, 0.3840304,
                                -0.1928827, -0.621302, -0.5548096, 0.7180758, -1.553635,
                                -0.3940995, 0.6228011, -1.660205, -0.2852927, -0.882944,
                                -0.9627715, 1.779103, 0.3613976, 1.629467, -0.08602144,
                                1.17511, -0.7527189, -0.8205422, 0.3628062, -0.6164582,
                                0.5369402, 0.2833965, 0.3060785, -0.04815354, 0.3594188,
                                -1.384912, 2.186611, -2.099519, -0.9166381, 0.3411614,
                                0.6604541, -0.806498, 0.3504362, -1.420558, -2.356196,
                                -0.2075198, -0.6999116, 0.05024668, 1.841642, -0.6917169,
                                -0.348083, 0.5199808, -0.717668, -0.505385, -1.811896,
                                -0.4385218, -0.9955552, -0.3586264, 0.3300445, 0.08630304,
                                0.4025446, -0.7631445, 0.4317926, 0.6514993, -0.8339014,
                                -1.024838, 1.830329, 2.511476, -0.04110108, 0.1201537,
                                0.219053, -0.8183172, 1.130273, 1.780593, 2.012296, 0.7790465,
                                -0.2952699, -0.3520203, -1.336534, 0.7775954, 0.1159857,
                                -0.270431, 0.4906784, 0.787246, 0.6728172, 0.9784339,
                                0.4064037, 0.01434835, -0.9002922, 0.51354),
                              ncol = 1)

test_that("get finite estimators", {
  fit_Laplace_2 <- glmmsr::glmm(response ~ X + (1 | cluster), family = binomial,
                                method = "Laplace", control = list(order = 2), verbose = 0,
                                data = problem_two_level)
})
