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


