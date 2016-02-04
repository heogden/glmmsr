library(glmmsr)
context("Model fitting")

set.seed(1)
player <- 1:10
player1 <- factor(rep(2:10, 10), levels = player)
player2 <- factor(rep(1:9, 10), levels = player)

x <- rnorm(length(player))
u0 <- rnorm(length(player))

beta0 <- 1
ability0 <- beta0*x + u0
p0 <- pnorm(ability0[player1] - ability0[player2])
y <- rbinom(length(p0), 1, p0)

formula <- y ~ 0 + Sub(ability[player1] - ability[player2])
subform <- ability[player] ~ 0 + x[player] + (1 | player)
data <- list(y = y, x = x, player1 = player1, player2 = player2)

fit <- glmm(formula, subform, data = data, family = binomial,
            method = "Laplace")


test_that("doesn't matter what name used for index", {
  subform_i <- ability[i] ~ 0 + x[i] + (1 | i)
  data_i <- list(x = x, player1 = player1, player2 = player2)
  fit_i <- glmm(formula, subform_i, data = data_i, family = binomial, method = "Laplace")
  expect_equal(fit$estim, fit_i$estim)
  expect_equal(fit$Sigma, fit_i$Sigma)
})


test_that("different forms of indexing give same result", {
  player1_num <- rep(2:10, 10)
  player2_num <- rep(1:9, 10)
  data_num <- list(x = x, player1 = player1_num, player2 = player2_num)

  fit_num <- glmm(formula, subform, data = data_num, family = binomial, method = "Laplace")
  expect_equal(fit$estim, fit_num$estim)
  expect_equal(fit$Sigma, fit_num$Sigma)
})

test_that("OK if don't use all rows of X", {
  player1_no_1 <- factor(rep(3:11, 10), levels = 1:12)
  player2_no_1 <- factor(rep(2:10, 10), levels = 1:12)
  player1_no_1_num <- rep(3:11, 10)
  player2_no_1_num <- rep(2:10, 10)
  # has entries for 1 and 12
  data_no_1 <- list(x = c(x[1],x, x[1]),
                    player1 = player1_no_1, player2 = player2_no_1)
  data_no_1_num <- list(x = c(x[1],x, x[1]),
                    player1 = player1_no_1_num, player2 = player2_no_1_num)

  fit_no_1 <- glmm(formula, subform, data = data_no_1,
                   family = binomial, method = "Laplace")
  expect_equal(fit$estim, fit_no_1$estim)
  expect_equal(fit$Sigma, fit_no_1$Sigma)

  fit_no_1_num <- glmm(formula, subform, data = data_no_1_num,
                       family = binomial, method = "Laplace")
  expect_equal(fit$estim, fit_no_1_num$estim)
  expect_equal(fit$Sigma, fit_no_1_num$Sigma)
})

test_that("includes offset", {
  set.seed(1)
  offset <- rnorm(length(y))
  fit_off <- glmm(formula, subform, data = data, family = binomial,
                  offset = offset, method = "Laplace")
  expect_false(identical(fit$estim, fit_off$estim))
})

test_that("random effects at observation level work OK", {
  data_re_obs <- data
  gr <- rep(1, length(y))
  gr[1:(length(y)/2)] <- 2
  gr <- as.factor(gr)
  data_re_obs$gr <- gr
  formula_re_obs <- y ~ 0 + (1 | gr) + Sub(ability[player1] - ability[player2])
  fit_re_obs <- glmm(formula_re_obs, subform, data = data_re_obs,
                     family = binomial, method = "Laplace")
})

test_that("fits a two-level model correctly", {
  mod_8_glmer <- lme4::glmer(response ~ covariate + (1 | cluster),
                        data = two_level, family = binomial, nAGQ = 8)
  estim_8_glmer <- c(mod_8_glmer@theta, mod_8_glmer@beta)
  mod_8 <- glmm(response ~ covariate + (1 | cluster),
                 data = two_level, family = binomial, method = "AGQ",
                 control = list(AGQ = 8))
  estim_8 <- mod_8$estim
  expect_true(sum(abs(estim_8_glmer - estim_8)) < 0.001)

  mod_8_SR <- glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = binomial, method = "SR",
                    control = list(nSL = 3))

  estim_8_SR <- mod_8_SR$estim

  expect_true(sum(abs(estim_8_SR - estim_8)) < 0.001)

})

test_that("factor response handled correctly", {
  two_level_factor <- two_level
  two_level_factor$response <- factor(c("N", "Y")[two_level$response + 1],
                                      levels = c("N", "Y"))
  mod_num <- glmm(response ~ covariate + (1 | cluster),
                  data = two_level, family = binomial, method = "SR",
                  control = list(nSL = 3))
  mod_fac <- glmm(response ~ covariate + (1 | cluster),
                  data = two_level_factor, family = binomial, method = "SR",
                  control = list(nSL = 3))

  expect_true(sum(abs(mod_num$estim - mod_fac$estim)) < 0.001)
})
