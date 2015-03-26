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

fit <- glmerSR(formula, subform, data = data, family = binomial)
s <- attr(VarCorr(fit)[[1]], "stddev")


test_that("doesn't matter what name used for index", {
  subform_i <- ability[i] ~ 0 + x[i] + (1 | i)
  data_i <- list(x = x, player1 = player1, player2 = player2)
  fit_i <- glmerSR(formula, subform_i, data = data_i, family = binomial)
  expect_equal(unname(fixef(fit)[[1]]), unname(fixef(fit_i)[[1]]))

  s_i <- attr(VarCorr(fit_i)[[1]], "stddev")
  expect_equal(s, s_i)
})


test_that("different forms of indexing give same result", {
  player1_num <- rep(2:10, 10)
  player2_num <- rep(1:9, 10)
  data_num <- list(x = x, player1 = player1_num, player2 = player2_num)

  fit_num <- glmerSR(formula, subform, data = data_num, family = binomial)
  expect_equal(unname(fixef(fit)[[1]]), unname(fixef(fit_num)[[1]]))

  s_num <- attr(VarCorr(fit_num)[[1]], "stddev")
  expect_equal(s, s_num)
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

  fit_no_1 <- glmerSR(formula, subform, data = data_no_1, family = binomial)
  expect_equal(unname(fixef(fit)[[1]]), unname(fixef(fit_no_1)[[1]]))
  s_no_1 <- attr(VarCorr(fit_no_1)[[1]], "stddev")
  expect_equal(s, s_no_1)

  fit_no_1_num <- glmerSR(formula, subform, data = data_no_1_num,
                          family = binomial)

  expect_equal(unname(fixef(fit)[[1]]), unname(fixef(fit_no_1_num)[[1]]))
  s_no_1_num <- attr(VarCorr(fit_no_1_num)[[1]], "stddev")
  expect_equal(s, s_no_1_num)
})

test_that("includes offset", {
  set.seed(1)
  offset <- rnorm(length(y))
  fit_off <- glmerSR(formula, subform, data = data, family = binomial,
                 offset = offset)
  expect_false(identical(coef(fit), coef(fit_off)))
})

test_that("random effects at observation level work OK", {
  data_re_obs <- data
  gr <- rep(1, length(y))
  gr[1:(length(y)/2)] <- 2
  gr <- as.factor(gr)
  data_re_obs$gr <- gr
  formula_re_obs <- y ~ 0 + (1 | gr) + Sub(ability[player1] - ability[player2])
  fit_re_obs <- glmerSR(formula_re_obs, subform, data = data_re_obs,
                        family = binomial)
})
