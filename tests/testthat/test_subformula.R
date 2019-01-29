library(glmmsr)
context("Subformula interface")


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
            method = "Laplace", control = list(check_Laplace = FALSE),
            verbose = 0)

test_that("passes to glFormula if no Sub() terms", {
    form <- y ~ 0 + (1 | player1) + (1 | player2)
    data <- list(y = y, player1 = player1, player2 = player2)
    modfr1 <- find_modfr_glmm(form, data = data, family = binomial)
    modfr2 <- lme4::glFormula(form, data = data, family = binomial)
    expect_equal(modfr1$reTrms, modfr2$reTrms)
})

test_that("splits up formula correctly", {
  expect_equal(length(split_formula(formula)$subexprs), 1)
  form0 <- formula(y ~ 0 + x)
  form1 <- formula(y ~ 0 + x + Sub(ability[player1] - ability[player2]))
  form2 <- formula(y ~ 0 + x + Sub(ability[player1] - ability[player2])
                  + Sub(stuff[other1] - stuff[other2]))
  expect_equal(length(split_formula(form0)$subexprs), 0)
  expect_equal(length(split_formula(form1)$subexprs), 1)
  expect_equal(length(split_formula(form2)$subexprs), 2)
})

test_that("finds subvar from subform", {
  subform0 <- formula(ability[player] ~ x[player] + (1 | player))
  expect_equal(find_subvar(subform0), "ability")
  subform1 <- formula(ability[player] + ability[player2] ~ (1 | player))
  expect_error(find_subvar(subform1), "single variable on LHS")
})


test_that("matches subforms and subexprs correctly", {
  subforms <- list(formula(ability[player] ~ 0 + (1 | player)),
                   formula(stuff[i, j] ~ x[i] + (1 | j)))
  subexprs <- list(quote(ability[player1] - ability[player2]),
                   quote(stuff[team1, team2] - 3*stuff[team2, team1]))
  data = list(player1 = c(1, 2), player2 = c(2, 1),
              team1 = c(1, 2, 3), team2 = c(4, 2, 1), x = c(1, 2, 3, 4))
  out <- match_subform_subexpr(subforms, subexprs, data)
  expect_equal_to_reference(out, "subs_ability_stuff.rds")
  subforms1 <- list(formula(ability[player] ~ 0 + (1 | player)),
                  formula(stuff[i, j] ~ x[i] + (1 | j)),
                  other[k] ~ (1 | k))
  expect_warning(match_subform_subexpr(subforms1, subexprs, data),
                "No subexpressions involving")
}
)


test_that("doesn't matter what name used for index", {
  subform_i <- ability[i] ~ 0 + x[i] + (1 | i)
  data_i <- list(x = x, player1 = player1, player2 = player2)
  fit_i <- glmm(formula, subform_i, data = data_i, family = binomial,
                method = "Laplace", control = list(check_Laplace = FALSE),
                verbose = 0)
  expect_equal(fit$estim, fit_i$estim)
  expect_equal(fit$Sigma, fit_i$Sigma)
})


test_that("different forms of indexing give same result", {
  player1_num <- rep(2:10, 10)
  player2_num <- rep(1:9, 10)
  data_num <- list(x = x, player1 = player1_num, player2 = player2_num)

  fit_num <- glmm(formula, subform, data = data_num, family = binomial,
                  method = "Laplace", control = list(check_Laplace = FALSE),
                  verbose = 0)
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
                   family = binomial, method = "Laplace",
                   control = list(check_Laplace = FALSE), verbose = 0)
  expect_equal(fit$estim, fit_no_1$estim)
  expect_equal(fit$Sigma, fit_no_1$Sigma)

  fit_no_1_num <- glmm(formula, subform, data = data_no_1_num,
                       family = binomial, method = "Laplace",
                       control = list(check_Laplace = FALSE), verbose = 0)
  expect_equal(fit$estim, fit_no_1_num$estim)
  expect_equal(fit$Sigma, fit_no_1_num$Sigma)
})

test_that("includes offset", {
  set.seed(1)
  offset <- rnorm(length(y))
  fit_off <- glmm(formula, subform, data = data, family = binomial,
                  offset = offset, method = "Laplace",
                  control = list(check_Laplace = FALSE), verbose = 0)
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
                     family = binomial, method = "Laplace",
                     control = list(check_Laplace = FALSE), verbose = 0)
})

test_that("split_formula correctly identifies subformula", {
  formula1 <- Mate ~ 0 + TypeM:TypeF + Sub(propen[female_id] + propen[male_id])
  formula2 <- Mate ~ 0 + Cross + Sub(propen[female_id] + propen[male_id])

  fsplit1 <- split_formula(formula1)
  fsplit2 <- split_formula(formula2)

  expect_equal(fsplit1$subexprs, fsplit2$subexprs)
})
