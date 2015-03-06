library(glmmsr)

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
subform <- ability ~ 0 + x + (1 | player)
data <- list(x=x, player1 = player1, player2 = player2, player = player)

test_that("passes to glFormula if no Sub() terms", {
    form <- y ~ 0 + (1 | player1) + (1 | player2)
    modfr1 <- glFormulaSub(form, data = data, family = binomial)
    modfr2 <- lme4::glFormula(form, data = data, family = binomial)
    expect_equal(modfr1, modfr2)
})

test_that("splits up formula correctly", {
  form0 <- formula(y ~ 0 + x)
  form1 <- formula(y ~ 0 + x + Sub(ability[player1] - ability[player2]))
  form2 <- formula(y ~ 0 + x + Sub(ability[player1] - ability[player2])
                  + Sub(stuff[other1] - stuff[other2]))
  expect_equal(length(split_formula(form0)$replace_exprs), 0)
  expect_equal(length(split_formula(form1)$replace_exprs), 1)
  expect_equal(length(split_formula(form2)$replace_exprs), 2)
})


