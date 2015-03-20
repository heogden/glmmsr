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
subform <- ability[player] ~ 0 + x[player] + (1 | player)
subform_i <- ability[i] ~ 0 + x[i] + (1 | i)
data <- list(x = x, player1 = player1, player2 = player2)

test_that("parses simple call correctly (no array indexing)", {
  fit <- glmerSR(formula, data = data, family = binomial,
                 subforms = list(subform))
  fit_i <- glmerSR(formula, data = data, family = binomial,
                 subforms = list(subform_i))
  expect_equal(unname(coef(fit)[[1]]), unname(coef(fit_i)[[1]]))
})
