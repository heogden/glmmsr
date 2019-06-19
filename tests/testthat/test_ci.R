library(glmmsr)
context("Confidence intervals")

test_that("gives reasonable confidence interval for two-level model", {
  mod <- glmm(response ~ covariate + (1 | cluster),
              data = two_level, family = binomial, method = "Laplace", verbose = 0)
  ci <- confint(mod)
  for(j in seq_along(nrow(ci))) {
    expect_lte(ci[j, 1], coef(mod)[j])
    expect_gte(ci[j, 2], coef(mod)[j])
  }
})
