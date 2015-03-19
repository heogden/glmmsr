library(glmmsr)

test_that("parses formulas with/without random effects correctly", {
  set.seed(1)
  y <- rbinom(10, 1, 0.5)
  x <- rbinom(10, 1, 0.5)
  cluster <- rep(1:5, each = 2)
  data = list(y = y, x = x, cluster = cluster)
  modfr_glm <- parse_formula(y ~ x, data = data, family = binomial)
  modfr_lme4 <- parse_formula(y ~ x + (1 | cluster), data = data,
                              family = binomial)
  expect_equal_to_reference(modfr_glm, "modfr_glm.rds")
  expect_equal_to_reference(modfr_lme4, "modfr_lme4.rds")
})
