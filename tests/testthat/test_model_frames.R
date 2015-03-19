library(glmmsr)

set.seed(1)
y <- rbinom(10, 1, 0.5)
x <- rbinom(10, 1, 0.5)
cluster <- rep(1:5, each = 2)
data = list(y = y, x = x, cluster = cluster)
modfr_glm <- parse_formula(y ~ x, data = data, family = binomial)
modfr_lme4 <- parse_formula(y ~ x + (1 | cluster), data = data,
                            family = binomial)

test_that("parses formulas with/without random effects correctly", {
  expect_equal_to_reference(modfr_glm, "modfr_glm.rds")
  expect_equal_to_reference(modfr_lme4, "modfr_lme4.rds")
})

modfr_sub_fixed  <- parse_subformula(y ~ x, data = data)
modfr_sub_random <- parse_subformula(y ~ x + (1 | cluster), data = data)

test_that("parses subformulas with/without random effects correctly", {
  expect_equal_to_reference(modfr_sub_fixed, "modfr_sub_fixed.rds")
  expect_equal_to_reference(modfr_sub_random, "modfr_sub_random.rds")
})


test_that("model frame subsetting as expected", {
  expect_equal_to_reference(`[fr`(modfr_sub_fixed, c(2, 5, 5, 6)),
                            "modfr_sub_fixed_subset.rds")
  expect_equal_to_reference(`[fr`(modfr_sub_random, c(2, 5, 5, 6)),
                            "modfr_sub_random_subset.rds")
})
