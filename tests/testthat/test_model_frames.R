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

modfr_sub_fixed_subset <- `[fr`(modfr_sub_fixed, c(2, 5, 5, 6))
modfr_sub_random_subset <- `[fr`(modfr_sub_random, c(2, 5, 5, 6))

test_that("model frame subsetting as expected", {
  expect_equal_to_reference(modfr_sub_fixed_subset,
                            "modfr_sub_fixed_subset.rds")
  expect_equal_to_reference(modfr_sub_random_subset,
                            "modfr_sub_random_subset.rds")
})

test_that("model frame (+, -, *, /) as expected", {
  mfplusmf <- `+fr`(modfr_sub_fixed_subset, modfr_sub_fixed_subset)
  mftimestwo <- `*fr`(modfr_sub_fixed_subset, 2)
  twotimesmf <- `*fr`(2, modfr_sub_fixed_subset)
  mfoverpt5 <- `/fr`(modfr_sub_fixed_subset, 0.5)
  expect_equal(mfplusmf, mftimestwo)
  expect_equal(mftimestwo, twotimesmf)
  expect_equal(mftimestwo, mfoverpt5)

  mrplusmr <- `+fr`(modfr_sub_random_subset, modfr_sub_random_subset)
  mrtimestwo <- `*fr`(modfr_sub_random_subset, 2)
  twotimesmr <- `*fr`(2, modfr_sub_random_subset)
  mroverpt5 <- `/fr`(modfr_sub_random_subset, 0.5)
  expect_equal(mrplusmr, mrtimestwo)
  expect_equal(mrtimestwo, twotimesmr)
  expect_equal(mrtimestwo, mroverpt5)
})

test_that("able to subset model frame by a factor", {
  index <- factor(c("b", "c", "d"), levels = letters[1:5])
  index_numeric <- as.numeric(index)
  modfr_sub_fixed_subset_factor <- `[fr`(modfr_sub_fixed, index)
  modfr_sub_random_subset_factor <- `[fr`(modfr_sub_random, index)
  modfr_sub_fixed_subset_numeric <- `[fr`(modfr_sub_fixed, index_numeric)
  modfr_sub_random_subset_factor <- `[fr`(modfr_sub_random, index_numeric)
  expect_equal(modfr_sub_fixed_subset_factor,  modfr_sub_fixed_subset_numeric)
  expect_equal(modfr_sub_random_subset_factor , modfr_sub_random_subset_factor)

})

test_that("model frames concatenate correctly", {
  fixed_concat <- concatenate_frames(modfr_sub_fixed, modfr_sub_fixed)
  random_concat <- concatenate_frames(modfr_sub_random, modfr_sub_random)
  expect_equal_to_reference(fixed_concat, "fixed_concat.rds")
  expect_equal_to_reference(random_concat, "random_concat.rds")
})
