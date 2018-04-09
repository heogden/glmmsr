library(glmmsr)
context("lme4_control")

test_that("can change options in glmerControl", {
  lme4_control = check.nlev.gtr.1 = "ignore"
  y <- c(rep(1, 10), rep(0, 10))
  i <- rep(1, 20)
  modfr <- find_modfr_glmm(y ~ (1 | i), family = "binomial")
  # should not give an error, by default
  expect_error(
    find_modfr_glmm(y ~ (1 | i), family = "binomial",
                    lme4_control = set_lme4_control(check.nlev.gtr.1 = "stop"))
  )
  # can change lme4_control so that it will give an error
})

