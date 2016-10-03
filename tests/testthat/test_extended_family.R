library(glmmsr)
context("Extended family")

test_that("extended family evaluates correctly", {
  binomial_logit_family <- binomial(link = "logit")
  binomial_logit_extended_family <- extended_family(family = "binomial",
                                                    link = "logit")
  y <- rep(c(0, 1), 5)
  wt <- rep(1, 10)
  eta <- seq(-1, 1, length.out = 10)
  mu <- binomial_logit_family$linkinv(eta)
  
  log_f <- -sum(binomial_logit_family$dev.resids(y, mu, wt)) / 2
  log_f_extended <- binomial_logit_extended_family$evaluate(eta, y, wt)
  expect_equal(log_f_extended, log_f)
})
