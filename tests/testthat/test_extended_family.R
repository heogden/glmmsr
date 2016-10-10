library(glmmsr)
context("Extended family")

test_that("binomial(logit) extended family evaluates correctly", {
  binomial_logit_family <- binomial(link = "logit")
  binomial_logit_extended_family <- extended_family(family = "binomial",
                                                    link = "logit")
  y <- rep(c(0, 1), 5)
  wt <- rep(2, 10)
  eta <- seq(-1, 1, length.out = 10)
  mu <- binomial_logit_family$linkinv(eta)

  log_f <- -sum(binomial_logit_family$dev.resids(y, mu, wt)) / 2
  log_f_extended <- binomial_logit_extended_family$evaluate(eta, y, wt)
  expect_equal(log_f_extended, log_f)

  log_f_d1_extended <- binomial_logit_extended_family$evaluate_d1(eta, y, wt)
  log_f_d2_extended <- binomial_logit_extended_family$evaluate_d2(eta, y, wt)
  log_f_d3_extended <- binomial_logit_extended_family$evaluate_d3(eta, y, wt)
  log_f_d4_extended <- binomial_logit_extended_family$evaluate_d4(eta, y, wt)

  log_f_fun <- function(eta) {
     mu <- binomial_logit_family$linkinv(eta)
    -sum(binomial_logit_family$dev.resids(y, mu, wt)) / 2
  }

  log_f_d1_num <- numDeriv::grad(log_f_fun, eta)

  expect_equal(log_f_d1_num, log_f_d1_extended)

  log_f_d1_fun <- function(eta) {
    binomial_logit_extended_family$evaluate_d1(eta, y, wt)
  }

  log_f_d2_num <- numDeriv::grad(log_f_d1_fun, eta)

  expect_equal(log_f_d2_num, log_f_d2_extended)

  log_f_d2_fun <- function(eta) {
    binomial_logit_extended_family$evaluate_d2(eta, y, wt)
  }

  log_f_d3_num <- numDeriv::grad(log_f_d2_fun, eta)

  expect_equal(log_f_d3_num, log_f_d3_extended)

  log_f_d3_fun <- function(eta) {
    binomial_logit_extended_family$evaluate_d3(eta, y, wt)
  }

  log_f_d4_num <- numDeriv::grad(log_f_d3_fun, eta)

  expect_equal(log_f_d4_num, log_f_d4_extended)
})


test_that("binomial(probit) extended family evaluates correctly", {
  binomial_probit_family <- binomial(link = "probit")
  binomial_probit_extended_family <- extended_family(family = "binomial",
                                                     link = "probit")
  y <- rep(c(0, 1), 5)
  wt <- rep(2, 10)
  eta <- seq(-1, 1, length.out = 10)
  mu <- binomial_probit_family$linkinv(eta)

  log_f <- -sum(binomial_probit_family$dev.resids(y, mu, wt)) / 2
  log_f_extended <- binomial_probit_extended_family$evaluate(eta, y, wt)
  expect_equal(log_f_extended, log_f)

  log_f_d1_extended <- binomial_probit_extended_family$evaluate_d1(eta, y, wt)
  log_f_d2_extended <- binomial_probit_extended_family$evaluate_d2(eta, y, wt)
  log_f_d3_extended <- binomial_probit_extended_family$evaluate_d3(eta, y, wt)
  log_f_d4_extended <- binomial_probit_extended_family$evaluate_d4(eta, y, wt)

  log_f_fun <- function(eta) {
    mu <- binomial_probit_family$linkinv(eta)
    -sum(binomial_probit_family$dev.resids(y, mu, wt)) / 2
  }

  log_f_d1_num <- numDeriv::grad(log_f_fun, eta)

  expect_equal(log_f_d1_num, log_f_d1_extended)

  log_f_d1_fun <- function(eta) {
    binomial_probit_extended_family$evaluate_d1(eta, y, wt)
  }

  log_f_d2_num <- numDeriv::grad(log_f_d1_fun, eta)

  expect_equal(log_f_d2_num, log_f_d2_extended)

  log_f_d2_fun <- function(eta) {
    binomial_probit_extended_family$evaluate_d2(eta, y, wt)
  }

  log_f_d3_num <- numDeriv::grad(log_f_d2_fun, eta)

  expect_equal(log_f_d3_num, log_f_d3_extended)

  log_f_d3_fun <- function(eta) {
    binomial_probit_extended_family$evaluate_d3(eta, y, wt)
  }

  log_f_d4_num <- numDeriv::grad(log_f_d3_fun, eta)

  expect_equal(log_f_d4_num, log_f_d4_extended)
})

