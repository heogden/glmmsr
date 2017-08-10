library(glmmsr)
context("replicate Shun1995")

# Checks that the second-order Laplace approximation is the
# same as the second-order Laplace approximation computed in
# "Laplace Approximation of High Dimensional Integrals",
# Zhenming Shun and Peter McCullagh,
# Journal of the Royal Statistical Society. Series B (Methodological)
# Volume 57, Issue 4 (1995), 749-760

response <- c(1, 0, 0, 1, 0, 1, 1, 1, 1, 1,
              1, 0, 1, 0, 0, 0, 0, 1, 0, 1,
              1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
              1, 0, 1, 0, 0, 1, 0, 0, 0, 1,
              0, 1, 1, 1, 1, 1, 0, 0, 1, 0,
              1, 0, 1, 1, 1, 1, 1, 1, 0, 1,
              1, 1, 1, 0, 1, 0, 0, 1, 1, 1,
              1, 1, 1, 1, 1, 0, 0, 0, 1, 0,
              0, 0, 1, 1, 1, 1, 1, 1, 0, 0,
              1, 0, 1, 1, 1, 0, 1, 0, 0, 0)
row_id <- factor(rep(paste("r", 1:10, sep = ""), times = 10))
col_id<- factor(rep(paste("c", 1:10, sep = ""), each = 10))

binary_array <- list(response = response, row_id = row_id, col_id = col_id)

modfr <- find_modfr_glmm(response ~ (1 | row_id) + (1 | col_id),
                         data = binary_array, family = binomial)

lfun_Laplace_1 <- find_lfun_glmm(modfr, method = "Laplace")
lfun_Laplace_2 <- find_lfun_glmm(modfr, method = "Laplace", control = list("order" = 2))

test_that("second-order Laplace matches Shun1995", {
  pars <- c(1, 1, 1)
  lfun_Laplace_1_pars <- lfun_Laplace_1(pars)
  lfun_Laplace_2_pars <- lfun_Laplace_2(pars)
  expect_true(abs(-72.6796 - lfun_Laplace_1_pars) < 1e-4)
  expect_true(abs(-72.2612 - lfun_Laplace_2_pars) < 1e-4)

  pars_0pt5 <- c(0.5, 0.5, 1)
  lfun_Laplace_1_pars_0pt5 <- lfun_Laplace_1(pars_0pt5)
  lfun_Laplace_2_pars_0pt5 <- lfun_Laplace_2(pars_0pt5)
  expect_true(abs(-70.0090 - 0.1024 - lfun_Laplace_1_pars_0pt5) < 1e-4)
  expect_true(abs(-70.0090 + 0.0038 - lfun_Laplace_2_pars_0pt5) < 1e-4)

  pars_3 <- c(3, 3, 1)
  lfun_Laplace_1_pars_3 <- lfun_Laplace_1(pars_3)
  lfun_Laplace_2_pars_3 <- lfun_Laplace_2(pars_3)
  expect_true(abs(-85.6111 - 0.8525 - lfun_Laplace_1_pars_3) < 1e-4)
  expect_true(abs(-85.6111 + 0.0547 - lfun_Laplace_2_pars_3) < 1e-4)

})
# (see Table 2, p 757, Shun1995)

test_that("first-order Laplace sufficiently accurate here", {
  fit_laplace_1 <- glmm(response ~ (1 | row_id) + (1 | col_id),
                        data = binary_array, family = binomial,
                        method = "Laplace",
                        control = list(check_Laplace = TRUE),
                        verbose = 0)
  expect_true(fit_laplace_1$laplace_divergence < 0.1)
})

fit_laplace_2 <- glmm(response ~ (1 | row_id) + (1 | col_id),
                      data = binary_array, family = binomial,
                      method = "Laplace",
                      control = list(order = 2),
                      verbose = 0)
