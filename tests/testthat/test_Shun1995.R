library(glmmsr)
context("replicate Shun1995")

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


pars <- c(1, 1, 1)
lfun_Laplace_1_pars <- lfun_Laplace_1(pars)
lfun_Laplace_2_pars <- lfun_Laplace_2(pars)

test_that("second-order Laplace matches Shun1995", {
  expect_true(abs(-72.6796 - lfun_Laplace_1_pars) < 1e-4)
  expect_true(abs(-72.2612 - lfun_Laplace_2_pars) < 1e-4)
})
