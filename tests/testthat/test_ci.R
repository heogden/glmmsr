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




test_that("warn about confidence interval if estimate at boundary", {
  set.seed(1)

  n_clusters <- 50
  n_in_cluster <- 2
  n <- n_clusters*n_in_cluster
  cluster <- rep(1:n_clusters, each = n_in_cluster)

  covariate <- rbinom(n, 1, 0.5)
  sigma0 <- 1
  beta0 <- c(0, -0.5)
  u0 <- rnorm(n_clusters)
  eta <- beta0[1] + beta0[2] * covariate + sigma0 * u0[cluster]
  p <- exp(eta) / (1 + exp(eta))
  response <- rbinom(p, 1, p)

  two_level_pt1 = list(response = response, covariate = covariate, cluster = cluster)
  
  n <- length(two_level$response)
  mod <- glmm(response ~ covariate + (1 | cluster),
              data = two_level_pt1, family = binomial, method = "AGQ",
              control = list(nAGQ = 15), verbose = 0)
  confint(mod)
  for(j in seq_along(nrow(ci))) {
    expect_lte(ci[j, 1], coef(mod)[j])
    expect_gte(ci[j, 2], coef(mod)[j])
  }
})
