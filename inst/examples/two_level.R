set.seed(1)

n_clusters <- 5
n_in_cluster <- 6
n <- n_clusters*n_in_cluster

y <- rbinom(n, 1, 0.5)
x <- rbinom(n, 1, 0.5)

cluster <- rep(1:n_clusters, each = n_in_cluster)
data = list(y = y, x = x, cluster = cluster)

devfun_approx <- glmerSR(y ~ x + (1 | cluster), data = data, family = binomial,
                         nAGQ = 0, k = 0)
devfun_10_lme4 <- glmerSR(y ~ x + (1 | cluster), data = data, family = binomial,
                          nAGQ = 10, k = 0)
devfun_10_sr <- glmerSR(y ~ x + (1 | cluster), data = data, family = binomial,
                        nAGQ = 10, k = 0)
devfun_approx(c(1, -0.5))
devfun_10_lme4(c(1, -0.5))
devfun_10_sr(c(1, -0.5))
