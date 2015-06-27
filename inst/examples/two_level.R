set.seed(1)

n_clusters <- 5
n_in_cluster <- 2
n <- n_clusters*n_in_cluster
cluster <- rep(1:n_clusters, each = n_in_cluster)

# simulate some data
x <- rbinom(n, 1, 0.5)
sigma0 <- 1
beta0 <- -0.5
u0 <- rnorm(n_clusters)
eta <- beta0 * x + sigma0 * u0[cluster]
p <- exp(eta) / (1 + exp(eta))
y <- rbinom(p, 1, p)

two_level = list(y = y, x = x, cluster = cluster)

devfun_approx <- lme4::glmer(y ~ x + (1 | cluster), data = two_level, family = binomial,
                         nAGQ = 1, devFunOnly = TRUE)
devfun_10_lme4 <- lme4::glmer(y ~ x + (1 | cluster), data = two_level, family = binomial,
                          nAGQ = 10, devFunOnly = TRUE)
devfun_10_sr <- glmerSR(y ~ x + (1 | cluster), data = two_level, family = binomial,
                        nAGQ = 10, k = 1, devFunOnly = TRUE)

devfun_approx(c(1, 0, 1))
devfun_10_lme4(c(1, 0, 1))
devfun_10_sr(c(1, 0, 1))
