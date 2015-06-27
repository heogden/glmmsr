set.seed(1)

n_groups <- 25
n_clusters_in_group <- 2
n_in_cluster <- 2
n <- n_groups * n_clusters_in_group * n_in_cluster

n_clusters <- n_groups * n_in_cluster
cluster <- rep(1:n_clusters, each = n_in_cluster)

n_in_group <- n_in_cluster * n_clusters_in_group
group <- rep(1:n_groups, each = n_in_group)

# simulate some data
covariate <- rbinom(n, 1, 0.5)
sigma0 <- c(0.5, 1)
beta0 <- -0.5
u0 <- rnorm(n_clusters)
v0 <- rnorm(n_groups)
eta <- beta0 * covariate + sigma0[1] * u0[cluster] + sigma0[2] * v0[group]
p <- exp(eta) / (1 + exp(eta))
response <- rbinom(p, 1, p)

three_level = list(response = response, covariate = covariate, cluster = cluster,
                   group = group)

devfun_approx <- lme4::glmer(response ~ covariate + (1 | cluster) + (1 | group),
                         data = three_level, family = binomial,
                         devFunOnly = TRUE)
devfun_10_3 <- glmerSR(response ~ covariate + (1 | cluster) + (1 | group),
                       data = three_level, family = binomial,
                       nAGQ = 10, k = 3,
                       devFunOnly = TRUE)

devfun_approx(c(0.5, 0.5, 1, -0.5))
devfun_10_3(c(0.5, 0.5, 1, -0.5))
