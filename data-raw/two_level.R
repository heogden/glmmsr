# the simulations used to construct the two_level dataset

set.seed(1)

n_clusters <- 50
n_in_cluster <- 2
n <- n_clusters*n_in_cluster
cluster <- rep(1:n_clusters, each = n_in_cluster)

# simulate some data
covariate <- rbinom(n, 1, 0.5)
sigma0 <- 1
beta0 <- c(0, -0.5)
u0 <- rnorm(n_clusters)
eta <- beta0[1] + beta0[2] * covariate + sigma0 * u0[cluster]
p <- exp(eta) / (1 + exp(eta))
response <- rbinom(p, 1, p)

two_level = list(response = response, covariate = covariate, cluster = cluster)
