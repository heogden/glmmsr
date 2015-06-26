set.seed(1)

n_clusters <- 25
n_subclusters_in_cluster <- 2
n_in_subcluster <- 2
n <- n_clusters * n_subclusters_in_cluster * n_in_subcluster

n_subclusters <- n_clusters * n_in_subcluster
subcluster <- rep(1:n_subclusters, each = n_in_subcluster)

n_in_cluster <- n_in_subcluster * n_subcluster_in_cluster
cluster <- rep(1:n_clusters, each = n_in_cluster)

# simulate some data
x <- rbinom(n, 1, 0.5)
sigma0 <- c(0.5, 1)
beta0 <- -0.5
u0 <- rnorm(n_subclusters)
v0 <- rnorm(n_clusters)
eta <- beta0 * x + sigma0[1] * u0[subcluster] + sigma0[2] * v0[cluster]
p <- exp(eta) / (1 + exp(eta))
y <- rbinom(p)

data = list(y = y, x = x, cluster = cluster, subcluster = subcluster)

devfun_approx <- glmerSR(y ~ x + (1 | cluster) + (1 | subcluster),
                         data = data, family = binomial,
                         devFunOnly = TRUE)
devfun_10_4 <- glmerSR(y ~ x + (1 | cluster) + (1 | subcluster),
                       data = data, family = binomial,
                       nAGQ = 10, k = 4,
                       devFunOnly = TRUE)

devfun_approx(c(0.5, 1, -0.5))
devfun_10_4(c(0.5, 1, -0.5))
