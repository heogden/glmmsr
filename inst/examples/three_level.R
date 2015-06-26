set.seed(1)

n_clusters <- 5
n_in_cluster <- 6
n <- n_clusters * n_in_cluster

y <- rbinom(n, 1, 0.5)
x <- rbinom(n, 1, 0.5)

n_in_subcluster <- 2
n_subclusters <- n_clusters * n_in_cluster/n_in_subcluster
subcluster <- rep(1:n_subclusters, each = n_in_subcluster)
data = list(y = y, x = x, cluster = cluster, subcluster = subcluster)

devfun_approx <- glmerSR(y ~ x + (1 | cluster) + (1 | subcluster),
                         data = data, family = binomial,
                         devFunOnly = TRUE)
devfun_10_4 <- glmerSR(y ~ x + (1 | cluster) + (1 | subcluster),
                       data = data, family = binomial,
                       nAGQ = 10, k = 4,
                       devFunOnly = TRUE)

devfun_approx(c(0.5, 1.5, -0.5))
devfun_10_4(c(0.5, 1.5, -0.5))
