library(glmmsr)
context("Split up model frame")

set.seed(1)

n_clusters <- 5
n_in_cluster <- 6
n <- n_clusters*n_in_cluster

y <- rbinom(n, 1, 0.5)
x <- rbinom(n, 1, 0.5)

cluster <- rep(1:n_clusters, each = n_in_cluster)
data = list(y = y, x = x, cluster = cluster)
modfr <- parse_formula(y ~ x + (1 | cluster), data = data,
                       family = binomial, weights = NULL, off = NULL)
q <- nrow(modfr$reTrms$Zt)

# for a three-level model
n_in_subcluster <- 2
n_subclusters <- n_clusters * n_in_cluster/n_in_subcluster
subcluster <- rep(1:n_subclusters, each = n_in_subcluster)
data_3 = list(y = y, x = x, cluster = cluster, subcluster = subcluster)
modfr_3 <- parse_formula(y ~ x + (1 | cluster) + (1 | subcluster),
                         data = data_3,
                         family = binomial, off = NULL, weights = NULL)
q_3 <- nrow(modfr_3$reTrms$Zt)

# devfun_3 <- do.call(lme4::mkGlmerDevfun, modfr_3)
# devfun_3 <- updateGlmerDevfun(devfun_3, modfr_3$reTrms)
# devfun_3(c(0.5, 1.2, 1.2, -0.2))
#
# PR <- get("pp", environment(devfun_3))
# L_tot <- expand(PR$L())
# L <- L_tot$L
# P <- L_tot$P
# t(P)%*%L%*%t(L)%*%P
#
# Sigma_inv <- t(P)%*%tcrossprod(L)%*%P
# mu <- PR$delu
#
# save_normal(mu, Sigma_inv, file = "normal_three_level.txt")


test_that("find posterior dependence graph correctly", {
  act <- find_active(modfr)
  G <- find_pdg(act, q)
  expect_equal(length(igraph::V(G)), q)
  expect_equal(length(igraph::E(G)), 0)
  act_3 <- find_active(modfr_3)
  G_3 <- find_pdg(act_3, q_3)
  expect_equal(length(igraph::V(G_3)), q_3)
  expect_equal(length(igraph::E(G_3)), n_clusters*n_in_cluster/n_in_subcluster)
})

test_that("reordering model frame keeps devfun unchanged", {
  devfun <- lme4::mkGlmerDevfun(modfr$fr, modfr$X, modfr$reTrms, modfr$family)
  elim_order <- c(2:q, 1)
  modfr2 <- reorder_modfr(modfr, elim_order)
  devfun2 <- lme4::mkGlmerDevfun(modfr2$fr, modfr2$X, modfr2$reTrms,
                                 modfr2$family)
  expect_equal(devfun(1), devfun2(1))
})

lmodfr <- split_modfr(modfr)
lmodfr_3 <- split_modfr(modfr_3)

test_that("split_modfr partitions obs", {
  n <- nrow(modfr$X)
  expect_equal(sum(sapply(lapply(lmodfr, "[[", "X"), nrow)), n)
  n_3 <- nrow(modfr_3$X)
  expect_equal(sum(sapply(lapply(lmodfr_3, "[[", "X"), nrow)), n_3)
})

test_that("split_modfr gives correct cliques", {
  C1_expect <- as.numeric(which(modfr_3$reTrms$Zt[,1] != 0))
  expect_equal(lmodfr_3[[1]]$C, C1_expect)
})

test_that("saves list of little model frames in correct form", {
  save_lmodfrs(lmodfr_3, q_3, file = "tmp.txt")
  expect_equal(readLines("tmp.txt"),
               readLines("lmodfrs.txt"))
  file.remove("tmp.txt")
})

