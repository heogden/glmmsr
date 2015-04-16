library(glmmsr)
context("Graph operations")

edges <- cbind(c(1, 2, 3, 4, 1), c(2, 3, 4, 5, 3))

g <- igraph::graph.edgelist(edges, directed = FALSE)

test_that("sorts cliques correctly", {
  cliques <- igraph::maximal.cliques(g)
  n <- length(igraph::V(g))
  cliques_sorted <- sort_cliques(cliques, n)
  expect_equal(cliques_sorted[[1]], c(1, 2, 3))
  expect_equal(cliques_sorted[[2]], c(3, 4))
  expect_equal(cliques_sorted[[3]], c(4, 5))
})

