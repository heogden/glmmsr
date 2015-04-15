number_clique <- function(clique, n, m){
  clique <- sort(clique)
  if(length(clique) < m) {
    clique <- c(clique, rep(0, m - length(clique)))
  }
  clique_number <- sum(clique * (n + 1) ^ (m:1))
  return(clique_number)
}

sort_cliques <- function(cliques){
  cliques <-  lapply(cliques, sort)
  m <- igraph::clique.number(G)
  n <- length(igraph::V(G))
  clique_numbers <- sapply(cliques, number_clique, n = n, m = m)
  return(cliques[order(clique_numbers)])
}
