number_clique <- function(clique, n, m){
  clique <- sort(clique)
  if(length(clique) < m) {
    clique <- c(clique, rep(0, m - length(clique)))
  }
  clique_number <- sum(clique * (n + 1) ^ (m:1))
  return(clique_number)
}

sort_cliques <- function(cliques, n){
  cliques <-  lapply(cliques, sort)
  m <- max(vapply(cliques, length, 1L))
  clique_numbers <- sapply(cliques, number_clique, n = n, m = m)
  return(cliques[order(clique_numbers)])
}
