# Eliminate a vertex, by joining its neighbours, then removing the vertex
eliminate <- function(v, G){
  Nv <- setdiff(unlist(igraph::neighborhood(G, 1, v)), v)
  G_new <- G
  if(length(Nv) > 1) {
    for(i in 2:length(Nv)) {
      for(j in 1:(i-1)) {
        G_new[Nv[i], Nv[j]] <- TRUE
      }
    }
  }
  igraph::induced.subgraph(G_new, setdiff(igraph::V(G_new), v))
}

# Find a good order in which to eliminate the variables
# Slow for very large graphs
find_elim_order <- function(G) {
  G <- igraph::set.vertex.attribute(G, "item", igraph::V(G), igraph::V(G))
  n <- length(V(G))
  order <- c()
  H <- G
  cost <- c()
  for(i in 1:n){
    eliminate_next <- which.min(igraph::degree(H))
    order[i] <- igraph::get.vertex.attribute(H, "item", eliminate_next)
    cost[i] <- igraph::degree(H, eliminate_next) + 1
    H <- eliminate(eliminate_next, H)
  }
  return(list(order = order, cost = cost))
}
