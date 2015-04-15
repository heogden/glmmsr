# Find the active set of items for each observation
find_active <- function(modfr) {
  Zt <- modfr$reTrms$Zt
  Lambdat <- modfr$reTrms$Lambdat
  LambdatZt <- Lambdat %*% Zt
  LambdatZt <- as(LambdatZt, "dgTMatrix")
  tapply(LambdatZt@i + 1, LambdatZt@j + 1, c, simplify = FALSE)
}

# Find the posterior dependence graph, given the active set for each observation
find_pdg <- function(act, q) {
  edges <- unlist(lapply(unique(act), find_pairs))
  G <- igraph::graph.empty(q, directed = FALSE)
  if(length(edges) > 0L) {
    edge_list <- unique(matrix(edges, ncol = 2, byrow = TRUE))
    G <- igraph::add.edges(G, edge_list)
  }
  G
}

add_items_obs_C <- function(C, act, items_rem, obs_rem) {
  items_C <- intersect(C, items_rem)
  obs_C <- obs_rem[vapply(act[obs_rem],
                          function(items){all(is.element(items, C))},
                          TRUE)]
  list(C = C, items_C = items_C, obs_C = obs_C)

}

add_items_obs <- function(cliques, act, items, obs) {
  items_rem <- items
  obs_rem <- obs
  cliques_ext <- list()
  for(i in seq_along(cliques)) {
    cliques_ext[[i]] <- add_items_obs_C(cliques[[i]], act, items_rem, obs_rem)
    items_rem <- setdiff(items_rem, cliques_ext[[i]]$items)
    obs_rem <- setdiff(obs_rem, cliques_ext[[i]]$obs)
  }
  cliques_ext
}

find_local_term <- function(clique_ext, modfr) {
  C <- clique_ext$C
  items_C <- clique_ext$items_C
  obs_C <- clique_ext$obs_C
  X_C <- modfr$X[obs_C, , drop = FALSE]
  Zt_C <- unname(as.matrix(modfr$reTrms$Zt[C, obs_C, drop = FALSE]))
  Lambdat_C <- modfr$reTrms$Lambdat[C, C, drop = FALSE]
  Lind_mat <- modfr$reTrms$Lambdat
  Lind_mat@x <- as.numeric(modfr$reTrms$Lind)
  Lind_C <- as.integer(Lind_mat[C, C, drop = FALSE]@x)
  resp <- model.response(modfr$fr)
  if(NCOL(resp) == 1) {
    resp_C <- resp[obs_C]
  } else {
    resp_C <- resp[obs_C, , drop = FALSE]
  }
  list(C = C, X = X_C, Zt = Zt_C, Lambdat = Lambdat_C,
       Lind = Lind_C, resp = resp_C, items_C = items_C)
}

split_modfr <- function(modfr) {
  act <- find_active(modfr)
  n <- ncol(modfr$reTrms$Zt)
  q <- nrow(modfr$reTrms$Zt)
  G <- find_pdg(act, q)
  elim_order <- find_elim_order(G)
  modfr <- reorder_modfr(modfr, elim_order)
  act <- find_active(modfr)
  G <- igraph::permute.vertices(G, elim_order)
  cliques <- igraph::maximal.cliques(G)
  cliques <- sort_cliques(cliques)
  cliques_ext <- add_items_obs(cliques, act, 1:q, 1:n)
  lapply(cliques_ext, find_local_term, modfr = modfr)
}
