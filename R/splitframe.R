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
  G <- igraph::graph(edges, n = q, directed = FALSE)
  igraph::simplify(G)
}

add_obs_C <- function(C, act, obs_rem) {
  obs_C <- obs_rem[vapply(act[obs_rem],
                          function(items){all(is.element(items, C))},
                          TRUE)]
  list(C = C, obs_C = obs_C)
}

add_obs <- function(cliques, act, obs) {
  obs_rem <- obs
  cliques_ext <- list()
  for(i in seq_along(cliques)) {
    cliques_ext[[i]] <- add_obs_C(cliques[[i]], act, obs_rem)
    obs_rem <- setdiff(obs_rem, cliques_ext[[i]]$obs)
  }
  if(length(obs_rem) != 0) {
    stop("Couldn't allocate some observations to a clique")
  }
  cliques_ext
}

find_local_term <- function(clique_ext, modfr) {
  C <- clique_ext$C
  obs_C <- clique_ext$obs_C
  X_C <- unname(modfr$X[obs_C, , drop = FALSE])
  Zt_C <- unname(as.matrix(modfr$reTrms$Zt[C, obs_C, drop = FALSE]))
  Lambdat_C <- modfr$reTrms$Lambdat[C, C, drop = FALSE]
  Lind_mat <- modfr$reTrms$Lambdat
  Lind_mat@x <- as.numeric(modfr$reTrms$Lind) - 1
  Lind_C <- as.integer(Lind_mat[C, C, drop = FALSE]@x)
  resp <- model.response(modfr$fr)
  weights <- model.weights(modfr$fr)
  if(is.null(weights)) {
    weights_C <- rep(1, length(obs_C))
  } else {
    weights_C <- weights[obs_C]
  }
  if(NCOL(resp) == 1) {
    resp_C <- resp[obs_C]
    resp_C <- matrix(unname(resp_C), ncol = 1)
  } else {
    #####################
    # FIXME: should convert to vector form
    resp_C <- resp[obs_C, , drop = FALSE]
    #####################
  }
  list(C = C, X = X_C, Zt = Zt_C, Lambdat = Lambdat_C,
       Lind = Lind_C, resp = resp_C, weights = weights_C)
}

split_modfr <- function(modfr) {
  act <- find_active(modfr)
  n <- ncol(modfr$reTrms$Zt)
  q <- nrow(modfr$reTrms$Zt)
  G <- find_pdg(act, q)
  elim_order <- find_elim_order(G)
  modfr <- reorder_modfr(modfr, elim_order)
  act <- find_active(modfr)
  G <- find_pdg(act, q)
  cliques <- igraph::maximal.cliques(G)
  cliques <- sort_cliques(cliques, n)
  cliques_ext <- add_obs(cliques, act, 1:n)
  lapply(cliques_ext, find_local_term, modfr = modfr)
}

lmodfr_to_oneline <- function(lmodfr, file = "") {
  size_clique <- length(lmodfr$C)
  nobs <- nrow(lmodfr$X)
  nfixed <- ncol(lmodfr$X)
  Lambdat_triplet <- as(lmodfr$Lambdat, "dgTMatrix")
  nentries <- length(Lambdat_triplet@i)
  Lambdat_print <- as.numeric(rbind(Lambdat_triplet@i, Lambdat_triplet@j,
                                    Lambdat_triplet@x))
  ret <- c("G", size_clique, lmodfr$C, nobs, nfixed, lmodfr$X, lmodfr$Zt,
           nentries, Lambdat_print,
           lmodfr$Lind, lmodfr$resp, lmodfr$weights)
  cat(cat(ret, file = file, append = TRUE),
      "\n", sep = "", file = file, append = TRUE)
}

save_normal_terms <- function(q, file = "") {
  for(i in 1:q){
    cat("N ", i, "\n", sep = "", file = file, append = TRUE)
  }
}

save_lmodfrs <- function(lmodfrs, q, file = "") {
  if(file.exists(file)) {
    file.remove(file)
  }
  a <- lapply(lmodfrs, lmodfr_to_oneline, file = file)
  a <- save_normal_terms(q, file = file)
}
