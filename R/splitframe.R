# Find the active set of items for each observation
find_active <- function(modfr) {
  Zt <- modfr$reTrms$Zt
  Lambdat <- modfr$reTrms$Lambdat
  LambdatZt <- Lambdat %*% Zt
  LambdatZt <- as(LambdatZt, "dgTMatrix")
  tapply(LambdatZt@i + 1, LambdatZt@j + 1, c, simplify = FALSE)
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
  Lind_mat@x <- as.numeric(modfr$reTrms$Lind)
  Lind_C <- as.integer(Lind_mat[C, C, drop = FALSE]@x) - 1
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

find_factorization_terms <- function(modfr) {
  act <- find_active(modfr)
  cliques <- unname(unique(act))
  n_obs <- ncol(modfr$reTrms$Zt)
  n_re <- nrow(modfr$reTrms$Zt)
  cliques_ext <- add_obs(cliques, act, 1:n_obs)
  factorization_terms <- rgraphpass::continuous_beliefs()
  for(i in seq_along(cliques_ext)) {
    lmodfr <- find_local_term(cliques_ext[[i]], modfr)
    factorization_terms$append_glmm_belief(items = lmodfr$C - 1,
                                           X = lmodfr$X,
                                           Zt = lmodfr$Zt,
                                           Lambdat = lmodfr$Lambdat,
                                           Lind = lmodfr$Lind,
                                           response = lmodfr$resp,
                                           weights = lmodfr$weights)
  }
  I1 <- matrix(1, nrow = 1, ncol = 1)
  for(i in seq_len(n_re)) {
    factorization_terms$append_normal_belief(items = c(i - 1),
                                             mean = c(0),
                                             precision = I1)
  }
  return(factorization_terms)
}
