
find_indices <- function(x, i, var) {
  if(is.atomic(x) || is.name(x)) {
    NULL
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && identical(as.character(x[[2]]), var)) {
      return(as.character(x[[i+2]]))
    }
    unique(unlist(lapply(x, find_indices, i = i, var = var)))
  } else if(is.pairlist(x)) {
    unique(unlist(lapply(x, find_indices, i = i, var = var)))
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}


find_indices_subform <- function(sub, data) {
  subvar <- sub$subvar
  subform <- sub$subform
  subexpr <- sub$subexpr

  d <- find_dim_sub(subexpr, subvar)

  # check that we get same dimension from subform
  if(find_dim_sub(subform, subvar) != d) {
    stop(paste0("\'", subvar, "\' is indexed inconsistently"), call. = FALSE)
  }

  # find the indices used to index subvar in subexpr
  indices_subexpr <- list()
  indices_subform <- list()
  for(i in 1:d){
    indices_subexpr[[i]] <- find_indices(subexpr, i, subvar)
    # check all subexpr indices are in data
    in_data <- vapply(indices_subexpr[[i]], exists, TRUE, where = data)
    if(any(!in_data)) {
      stop(paste0("Can't find indexing variables: ",
                  indices_subexpr[[i]][!in_data]))
    }
    indices_i <- mget(indices_subexpr[[i]], as.environment(data))
    # coerce to factor with common levels
    index <- find_indices(subform, i, subvar)
    levels_i <- get(index, data)
    indices_i <- lapply(indices_i, factor, levels = levels_i)
    # replace in data, so have factors with correct levels
    for(j in seq_along(indices_i)) {
      data[[names(indices_i)[j]]] <- indices_i[[j]]
    }
    indices_subform[[i]] <- 1:length(levels_i)
    names(indices_subform)[i] <- index
  }
  return(list(data = data, indices_subform = indices_subform))
}


matrix_indexing <- function(x, indices) {
  if(is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && length(x) > 3) {
      y <- x[1:2]
      #indices[, as.list(x[-c(1,2)]), drop = FALSE]
      y[[3]] <- as.call(c(list(quote(cbind)), as.list(x[-c(1,2)])))
      return(y)
    } else{
      as.call(lapply(x, matrix_indexing, indices = indices))
    }
  } else if(is.pairlist(x)) {
    as.pairlist(lapply(x, matrix_indexing, indices = indices))

  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

flatten_formula <- function(formula, indices){
  # drop indexing in LHS of formula
  lhs_var <- find_subvar(formula, char = FALSE)
  tf <- terms(formula)
  tf[[2]] <- lhs_var
  formula_flat <- formula(tf)

  d <- length(indices)

  if(d > 1) {
    # each time we see `[`, extract relevant columns of index_matrix
    # and combine with cbind
    # for now, insist all indexing is done with indices in indices

    formula_flat <- matrix_indexing(formula_flat, indices)
  }
  formula_flat
}
