has_re <- function(formula) {
  length(lme4::findbars(formula[[length(formula)]])) > 0L
}

find_dim_in_dir <- function(x, i) {
  d <- length(dim(x))
  if(d == 0 && i == 1) {
    # have a vector
    return(length(x))
  } else if(d < i) {
    stop(paste0("Array does not have dimension ", i), call. = FALSE)
  } else {
    return(dim(x)[i])
  }
}

subset_dim <- function(x, i, indices){
  d <- length(dim(x))
  if(d == 0 && i == 1) {
    # have a vector
    return(x[indices])
  }
  if(d < i) {
    stop(paste0("Array does not have dimension ", i), call. = FALSE)
  }
  if(i > 1L) {
    commas_before <- paste(rep(",", times = i - 1), collapse = " ")
  } else {
    commas_before <- character(0)
  }
  if(i < d) {
    commas_after <- paste(rep(",", times = d - i), collapse = " ")
  } else {
    commas_after <- character(0)
  }
  expr_text <- paste("x[", commas_before, "indices", commas_after,
                     ", drop = FALSE]")
  eval(parse(text = expr_text))
}
