find_sub_status <- function(x, subvar) {
  if(is.logical(x)) {
    x
  }
  else if(is.name(x)) {
    identical(as.character(x), subvar)
  }
  else if (is.call(x) || is.pairlist(x) ) {
    sub_status_vector <- vapply(x, find_sub_status, TRUE, subvar = subvar)
    any(sub_status_vector)
  }
  else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

modify_subexpr <- function(x, subvar) {
  if(is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    arg1sub <- FALSE
    arg2sub <- FALSE
    if(length(x) > 1L) {
      arg1sub <- find_sub_status(x[[2]], subvar)
      if(length(x) > 2L) {
        arg2sub <- find_sub_status(x[[3]], subvar)
      }
    }
    if(arg1sub || arg2sub) {
      if(identical(x[[1]], quote(`[`)) && arg1sub && !arg2sub) {
        x[[1]] <- quote(`[fr`)
        if(length(x) > 3L) {
          # have multiple indexing: must flatten
          index <- parse(text = paste(x[-c(1, 2)], collapse = "_"))[[1]]
          x <- x[1:3]
          x[[3]] <- index
        }
      } else if(identical(x[[1]], quote(`+`))) {
        x[[1]] <- quote(`+fr`)
      } else if(identical(x[[1]], quote(`-`))) {
        x[[1]] <- quote(`-fr`)
      } else if(identical(x[[1]], quote(`*`))) {
        x[[1]] <- quote(`*fr`)
      } else if(identical(x[[1]], quote(`/`))) {
        x[[1]] <- quote(`/fr`)
      } else {
        stop("Substituted variables used in non-linear function",
             call. = FALSE)
      }
    }
    as.call(lapply(x, modify_subexpr, subvar = subvar))

  } else if(is.pairlist(x)) {
    as.pairlist(lapply(x, modify_subexpr, subvar = subvar))

  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

extract_to_flatten <- function(x, subvar) {
  if(is.atomic(x) || is.name(x)) {
    list()
  } else if(is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && length(x) > 3L && find_sub_status(x[[2]], subvar)) {
      # have multiple indexing: must flatten
      to_flatten <- paste(as.list(x)[-c(1, 2)])
      out <- list()
      out[[1]] <- to_flatten
      out <- unique(c(out, unlist(lapply(x, extract_to_flatten,
                                         subvar = subvar), recursive = FALSE)))
      out[vapply(out, length, 1L) > 0L]
    } else {
      out <- unique(unlist(lapply(x, extract_to_flatten, subvar = subvar),
                           recursive = FALSE))
      out[vapply(out, length, 1L) > 0L]
    }
  } else if(is.pairlist(x)) {
    out <- unique(unlist(lapply(x, extract_to_flatten, subvar = subvar),
                         recursive = FALSE))
    out[vapply(out, length, 1L) > 0L]
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}
