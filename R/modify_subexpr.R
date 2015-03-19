# add an attribute "sub" to each element of x (recursively),
# to say whether that element contains subvar
add_sub_status <- function(x, subvar) {
  if(is.atomic(x)){
    attr(x, "sub") <- FALSE
    x
  } else if(is.name(x)) {
    if(identical(as.character(x), subvar)) {
      attr(x, "sub") <- TRUE
    } else {
      attr(x, "sub") <- FALSE
    }
    x
  } else if (is.call(x)) {
    x_list <- lapply(x, add_sub_status, subvar = subvar)
    sub_status <- any(vapply(x_list, attr, TRUE, which = "sub", exact = TRUE))
    x <- as.call(x_list)
    attr(x, "sub") <- sub_status
    x
  } else if(is.pairlist(x)) {
    x_list <- lapply(x, add_sub_status, subvar = subvar)
    sub_status <- any(vapply(x_list, attr, TRUE, which = "sub", exact = TRUE))
    x <- as.pairlist(x_list)
    attr(x, "sub") <- sub_status
    x
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

modify_subexpr <- function(x, subvar) {
  x <- add_sub_status(x, subvar)
  if(is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    arg1sub <- FALSE
    arg2sub <- FALSE
    if(length(x) > 1L) {
      arg1sub <- attr(x[[2]], "sub")
      if(length(x) < 2L) {
        arg2sub <- attr(x[[3]], "sub")
      }
    }
    if(arg1sub || arg2sub) {
      if(identical(x[[1]], quote(`[`)) && arg1sub && !arg2sub) {
        x[[1]] <- quote(`[fr`)
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
