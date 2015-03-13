check_subexpr <- function(x, data) {
  if (is.atomic(x)) {
    return(x)
  } else if (is.name(x)) {
    if (exists(x, data)) {

    }

  } else if (is.call(x)) {

  } else if (is.pairlist(x)) {

  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}
