check_if_order_implemented <- function(family, order) {
  stop("higher-order Laplace approximation not yet implemented",
       call. = FALSE)
}

find_lfun_Laplace <- function(modfr, devfun_laplace_1, order) {
  if(order == 1L ) {
    lfun <- function(x) { -devfun_laplace_1(x) / 2 }
  }
  else {
    check_if_order_implemented(modfr$family, order)
  }
  lfun
}
