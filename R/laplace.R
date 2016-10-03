check_if_order_implemented <- function(family, order) {
  if(order > 2L) {
    stop("order-", order, " Laplace approximation not yet implemented",
         call. = FALSE)
  }
}

find_lfun_Laplace <- function(modfr, devfun_laplace_1, order) {
  if(order == 1L ) {
    lfun <- function(x) { -devfun_laplace_1(x) / 2 }
  }
  else {
    check_if_order_implemented(modfr$family, order)
    if(order == 2L) {
      epsilon_1 <- find_epsilon_1(modfr, devfun_laplace_1)
      lfun <- function(x){ -devfun_laplace_1(x) / 2 + epsilon_1(x) }
    }
  }
  lfun
}
