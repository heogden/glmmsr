#' Construct a glmmMod object
#'
#' @param x a list
#' @return An object of class \code{glmmMod}
#' @export
glmmMod <- function(x) {
  structure(x, class = "glmmMod")
}


#' Construct a summaryGlmmMod object
#'
#' @param x a list
#' @return An object of class \code{summaryGlmmMod}
#' @export
summaryGlmmMod <- function(x) {
  structure(x, class = "summaryGlmmMod")
}

#' Find the name of the likelihood approximation used for fitting
#'
#' @inheritParams glmm
#' @return a string with the name of the likelihood approximation
find_approximation_name <- function(method, control) {
  switch(method,
         "Laplace" = "Laplace approximation (lme4)",
         "AGQ" = paste("Adaptive Gaussian Quadrature with", control$nAGQ, "points (lme4)"),
         "SR" = paste("Sequential reduction at level", control$nSL),
         stop(cat("method", method, "not found")))
}


print_subformula <- function(subformula) {
  if(length(subformula) > 0) {
    if(is.list(subformula)) {
      subform <- subformula
    } else {
      subform <- list(subformula)
    }
    if(length(subform) > 1) {
      cat("Subformulas: \n")
    } else {
      cat("Subformula: ")
    }
    out <- lapply(subform, print, showEnv = FALSE)
  }
}

#' Print glmmMod object
#'
#' @param x glmmMod object
#' @param ... ignored
#' @method print glmmMod
#' @export
print.glmmMod <- function(x, ...){
  name <- find_approximation_name(x$method, x$control)
  cat("Generalized linear mixed model fit by maximum likelihood [glmmMod] \n")
  cat("Likelihood approximation:", name, "\n \n")
  cat("Family:", x$modfr$family$family, "(", x$modfr$family$link, ") \n")
  cat("Formula: ")
  print(x$modfr$formula, showEnv=FALSE)
  print_subformula(x$modfr$subformula)
  cat("\n")

  names_beta <- attr(x$modfr$X,"dimnames")[[2]]
  group_names <- names(x$modfr$reTrms$cnms)
  names_theta <- unlist(unname(x$modfr$reTrms$cnms))
  p_beta <- ncol(x$modfr$X)
  p_theta <- length(x$modfr$reTrms$theta)
  p <- p_beta + p_theta
  estim_fixed <- x$estim[(p_theta+1):p]
  names(estim_fixed) <- names_beta
  estim_random <- x$estim[1:p_theta]
  estim_random_df <- data.frame("Groups" = group_names,
                                "Name" = names_theta,
                                "Estimate" = estim_random)

  cat("Random effects:\n")
  print(estim_random_df, digits = 4,right = FALSE, row_names = FALSE)
  nlevels_each <- sapply(x$modfr$reTrms$flist, nlevels)
  y <- model.response(x$modfr$fr)
  nobs <- NROW(y)
  cat("Number of obs: ", nobs, ", groups: ", sep="")
  for(i in 1:length(group_names)) {
    cat(group_names[i], nlevels_each[i], sep=", ")
    cat("; ")
  }
  cat("\n \n")
  cat("Fixed effects: \n")
  print(estim_fixed, digits = 4)
}

#' Summarize a glmmMod fit
#'
#' @param object glmmMod object
#' @param ... ignored
#' @return An object of class \code{summaryGlmmMod}
#' @method summary glmmMod
#' @export
summary.glmmMod <- function(object, ...) {
  se <- sqrt(diag(object$Sigma))
  z <- abs(object$estim / se)
  p_value <- 2 * pnorm(z, lower.tail=FALSE)
  result <- summaryGlmmMod(list(fit = object, se = se, z = z, p_value = p_value))
  return(result)
}

#' Print summaryGlmmMod object
#'
#' @param x summaryGlmmMod object
#' @param ... ignored
#' @method print summaryGlmmMod
#' @export
print.summaryGlmmMod <- function(x, ...){
  fit <- x$fit
  name <- find_approximation_name(fit$method, fit$control)
  cat("Generalized linear mixed model fit by maximum likelihood [glmmMod] \n")
  cat("Likelihood approximation:", name, "\n \n")
  cat("Family:", fit$modfr$family$family, "(", fit$modfr$family$link, ") \n")
  cat("Formula: ")
  print(fit$modfr$formula, showEnv=FALSE)
  print_subformula(fit$modfr$subformula)
  cat("\n")

  cat("Random effects:\n")
  names_beta <- attr(fit$modfr$X, "dimnames")[[2]]
  group_names <- names(fit$modfr$reTrms$cnms)
  names_theta <- unlist(unname(fit$modfr$reTrms$cnms))
  p_beta <- ncol(fit$modfr$X)
  p_theta <- length(fit$modfr$reTrms$theta)
  p <- p_beta + p_theta
  estim_random <- fit$estim[1:p_theta]
  se_random <- x$se[1:p_theta]
  estim_random_df <- data.frame("Groups" = group_names,
                                "Name" = names_theta,
                                "Estimate" = estim_random,
                                "Std.Error" = se_random)
  print(estim_random_df, digits = 4, right = FALSE, row.names=FALSE)
  nlevels_each <- sapply(fit$modfr$reTrms$flist, nlevels)
  y <- model.response(fit$modfr$fr)
  nobs <- NROW(y)
  cat("Number of obs: ", nobs, ", groups: ", sep="")
  for(i in 1:length(group_names)) {
    cat(group_names[i], nlevels_each[i], sep=", ")
    cat("; ")
  }
  cat("\n")
  cat("\n")
  cat("Fixed effects: \n")
  which_fixed <- (p_theta+1):p
  estim_fixed <- fit$estim[which_fixed]
  se_fixed <- x$se[which_fixed]
  z_fixed <- x$z[which_fixed]
  p_value_fixed <- x$p_value[which_fixed]
  fixed_mat <- cbind(estim_fixed, se_fixed, z_fixed, p_value_fixed)
  rownames(fixed_mat) <- names_beta
  colnames(fixed_mat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  print(fixed_mat, digits = 4)
}

coef_random.glmmMod <- function(x) {
  p_theta <- length(x$modfr$reTrms$theta)

  names_theta <- unlist(unname(x$modfr$reTrms$cnms))
  estim_random <- x$estim[1:p_theta]
  names(estim_random) <- names_theta

  estim_random
}

coef_fixed.glmmMod <- function(x) {
  p_theta <- length(x$modfr$reTrms$theta)
  p_beta <- ncol(x$modfr$X)
  p <- p_beta + p_theta

  names_beta <- attr(x$modfr$X,"dimnames")[[2]]
  which_fixed <- (p_theta+1):p
  estim_fixed <- x$estim[which_fixed]
  names(estim_fixed) <- names_beta

  estim_fixed
}


coef.glmmMod <- function(x) {
  list(fixed = coef_fixed.glmmMod(x), random = coef_random.glmmMod(x))
}
