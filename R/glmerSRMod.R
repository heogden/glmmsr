#' Construct a glmerSRMod object
#'
#' @param x a list
#' @return An object of class \code{glmerSRMod}
#' @export
glmerSRMod <- function(x) {
  structure(x, class = "glmerSRMod")
}


#' Construct a summaryGlmerSRMod object
#'
#' @param x a list
#' @return An object of class \code{summaryGlmerSRMod}
#' @export
summaryGlmerSRMod <- function(x) {
  structure(x, class = "summaryGlmerSRMod")
}


#' Print glmerSRMod object
#'
#' @param x glmerSRMod object
#' @param ... ignored
#' @method print glmerSRMod
#' @export
print.glmerSRMod <- function(x, ...){
  cat("Generalized linear mixed model fit by maximum likelihood ",
      "(Sequential Reduction Approximation, k = ", x$k, ", nAGQ = ", x$nAGQ, ") [glmerSRMod] \n",
      sep = "")
  cat("Family:", x$modfr$family$family, "(", x$modfr$family$link, ") \n")
  cat("Formula: ")
  print(x$modfr$formula, showEnv=FALSE)
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
  print(estim_random_df, digits = 4,right = FALSE, row_names = FALSE)
  nlevels_each <- sapply(x$modfr$reTrms$flist, nlevels)
  y <- model.response(x$modfr$fr)
  nobs <- NROW(y)
  cat("Number of obs: ", nobs, ", groups: ", sep="")
  for(i in 1:length(group_names)) {
    cat(group_names[i], nlevels_each[i], sep=", ")
    cat("; ")
  }
  cat("\n")
  cat("Fixed effects: \n")
  print(estim_fixed, digits = 4)
}

#' Summarize a glmerSRMod fit
#'
#' @param object glmerSRMod object
#' @param ... ignored
#' @return An object of class \code{summaryGlmerSRMod}
#' @method summary glmerSRMod
#' @export
summary.glmerSRMod <- function(object, ...) {
  se <- sqrt(diag(object$Sigma))
  z <- abs(object$estim / se)
  p_value <- 2 * pnorm(z, lower.tail=FALSE)
  result <- summaryGlmerSRMod(list(fit = object, se = se, z = z,
                                   p_value = p_value))
  return(result)
}

#' Print summaryGlmerSRMod object
#'
#' @param x summaryGlmerSRMod object
#' @param ... ignored
#' @method print summaryGlmerSRMod
#' @export
print.summaryGlmerSRMod <- function(x, ...){
  fit <- x$fit
  cat("Generalized linear mixed model fit by maximum likelihood ",
      "(Sequential Reduction Approximation, k = ", fit$k, ", nAGQ = ", fit$nAGQ, ") [glmerSRMod] \n",
      sep = "")
  cat("Family:", fit$modfr$family$family, "(", fit$modfr$family$link, ") \n")
  cat("Formula: ")
  print(fit$modfr$formula, showEnv=FALSE)
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
