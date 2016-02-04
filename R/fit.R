#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the
#'  variables named in \code{formula}, and in any of the subformulas.
#' @param control a list of extra parameters controlling the approximation
#'  to the likelihood
#' @inheritParams lme4::glmer
#' @export
glmm <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                 method = NULL, control = list(), weights = NULL, offset = NULL)
{
  con <- find_control_with_defaults(control, method)
  check_weights(weights)

  modfr <- find_modfr_glmm(formula, subformula = subformula, data = data,
                           family = family, weights = weights, offset = offset)

  if(has_reTrms(modfr)) {
    lfun <- find_lfun_glmm(modfr, method = method, control = con)

    p_beta <- ncol(modfr$X)
    p_theta <- length(modfr$reTrms$theta)
    opt <- optimizeGlmm(lfun, p_beta = p_beta, p_theta = p_theta)
    if(all(modfr$reTrms$lower == 0)) {
      opt$estim[1:p_theta] <- abs(opt$estim[1:p_theta])
      result <- glmmMod(list(estim = opt$estim, Sigma = opt$Sigma,
                             lfun = lfun, modfr = modfr,
                             method = method, control = con))
    }else{
      warning("proper print and summary method not yet implemented ",
              "for correlated random effects")
      result <- opt
    }
    return(result)

  } else {
    stop("haven't yet implemented no random effects case")
  }
}
