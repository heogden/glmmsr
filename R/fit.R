#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the
#'  variables named in \code{formula}, and in any of the subformulas.
#' @param method the method used to approximate the likelihood. The options
#'  are \code{"Laplace"}, \code{"AGQ"} (the adaptive Gaussian quadrature approximation,
#'  from \code{lme4}), and \code{"SR"} (the sequential reduction approximation).
#' @param control a list of extra parameters controlling the approximation
#'  to the likelihood. See 'Details' for more information.
#' @param prev_fit a \code{glmmMod} object, the result of a previous model fit.
#' @param verbose controls how much detail to print out while fitting the model.
#'  For verbose = 0, print nothing. For verbose = 1 (the default), print
#'  output approximately once a second during model fitting.
#' @inheritParams lme4::glmer
#' @details The \code{control} argument is a list, used to specify further
#'  arguments controlling the approximation to the likelihood:
#'  \describe{
#'   \item{\code{nAGQ}}{the number of adaptive Gaussian quadrature points.
#'   Only used if \code{method = "AGQ"}. Defaults to 8.}
#'   \item{\code{nSL}}{the level of sparse grid storage.
#'   Only used if \code{method = "SR"}. Defaults to 3.}
#'  }
#' @return An object of the class \code{glmmMod}
#' @export
glmm <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                 method = NULL, control = list(), weights = NULL, offset = NULL,
                 prev_fit = NULL, verbose = 1L)
{
  check_weights(weights)

  modfr <- find_modfr_glmm(formula, subformula = subformula, data = data,
                           family = family, weights = weights, offset = offset)

  if(has_reTrms(modfr)) {
    lfun <- find_lfun_glmm(modfr, method = method, control = control)

    p_beta <- ncol(modfr$X)
    p_theta <- length(modfr$reTrms$theta)
    opt <- optimizeGlmm(lfun, p_beta = p_beta, p_theta = p_theta,
                        prev_fit = prev_fit, verbose = verbose)
    if(all(modfr$reTrms$lower == 0)) {
      opt$estim[1:p_theta] <- abs(opt$estim[1:p_theta])
      result <- glmmMod(list(estim = opt$estim, Sigma = opt$Sigma,
                             lfun = lfun, modfr = modfr,
                             method = method, control = control))
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
