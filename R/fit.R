#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the
#'  variables named in \code{formula}, and in any of the subformulas.
#' @param method the method used to approximate the likelihood. The options
#'  are \code{"Laplace"}, \code{"AGQ"} (the adaptive Gaussian quadrature approximation,
#'  from \code{lme4}), \code{"SR"} (the sequential reduction approximation)
#'  and \code{"IS"} (an importance sampling approximation).
#' @param control a list of extra parameters controlling the approximation
#'  to the likelihood. See 'Details' for more information.
#' @param prev_fit a \code{glmmFit} object, the result of a previous model fit.
#' @param verbose controls how much detail to print out while fitting the model.
#'  For verbose = 0, print nothing. For verbose = 1 (the default), print
#'  output approximately once a second during model fitting. For verbose = 2,
#'  print out the parameter value and log-likelihood at every stage of
#'  optimization.
#' @param lme4_control the result of a call to \code{lme4_control}, containing
#'  control parameters passed to \code{lme4}. See \code{?lme4_control}.
#' @inheritParams lme4::glmer
#' @details The \code{control} argument is a list, used to specify further
#'  arguments controlling the approximation to the likelihood:
#'  \describe{
#'   \item{\code{nAGQ}}{the number of adaptive Gaussian quadrature points.
#'   Only used if \code{method = "AGQ"}. Defaults to 15.}
#'   \item{\code{nSL}}{the level of sparse grid storage.
#'   Only used if \code{method = "SR"}. Defaults to 3.}
#'   \item{\code{nIS}}{the number of samples to use for importance sampling.
#'   Only used if \code{method = "IS"}. Defaults to 1000.}
#'   \item{\code{order}}{the order of Laplace approxiation.
#'   only used if \code{method = "Laplace"}. Defaults to 1.}
#'   \item{\code{check_Laplace}}{should quality of first-order Laplace
#'   approximation be checked? Only used  if \code{method = "Laplace"}
#'   and \code{order = 1}. Defaults to TRUE.}
#'   \item{\code{divergence_threshold}}{if \code{check_Laplace = TRUE},
#'   warn about quality of inference using the first-order Laplace
#'   approximation if measure of divergence from inference with
#'   second-order Laplace approximation exceeds \code{divergence_threshold}.
#'   Defaults to 0.1.}
#'  }
#' @return An object of the class \code{glmmFit}
#' @example inst/examples/three_level.R
#' @export
glmm <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                 method = NULL, control = list(), weights = NULL, offset = NULL,
                 prev_fit = NULL, verbose = 1L, lme4_control = set_lme4_control())
{
  check_weights(weights)
  con <- find_control_with_defaults(control, method)

  modfr <- find_modfr_glmm(formula, subformula = subformula, data = data,
                           family = family, weights = weights, offset = offset)

  if(has_reTrms(modfr)) {
    devfun_laplace_1 <- find_devfun_laplace_1(modfr, lme4_control)
    lfun <- find_lfun_glmm_internal(modfr, method = method, control = con,
                                    devfun_laplace_1 = devfun_laplace_1)

    p_beta <- ncol(modfr$X)
    p_theta <- length(modfr$reTrms$theta)
    opt <- optimize_glmm(lfun, p_beta = p_beta, p_theta = p_theta,
                         prev_fit = prev_fit, verbose = verbose)
    if(con$check_Laplace) {
      opt$laplace_divergence <- find_laplace_divergence(opt, modfr, devfun_laplace_1)
      if(opt$laplace_divergence > con$divergence_threshold)
        warning("Inference using the first-order Laplace approximation may be unreliable in this case",
                call. = FALSE)
    }
    if(all(modfr$reTrms$lower == 0)) {
      opt$estim[1:p_theta] <- abs(opt$estim[1:p_theta])
      if(any(opt$estim[1:p_theta] > 4.9)) {
        opt$Sigma <- matrix(NA, nrow = length(opt$estim), ncol = length(opt$estim))
        warning("The approximate MLE might not be finite", call. = FALSE)
      }
      result <- glmmFit(list(estim = opt$estim, Sigma = opt$Sigma,
                             lfun = lfun, modfr = modfr,
                             method = method, control = con,
                             laplace_divergence = opt$laplace_divergence))
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
