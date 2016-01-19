#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the
#'  variables named in \code{formula}, and in any of the subformulas.
#' @param control the output of a call to \code{\link{glmmControl}}, a list
#'  containing control paramaters.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @inheritParams lme4::glmer
#' @export
glmm <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                 control = glmmControl(), verbose = 0L, nAGQ = 1L, k = 0L,
                 weights = NULL, offset = NULL, devFunOnly = FALSE)
{
  modfr <- glFormulaSub(formula, subformula = subformula, data = data,
                        family = family, control = control, weights = weights,
                        offset = offset)
  if(has_reTrms(modfr)) {
    devfun <- do.call(mkGlmmDevfun, c(modfr, list(verbose = verbose,
                                                     control = control,
                                                     nAGQ = nAGQ,
                                                     k = k)))

    if(devFunOnly) {
      return(devfun)
    } else {
      if(k == 0L) {
        opt <- optimizeGlmer(devfun, optimizer = control$optimizer[[2]],
                             restart_edge = control$restart_edge,
                             boundary.tol = control$boundary.tol,
                             control = control$optCtrl,
                             verbose = verbose,
                             stage = 2,
                             calc.derivs = control$calc.derivs,
                             use.last.params = control$use.last.params)
        return(mkMerMod(environment(devfun), opt, modfr$reTrms, fr = modfr$fr))
      } else {
        p_beta <- ncol(modfr$X)
        p_theta <- length(modfr$reTrms$theta)
        opt <- optimizeGlmm(devfun, p_beta = p_beta, p_theta = p_theta,
                                   verbose = verbose)
        if(all(modfr$reTrms$lower == 0)) {
          opt$estim[1:p_theta] <- abs(opt$estim[1:p_theta])
          result <- glmmMod(list(estim = opt$estim, Sigma = opt$Sigma,
                                 devfun = devfun, modfr = modfr, k = k,
                                 nAGQ = nAGQ))
        }else{
          warning("proper print and summary method not yet implemented ",
                  "for correlated random effects")
          result <- opt
        }
        return(result)
      }

    }
  } else {
    stop("haven't yet implemented no random effects case")
  }
}
