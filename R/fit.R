#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the
#'  variables named in \code{formula}, and in any of the subformulas.
#' @param control the output of a call to \code{\link{glmmsrControl}}, a list
#'  containing control paramaters.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @inheritParams lme4::glmer
#' @export
glmerSR <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                    control = glmmsrControl(), verbose = 0L, nAGQ = 1L, k = 0L,
                    offset = NULL, devFunOnly = FALSE)
{
  if(k > 0L) {
    stop("Sequential reduction approximation not yet implemented")
  }
  modfr <- glFormulaSub(formula, subformula = subformula, data = data,
                        family = family, control = control, offset = offset)
  if(has_reTrms(modfr)) {
    modfr_ext <- modfr
    modfr_ext$control <- control
    devfun <- do.call(mkGlmerDevfun, c(modfr, list(verbose = verbose,
                      control = control, nAGQ = nAGQ)))
    if(!devFunOnly){
      opt <- optimizeGlmer(devfun, verbose = verbose)
    }
    devfun <- updateGlmerDevfun(devfun, modfr$reTrms, nAGQ = nAGQ)
    if(devFunOnly) {
      return(devfun)
    } else {
      opt <- optimizeGlmer(devfun, optimizer = control$optimizer[[2]],
                           restart_edge = control$restart_edge,
                           boundary.tol = control$boundary.tol,
                           control = control$optCtrl,
                           verbose = verbose,
                           stage = 2,
                           calc.derivs = control$calc.derivs,
                           use.last.params = control$use.last.params)
      return(mkMerMod(environment(devfun), opt, modfr$reTrms, fr = modfr$fr))
    }
  } else {
    stop("haven't yet implemented no random effects case")
  }
}
