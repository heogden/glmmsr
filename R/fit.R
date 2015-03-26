#' Fit a GLMM
#'
#' @param subformula a subformula, describing how a substituted variable
#'  depends on covariates, or a list of subformulas, if there is more
#'  than one \code{Sub()} term in \code{formula}.
#' @param data an optional data frame, list or environment containing the variables
#'  named in \code{formula}, and in any of the subformulas.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @inheritParams lme4::glmer
#' @export
glmerSR <- function(formula, subformula = NULL, data = NULL, family = gaussian,
                    verbose = 0L, nAGQ = 1L, k = 0L, devFunOnly = FALSE) {
  if(is.list(subformula) || length(subformula) == 0L){
    subforms <- subformula
  } else {
    subforms <- list(subformula)
  }
  modfr <- glFormulaSub(formula, data, family, subforms)
  if(has_reTrms(modfr)) {
    control <- glmerControl(check.response.not.const = "ignore")
    modfr_ext <- modfr
    modfr_ext$control <- control
    devfun <- do.call(mkGlmerDevfun, modfr_ext)
    if(!devFunOnly){
      opt <- optimizeGlmer(devfun, verbose = verbose)
    }
    devfun <- updateGlmerDevfun(devfun, modfr$reTrms, nAGQ = nAGQ)
    if(devFunOnly) {
      return(devfun)
    } else {
      opt <- optimizeGlmer(devfun, stage=2, verbose = verbose)
      return(mkMerMod(environment(devfun), opt, modfr$reTrms, fr = modfr$fr))
    }
  } else {
    stop("haven't yet implemented no random effects case")
  }
}
