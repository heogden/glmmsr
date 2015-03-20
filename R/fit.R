#' Fit a GLMM
#'
#' @param data an optional data frame or environment containing the variables
#'  named in \code{formula}, and in any of the subformulas contained in
#'  \code{...}.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @param ... other arguments. May include subformulas.
#' @inheritParams lme4::glmer
#' @export
glmerSR <- function(formula, data = NULL, family = gaussian, verbose = 0L,
                    nAGQ = 1L, k = 0L, devFunOnly = FALSE, subforms) {
  modfr <- glFormulaSub(formula, data, family, subforms)
  if(has_reTrms(modfr)) {
    devfun <- do.call(mkGlmerDevfun, modfr)
    if(!devFunOnly){
      opt <- optimizeGlmer(devfun)
    }
    devfun <- updateGlmerDevfun(devfun, modfr$reTrms)
    if(devFunOnly) {
      return(devfun)
    } else {
      opt <- optimizeGlmer(devfun, stage=2)
      return(mkMerMod(environment(devfun), opt, modfr$reTrms, fr = modfr$fr))
    }
  } else {
    stop("haven't yet implemented no random effects case")
  }
}
