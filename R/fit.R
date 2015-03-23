#' Fit a GLMM
#'
#' @param data an optional data frame or environment containing the variables
#'  named in \code{formula}, and in any of the subformulas contained in
#'  subforms.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @param subforms an (optional) list of subformulas
#' @inheritParams lme4::glmer
#' @export
glmerSR <- function(formula, data = NULL, family = gaussian, verbose = 0L,
                    nAGQ = 1L, k = 0L, devFunOnly = FALSE, subforms) {
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
