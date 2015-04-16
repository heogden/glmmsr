#' Make the deviance function
#' @inheritParams lme4::mkGlmerDevfun
#' @inheritParams glmerSR
#' @export
mkGlmerDevfunSR <- function(fr, X, reTrms, family, nAGQ = 1L, k = 0L,
                            verbose = 0L, control = glmmsrControl(), ...)
{
  if(k == 0L) {
    devfun <- mkGlmerDevfun(fr, X, reTrms, family, nAGQ, verbose, control, ...)
    devfun <- updateGlmerDevfun(devfun, reTrms, nAGQ)
  } else {
    modfr <- list(fr = fr, X = X, reTrms = reTrms, family = family)
    lmodfr <- split_modfr(modfr)
  }
  return(devfun)
}
