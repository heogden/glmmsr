#' Parse a formula (and possibly subformulas)
#'
#' @inheritParams glmerSR
#' @export
glFormulaSub <- function (formula, data = NULL, family = gaussian, subset,
                          weights, na.action, offset, contrasts = NULL, mustart,
                          etastart, control = glmerControl(), ...)
{
  #detect whether formula contains subformulas
  tf <- terms(formula, specials = "Sub")
  stf <- attr(tf, "specials")
  if(length(stf$Sub) == 0L) {
    #if there are no subformulas, can just use glFormula

    mc <- match.call()
    mc[[1]] <- quote(lme4::glFormula)
    return(eval(mc, parent.frame()))
  } else{
    # there are subformulas

  }

}
