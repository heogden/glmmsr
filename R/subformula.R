extractSubform <- function(v, n_rhs_vars, tf) {
  drop <- (1:n_rhs_vars)[-v]
  tf_v <- drop.terms(tf, drop, keep.response = TRUE)
  formula(tf_v)
}


splitFormula <- function(formula) {
  tf <- terms(formula, specials = "Sub")
  var_sub <- attr(tf, "specials")$Sub
  if(length(var_sub) > 0L) {
    # variables numbered starting at LHS.
    # we are assuming here that there is exactly one LHS variable
    # make this more general
    var_sub_rhs <- var_sub - 1L
    fac <- attr(tf, "factors")
    rhs_vars <- attr(fac, "dimnames")[[2]]
    n_rhs_vars <- length(rhs_vars)
    tf_no_sub <- drop.terms(tf, var_sub_rhs, keep.response = TRUE)
    form_no_sub <- formula(tf_no_sub)
    subforms <- lapply(var_sub_rhs, extractSubform,
                       n_rhs_vars = n_rhs_vars, tf = tf)
  }
  else{
    form_no_sub <- formula
    subforms <- NULL
  }
  return(list(form_no_sub = form_no_sub, subforms = subforms))
}


#' Parse a formula (and possibly subformulas)
#'
#' @inheritParams glmerSR
#' @export
glFormulaSub <- function (formula, data = NULL, family = gaussian, subset,
                          weights, na.action, offset, contrasts = NULL, mustart,
                          etastart, control = glmerControl(), ...)
{
  formula_split <- splitFormula(formula)
  form_no_sub <- formula_split$form_no_sub
  subforms <- formula_split$subforms
  mc <- match.call()
  mc[[1]] <- quote(lme4::glFormula)
  mc$formula <- form_no_sub
  modfr_no_sub <- eval(mc, parent.frame())
  if(length(subforms) == 0L) {
    return(modfr_no_sub)
  } else{
    lapply(subforms, parseSF, family, gauss)



  }

}


parseSF <- function(subform, data = NULL, family = gaussian, subset,
                    weights, na.action, offset, contrasts = NULL, mustart,
                    etastart, control = glmerControl(), ...)
{

  # check LHS of subform: is it already contained in data?


}

