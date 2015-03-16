find_subexpr <- function(subvar) {
  parse(text = substr(subvar, 5, nchar(subvar) - 1))
}

split_formula <- function(formula) {
  tf <- terms(formula, specials = "Sub")
  var_sub <- attr(tf, "specials")$Sub
  if(length(var_sub) > 0L) {
    # variables numbered starting at LHS.
    # assuming here that there is exactly one LHS variable
    # make this more general
    var_sub_rhs <- var_sub - 1L
    fac <- attr(tf, "factors")
    rhs_vars <- attr(fac, "dimnames")[[2]]
    tf_no_sub <- drop.terms(tf, var_sub_rhs, keep.response = TRUE)
    form_no_sub <- formula(tf_no_sub)
    subexprs <- lapply(rhs_vars[var_sub_rhs], find_subexpr)
  }
  else{
    form_no_sub <- formula
    subexprs <- NULL
  }
  return(list(form_no_sub = form_no_sub, subexprs = subexprs))
}



parse_subexpr <- function(subexpr, data = NULL, family = gaussian,
                               subset, weights, na.action, offset,
                               contrasts = NULL, mustart, etastart,
                               control = glmerControl(), ...)
{

  dots <- list(...)





}


#' Parse a formula (and possibly subformulas)
#'
#' @inheritParams glmerSR
#' @export
glFormulaSub <- function (formula, data = NULL, family = gaussian, subset,
                          weights, na.action, offset, contrasts = NULL, mustart,
                          etastart, control = glmerControl(), ...)
{
  formula_split <- split_formula(formula)
  form_no_sub <- formula_split$form_no_sub
  subexprs <- formula_split$subexprs
  mc <- match.call()
  mc[[1]] <- quote(lme4::glFormula)
  mc$formula <- form_no_sub
  modfr_no_sub <- eval(mc, parent.frame())
  if(length(replace_exprs) == 0L) {
    return(modfr_no_sub)
  } else{
    modfr_list <- lapply(subexprs, parse_subexpr, data = data,
                         family = family, subset = subset, weights = weights,
                         na.action = na.action, offset = offset,
                         contrasts = contrasts, mustart = mustart,
                         etastart = etastart, control = control, ...)



  }

}

