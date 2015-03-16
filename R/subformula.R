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

#drop all indexing from an expression
drop_index <- function(x) {
  if(is.atomic(x) || is.name(x)){
    x
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`))) {
      drop_index(x[[2]])
    } else {
      as.call(lapply(x, drop_index))
    }
  } else if (is.pairlist(x)) {
    as.pairlist(lapply(x, modify_sub, data = data))
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

find_subvar <- function(subform) {
  tf <- terms(subform)
  lhs <- tf[[2]]
  subvar <- drop_index(lhs)
  if(!is.name(subvar)) {
    stop("Should have only a single variable on LHS of subformula")
  }
  name(subform) <- subvar
  subform
}

extract_subform <- function(...) {
  subformulas <- list(...)
  lapply(subformulas, find_subvar)
}

add_subexpr <- function(subform, subexpr_list) {
  subvar <- name(subform)
  # find those subexpr involving (only) subvar
  which_subexpr <- which(vapply(which_subvars, all.equal, TRUE, y = subvar))
  if(length(which_subexpr) == 0L){
    warning(paste0("No subexpressions involving ", subvar))
    return(NULL)
  } else if(length(which_subexpr) > 1L) {
    stop(paste0("Multiple subexpressions involving ", subvar))
  }
  subexpr <- subexpr_list[which_subexpr]
  return(list(subvar = subvar, subexpr = subexpr))
}

match_subform_subexpr <- function(subform_list, subexpr_list, data) {
  res <- subform_list
  subvars <- names(subform_list)
  # find the names of the subvars involved in each subexpr
  which_subvars <- lapply(subexpr_list, find_which_subvars, subvars = subvars,
                          data = data)
  # check how many subvars involved in each subexpr
  n_subvars <- vapply(which_subvars, length, 1L)
  if(any(n_subvars) > 1L) {
    stop("Each Sub(.) should only involve a single substituted variable")
  } else if (any(n_subvars) == 0L) {
    stop("Each Sub(.) should involve a substituted variable"))
  }
  lapply(subform_list, add_subexpr, subexpr_list = subexpr_list)
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
  if(length(subexprs) == 0L) {
    return(modfr_no_sub)
  } else{
    modfr_list <- lapply(subexprs, parse_subexpr, data = data,
                         family = family, subset = subset, weights = weights,
                         na.action = na.action, offset = offset,
                         contrasts = contrasts, mustart = mustart,
                         etastart = etastart, control = control, ...)
    return(combine_modfr(c(modfr_no_sub, modfr_list)))
  }
}

