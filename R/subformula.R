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
    as.pairlist(lapply(x, drop_index))
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

find_subvar <- function(subform) {
  tf <- terms(subform)
  lhs <- tf[[2]]
  subvar <- drop_index(lhs)
  if(!is.name(subvar)) {
    stop("Should have only a single variable on LHS of subformula",
         call. = FALSE)
  }
  as.character(subvar)
}

# find the vars (from vars), involved in x
find_vars <- function(x, vars) {
  if(is.atomic(x)) {
    NULL
  } else if (is.name(x)) {
    if(is.element(as.character(x), vars)){
      as.character(x)
    } else {
      NULL
    }
  } else if (is.call(x) || is.pairlist(x)) {
      unique(unlist(lapply(x, find_vars, vars = vars)))
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

add_subexpr <- function(subform, subexprs, which_subvars) {
  subvar <- find_subvar(subform)
  # find those subexpr involving (only) subvar
  which_subexpr <- which(vapply(which_subvars, function(x) {x[1] == subvar}, TRUE))
  if(length(which_subexpr) == 0L){
    warning(paste0("No subexpressions involving \'", subvar, "\'"), call. = FALSE)
    return(NULL)
  } else if(length(which_subexpr) > 1L) {
    stop(paste0("Multiple subexpressions involving ", subvar))
  }
  subexpr <- subexprs[[which_subexpr]]
  return(list(subvar = subvar, subform = subform, subexpr = subexpr))
}

match_subform_subexpr <- function(subforms, subexprs, data) {
  subvars <- vapply(subforms, find_subvar, "test")

  # find the names of the subvars involved in each subexpr
  which_subvars <- lapply(subexprs, find_vars, vars = subvars)
  # check how many subvars involved in each subexpr
  n_subvars <- vapply(which_subvars, length, 1L)
  if(any(n_subvars) > 1L) {
    stop("Each Sub(.) should only involve a single substituted variable",
      .call = FALSE)
  } else if (any(n_subvars) == 0L) {
    stop("Each Sub(.) should involve a substituted variable", .call = FALSE)
  }
  lapply(subforms, add_subexpr, subexprs = subexprs, which_subvars = which_subvars)
}

parse_sub <- function(sub, data, family, subset, weights, na.action,
                       offset, contrasts, mustart, etastart, control)
{
  subvar <- sub$subvar
  subform <- sub$subform
  subexpr <- sub$subexpr

  # find the indices used to index subvar in subexpr

  # find the indices used to index subvar in subform





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
    subforms <- list(...)
    subs <- match_subform_subexpr(subforms, subexprs, data)
    modfr_list <- lapply(subs, parse_sub, data = data, family = family,
                         subset = subset, weights = weights,
                         na.action = na.action, offset = offset,
                         contrasts = constrasts, mustart = mustart,
                         etastart = etastart, control = control)
    return(combine_modfr(c(modfr_no_sub, modfr_list)))
  }
}

