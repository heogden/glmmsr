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

find_subvar <- function(subform, char = TRUE) {
  tf <- terms(subform)
  lhs <- tf[[2]]
  subvar <- drop_index(lhs)
  if(!is.name(subvar)) {
    stop("Should have only a single variable on LHS of subformula",
         call. = FALSE)
  }
  if(char) {
    res <- as.character(subvar)
  } else{
    res <- subvar
  }
  res
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

find_dim_sub <- function(x, var, d = NULL){
  if(is.atomic(x) || is.name(x)) {
    d
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && identical(as.character(x[[2]]), var)) {
      if(length(d) > 0L){
        if(d != (length(x) - 2)) {
          stop(paste0("\'", var, "\' is indexed inconsistently"), call. = FALSE)
        }
      } else{
        d <- length(x) - 2
      }
    }
    for(i in seq_along(x)) {
      d <- find_dim_sub(x[[i]], var, d)
    }
  } else if(is.pairlist(x)) {
    for(i in seq_along(x)) {
      d <- find_dim_sub(x[[i]], var, d)
    }
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
  d
}

find_indices <- function(x, i, var) {
  if(is.atomic(x) || is.name(x)) {
    NULL
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && identical(as.character(x[[2]]), var)) {
      return(as.character(x[[i+2]]))
    }
    unique(unlist(lapply(x, find_indices, i = i, var = var)))
  } else if(is.pairlist(x)) {
    unique(unlist(lapply(x, find_indices, i = i, var = var)))
  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

matrix_indexing <- function(x, indices) {
  if(is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    if(identical(x[[1]], quote(`[`)) && length(x) > 3) {
      y <- x[1:2]
      #indices[, as.list(x[-c(1,2)]), drop = FALSE]
      y[[3]] <- as.call(c(list(quote(cbind)), as.list(x[-c(1,2)])))
      return(y)
    } else{
      as.call(lapply(x, matrix_indexing, indices = indices))
    }
  } else if(ispairlist(x)) {
    as.pairlist(lapply(x, matrix_indexing, indices = indices))

  } else{
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

flatten_formula <- function(formula, indices, data){
  # drop indexing in LHS of formula
  lhs_var <- find_subvar(formula, char = FALSE)
  tf <- terms(formula)
  tf[[2]] <- lhs_var
  formula_flat <- formula(tf)

  # each time we see `[`, extract relevant columns of index_matrix
  # and combine with cbind
  # for now, insist all indexing is done with indices in indices

  formula_flat <- matrix_indexing(formula_flat, indices)



}

find_indices_subform <- function(sub, data) {
  subvar <- sub$subvar
  subform <- sub$subform
  subexpr <- sub$subexpr

  d <- find_dim_sub(subexpr, subvar)

  # check that we get same dimension from subform
  if(find_dim_sub(subform, subvar) != d) {
    stop(paste0("\'", subvar, "\' is indexed inconsistently"), call. = FALSE)
  }

  # find the indices used to index subvar in subexpr
  indices_subexpr <- list()
  indices_subform <- list()
  for(i in 1:d){
    indices_subexpr[[i]] <- find_indices(subexpr, i, subvar)
    # check all subexpr indices are in data
    in_data <- vapply(indices_subexpr[[i]], exists, TRUE, where = data)
    if(any(!in_data)) {
      stop(paste0("Can't find indexing variables: ",
                  indices_subexpr[[i]][!in_data]))
    }
    indices_i <- mget(indices_subexpr[[i]], as.environment(data))
    # coerce to factor
    indices_i <- lapply(indices_i, as.factor)
    levels_indices_i <- sort(unique(Reduce(c, lapply(indices_i, levels))))
    # coerce to factor with common levels
    indices_i <- lapply(indices_i, factor, levels = levels_indices_i)
    L_i <- length(levels_indices_i)
    indices_subform[[i]] <- as.numeric(factor(levels_indices_i))
    names(indices_subform)[i] <- find_indices(subform, i, subvar)

  }
  return(indices_subform)
}

parse_sub <- function(sub, data, family, subset, weights, na.action,
                       offset, contrasts, mustart, etastart, control)
{
  indices_subform <- find_indices_subform(sub)
  d <- length(indices_subform)
  if(d > 1) {
    #have array indexing: should flatten
    L <- vapply(indices_subform, length, 1L)
    indices_flat <- as.list(expand.grid(indices_subform))
    subform_flat <- flatten_formula(subformula, indices_flat, data)
  }







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

