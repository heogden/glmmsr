# convert formula to a model frame, using glFormula if there are random
# effects, and manually, using model.matrix, if not
parse_formula <- function(formula, data, family) {
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if(has_re(formula)) {
    modfr <- lme4::glFormula(formula, data = data, family = family)
  } else {
    fr <- glm(formula, data = data, family = family, method = "model.frame")
    X <- model.matrix(formula, data = fr)
    modfr <- list(fr = fr, X = X, family = family, formula = formula)
  }
  modfr
}

# convert formula to a model frame, using lFormula if there are random
# effects, and manually, using model.matrix, if not
parse_subformula <- function(formula, data) {
  if(has_re(formula)) {
    modfr_tot <- lme4::lFormula(formula, data = data)
    modfr <- list(X = modfr_tot$X, reTrms = modfr_tot$reTrms)
  } else {
    fr <- lm(formula, data = data, family = family, method = "model.frame")
    X <- model.matrix(formula, data = fr)
    modfr <- list(X = X)
  }
  modfr
}


has_reTrms <- function(modfr) {
  "reTrms" %in% names(modfr)
}


`[fr` <- function(modfr, i) {
  out <- modfr
  out$X <- modfr$X[i, , drop = FALSE]
  if(has_reTrms(modfr)) {
    reTrms <- modfr$reTrms
    out$reTrms$Zt <- reTrms$Zt[, i, drop = FALSE]
    out$reTrms$Ztlist <- lapply(reTrms$Ztlist, "[", j = i, drop = FALSE)
    out$reTrms$flist <- reTrms$flist[i, , drop = FALSE]
  }
  return(out)
}


