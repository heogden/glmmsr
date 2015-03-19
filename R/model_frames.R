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

has_reTrms <- function(modfr) {
  "reTrms" %in% names(modfr)
}


extract_X <- function(modfr) {
  if(has_reTrms(modfr)) {

  }

}

`[fr` <- function(x, i) {


  # detect which one

}
