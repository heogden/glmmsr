has_re <- function(formula) {
  length(lme4::findbars(formula[[length(formula)]])) > 0L
}


# convert formula to a model frame, using glFormula if there are random
# effects, and glm (with method "model.frame") if not.
parse_formula <- function(formula, data, family) {
  if(has_re(formula)) {
  modfr <- lme4::glFormula(formula, data = data, family = family)
  } else {
  modfr <- glm(formula, data = data, family = family, method = "model.frame")
  }
  modfr
}

