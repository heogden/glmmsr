has_re <- function(formula) {
  length(lme4::findbars(formula[[length(formula)]])) > 0L
}
