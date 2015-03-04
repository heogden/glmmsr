#' Fit a GLMM
#'
#' @param data an optional data frame or environment containing the variables
#'  named in \code{formula}, and in any of the subformulas contained in
#'  \code{...}.
#' @param k integer scalar - the level of approximation used in the sequential
#'  reduction approximation to the likelihood.
#' @param ... other arguments. May include subformulas.
#' @inheritParams lme4::glmer
#' @export
glmerSR <- function(formula, data = NULL, family = gaussian,
                    control = glmerControl(), start = NULL, verbose = 0L,
                    nAGQ = 1L, k = 0L, subset, weights, na.action, offset,
                    contrasts = NULL, mustart, etastart,
                    devFunOnly = FALSE, ...) {

}
