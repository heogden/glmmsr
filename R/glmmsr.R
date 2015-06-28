#' glmmsr: fit GLMMs with the sequential reduction approximation
#'
#' The glmmsr package provides functions to do inference on generalized linear
#' mixed models, using the sequential reduction approximation to the likelihood.
#' The interface and output are designed to look similar to lme4.
#'
#' The \code{lme4} package uses the Laplace approximation to the likelihood,
#' except in the special case of two-level models, where adapative Gaussian
#' quadrature may be used. In some situations, this Laplace approximation may be
#' poor quality, and the sequential reduction approximation may be used to find
#' an accurate approximation to the likelihood.
#'
#' The main function of the glmmsr package is \code{\link{glmerSR}}, which is
#' used to fit the GLMM. Its interface is a generalization of
#' \code{\link[lme4]{glmer}}, to allow a larger class of models, including
#' structured pairwise comparison models.
#'
#' @references Helen E. Ogden (2015). A sequential reduction method for
#'   inference in generalized linear mixed models. Electronic Journal of
#'   Statistics 9: 135-152. doi:
#'   \href{http://dx.doi.org/10.1214/15-EJS991}{10.1214/15-EJS991}
#' @docType package
#' @import lme4
#' @name glmmsr
NULL

#' A dataset simulated from a two-level model
#'
"two_level"

#' A dataset simulated from a three-level model
#'
"three_level"
