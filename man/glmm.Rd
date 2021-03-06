% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{glmm}
\alias{glmm}
\title{Fit a GLMM}
\usage{
glmm(formula, subformula = NULL, data = NULL, family = gaussian,
  method = NULL, control = list(), weights = NULL, offset = NULL,
  prev_fit = NULL, verbose = 1L, lme4_control = set_lme4_control())
}
\arguments{
\item{formula}{a two-sided linear formula object describing both the
    fixed-effects and random-effects part of the model, with the response
    on the left of a \code{~} operator and the terms, separated by
    \code{+} operators, on the right.  Random-effects terms are
    distinguished by vertical bars (\code{"|"}) separating expressions
    for design matrices from grouping factors.}

\item{subformula}{a subformula, describing how a substituted variable
depends on covariates, or a list of subformulas, if there is more
than one \code{Sub()} term in \code{formula}.}

\item{data}{an optional data frame, list or environment containing the
variables named in \code{formula}, and in any of the subformulas.}

\item{family}{a GLM family, see \code{\link[stats]{glm}} and
    \code{\link[stats]{family}}.}

\item{method}{the method used to approximate the likelihood. The options
are \code{"Laplace"}, \code{"AGQ"} (the adaptive Gaussian quadrature approximation,
from \code{lme4}), \code{"SR"} (the sequential reduction approximation)
and \code{"IS"} (an importance sampling approximation).}

\item{control}{a list of extra parameters controlling the approximation
to the likelihood. See 'Details' for more information.}

\item{weights}{an optional vector of \sQuote{prior weights} to be used
      in the fitting process.  Should be \code{NULL} or a numeric
      vector.}

\item{offset}{this can be used to specify an \emph{a priori} known
    component to be included in the linear predictor during
    fitting. This should be \code{NULL} or a numeric vector of length
    equal to the number of cases.  One or more \code{\link{offset}}
    terms can be included in the formula instead or as well, and if more
    than one is specified their sum is used.  See \code{\link{model.offset}}.}

\item{prev_fit}{a \code{glmmFit} object, the result of a previous model fit.}

\item{verbose}{controls how much detail to print out while fitting the model.
For verbose = 0, print nothing. For verbose = 1 (the default), print
output approximately once a second during model fitting. For verbose = 2,
print out the parameter value and log-likelihood at every stage of
optimization.}

\item{lme4_control}{the result of a call to \code{lme4_control}, containing
control parameters passed to \code{lme4}. See \code{?lme4_control}.}
}
\value{
An object of the class \code{glmmFit}
}
\description{
Fit a GLMM
}
\details{
The \code{control} argument is a list, used to specify further
 arguments controlling the approximation to the likelihood:
 \describe{
  \item{\code{nAGQ}}{the number of adaptive Gaussian quadrature points.
  Only used if \code{method = "AGQ"}. Defaults to 15.}
  \item{\code{nSL}}{the level of sparse grid storage.
  Only used if \code{method = "SR"}. Defaults to 3.}
  \item{\code{nIS}}{the number of samples to use for importance sampling.
  Only used if \code{method = "IS"}. Defaults to 1000.}
  \item{\code{order}}{the order of Laplace approxiation.
  only used if \code{method = "Laplace"}. Defaults to 1.}
  \item{\code{check_Laplace}}{should quality of first-order Laplace
  approximation be checked? Only used  if \code{method = "Laplace"}
  and \code{order = 1}. Defaults to TRUE.}
  \item{\code{divergence_threshold}}{if \code{check_Laplace = TRUE},
  warn about quality of inference using the first-order Laplace
  approximation if measure of divergence from inference with
  second-order Laplace approximation exceeds \code{divergence_threshold}.
  Defaults to 0.1.}
 }
}
\examples{
# Fit a three-level model with the Laplace approximation to the likelihood
(mod_Laplace <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                     data = three_level, family = binomial,
                     method = "Laplace"))

# if we try to fit with adaptive Gaussian quadrature, we get an error
\dontrun{
  (mod_AGQ <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                   data = three_level, family = binomial, method = "AGQ",
                   control = list(nAGQ = 15)))
}

# We can fit with the Sequential Reduction approximation
\dontrun{
  (mod_SR <- glmm(response ~ covariate + (1 | cluster) + (1 | group),
                  data = three_level, family = binomial, method = "SR",
                  control = list(nSL = 3)))
}
# the estimates of the random effects standard deviations
# are larger than those using the Laplace approximation
}
