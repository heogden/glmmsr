#' Control of GLMM fitting
#'
#' A version of \code{\link{glmerControl}}, from \code{lme4}, with different
#' defaults.
#'
#' @param optimizer character - name of optimizing function(s). See
#'  \code{\link{glmerControl}} for more details.
#' @inheritParams lme4::glmerControl
#' @export
glmmsrControl <- function(optimizer = c("bobyqa", "Nelder_Mead"),
                          restart_edge = FALSE,
                          boundary.tol = 1e-5,
                          calc.derivs=TRUE,
                          use.last.params=FALSE,
                          sparseX = FALSE,
                          tolPwrss=1e-7,
                          compDev=TRUE,
                          nAGQ0initStep=TRUE,
                          ## input checking options
                          check.nobs.vs.rankZ = "ignore",
                          check.nobs.vs.nlev = "ignore",
                          check.nlev.gtreq.5 = "ignore",
                          check.nlev.gtr.1 = "ignore",
                          check.nobs.vs.nRE="ignore",
                          check.rankX = c("message+drop.cols",
                                          "silent.drop.cols", "warn+drop.cols",
                                          "stop.deficient", "ignore"),
                          check.scaleX  = "warning",
                          check.formula.LHS = "stop",
                          check.response.not.const = "ignore",
                          ## convergence checking options
                          check.conv.grad = .makeCC("warning", tol = 1e-3,
                                                    relTol = NULL),
                          check.conv.singular = .makeCC(action = "ignore",
                                                        tol = 1e-4),
                          check.conv.hess = .makeCC(action = "warning",
                                                    tol = 1e-6),
                          ## optimizer args
                          optCtrl = list())
{
  lme4::glmerControl(optimizer = optimizer,
                     restart_edge = restart_edge,
                     boundary.tol = boundary.tol,
                     use.last.params = use.last.params,
                     sparseX = sparseX,
                     tolPwrss = tolPwrss,
                     compDev = compDev,
                     nAGQ0initStep = nAGQ0initStep,
                     check.nobs.vs.rankZ = check.nobs.vs.rankZ,
                     check.nobs.vs.nlev = check.nobs.vs.nlev,
                     check.nlev.gtreq.5 = check.nlev.gtreq.5,
                     check.nlev.gtr.1 = check.nlev.gtr.1,
                     check.nobs.vs.nRE = check.nobs.vs.nRE,
                     check.rankX = check.rankX,
                     check.scaleX = check.scaleX,
                     check.formula.LHS = check.formula.LHS,
                     check.response.not.const = check.response.not.const,
                     check.conv.grad = check.conv.grad,
                     check.conv.singular = check.conv.singular,
                     check.conv.hess = check.conv.hess,
                     optCtrl = optCtrl)
}
