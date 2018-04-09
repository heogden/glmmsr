#' Control of Mixed Model Fitting
#'
#' A version of \code{glmerControl} from \code{lme4}, with different defaults.
#'
#' @param ... other arguments to \code{glmerControl}
#' @inheritParams lme4::glmerControl
#' @export
set_lme4_control <- function(check.nobs.vs.rankZ = "ignore",
                             check.nobs.vs.nlev = "ignore",
                             check.nlev.gtreq.5 = "ignore",
                             check.nlev.gtr.1 = "ignore",
                             check.nobs.vs.nRE = "ignore",
                             check.rankX = c("message+drop.cols","silent.drop.cols",
                                             "warn+drop.cols", "stop.deficient",
                                             "ignore"),
                             check.scaleX  = "warning",
                             check.formula.LHS = "stop",
                             check.response.not.const = "ignore",
                             ...)
{
  lme4::glmerControl(check.nobs.vs.rankZ = check.nobs.vs.rankZ,
                     check.nobs.vs.nlev = check.nobs.vs.nlev,
                     check.nlev.gtreq.5 = check.nlev.gtreq.5,
                     check.nlev.gtr.1 = check.nlev.gtr.1,
                     check.nobs.vs.nRE = check.nobs.vs.nRE,
                     check.rankX = check.rankX,
                     check.scaleX  = check.scaleX,
                     check.formula.LHS = check.formula.LHS,
                     check.response.not.const = check.response.not.const,
                     ...)
}

# code modified from use of control in optim
find_control_with_defaults <- function(control, method)
{
  if( length(method) == 0 )
    stop("You must specify which method to use for likelihood approximation", call. = FALSE)

  conLaplace <- list(order = 1, check_Laplace = FALSE, divergence_threshold = 0.1)
  conAGQ <- list(nAGQ = 15)
  conSR <- list(nSL = 3)
  conIS <- list(nIS = 1000)
  con_tot <- c(conLaplace, conAGQ, conSR, conIS)

  con <- switch(method,
                "Laplace" = conLaplace,
                "AGQ" = conAGQ,
                "SR" = conSR,
                "IS" = conIS,
                stop(paste("The method", method, "is not recognised"), call. = FALSE))

  are_known <- names(control) %in% names(con_tot)
  names_known <- names(control)[are_known]
  names_unknown <- names(control)[!are_known]

  are_needed <- names(control[are_known]) %in% names(con)
  names_not_needed <- names_known[!are_needed]

  if ( length(names_unknown) > 0 )
    warning("unknown names in control: ", paste(names_unknown, collapse = ", "),
            call. = FALSE)

  if ( length(names_not_needed) > 0 )
    warning("For method = ", method, "parts of control were ignored: ",
            paste(names_not_needed, collapse = ", "), call. = FALSE)

  con[names(control)] <- control
  if(method == "SR") {
    con$nAGQ <- 2^(con$nSL + 1) - 1
  }
  if(method != "Laplace") {
    con$check_Laplace <- FALSE
  } else {
    if(con$order > 1) {
      con$check_Laplace <- FALSE
    }
  }

  con
}
