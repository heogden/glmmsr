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
    mod_control <- lme4::lmerControl(check.nobs.vs.nlev = "ignore",
                                     check.nobs.vs.nRE = "ignore")
    modfr_tot <- lme4::lFormula(formula, data = data, control = mod_control)
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

is_modfr <- function(x) {
  is.list(x) && ("X" %in% names(x))
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


# multiply a model frame by a constant
`*fr` <- function(modfr, a) {
  if(!is_modfr(modfr) || !is.numeric(a)) {
    if(is_modfr(a) && is.numeric(modfr)) {
      return(`*fr`(a, modfr))
    } else {
      stop("Invalid use of `*fr`", call. = FALSE)
    }
  }
  out <- modfr
  out$X <- modfr$X * a
  if(has_reTrms(modfr)) {
    out$reTrms$Zt <- modfr$reTrms$Zt * a
    out$reTrms$Ztlist <- lapply(modfr$reTrms$Ztlist, `*`, e2 = a)
  }
  return(out)
}

# divide a model frame by a constant
`/fr` <- function(modfr, a) {
  if(!is_modfr(modfr) || !is.numeric(a)) {
    stop("Invalid use of `/fr`", call. = FALSE)
  }
  `*fr`(modfr, 1/a)
}

# add two model frames
`+fr` <- function(modfr1, modfr2) {
  if(!is_modfr(modfr1) || !is_modfr(modfr2)) {
    stop("Invalid use of `+fr`: both arguments should be model frames",
         call. = FALSE)
  }
  if(has_reTrms(modfr1) != has_reTrms(modfr2)) {
    stop("Invalid use of `+fr`: Cannot add model frame with reTrms
         to model frame without",
         call. = FALSE)
  }
  out <- modfr1
  if(any(dim(modfr1$X) != dim(modfr2$X))) {
    stop("Invalid use of `+fr`: Dimension mismatch in model matrices",
         call. = FALSE)
  }
  out$X <- modfr1$X + modfr2$X
  if(has_reTrms(modfr1)) {
    if(any(dim(modfr1$reTrms$Zt) != dim(modfr2$reTrms$Zt))) {
      stop("Invalid use of `+fr`: Dimension mismatch in model matrices",
           call. = FALSE)
    }
    out$reTrms$Zt <- modfr1$reTrms$Zt + modfr2$reTrms$Zt
    out$reTrms$Ztlist <- mapply("+", modfr1$reTrms$Ztlist, modfr2$reTrms$Ztlist)
    # what about flist?
  }
  return(out)
}

# subtract one model frame from another
`-fr` <- function(modfr1, modfr2) {
  if(!is_modfr(modfr1) || !is_modfr(modfr2)) {
    stop("Invalid use of `-fr`: both arguments should be model frames",
         call. = FALSE)
  }
  minus_modfr2 <- `*fr`(modfr2, -1L)
  `+fr`(modfr1, minus_modfr2)
}

