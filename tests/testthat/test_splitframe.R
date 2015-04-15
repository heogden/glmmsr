library(glmmsr)
context("Split up model frame")

set.seed(1)
y <- rbinom(10, 1, 0.5)
x <- rbinom(10, 1, 0.5)
cluster <- rep(1:5, each = 2)
data = list(y = y, x = x, cluster = cluster)
modfr <- parse_formula(y ~ x + (1 | cluster), data = data,
                       family = binomial, off = NULL)

test_that("find posterior dependence graph correctly", {
  act <- find_active(modfr)
  q <- nrow(modfr$reTrms$Zt)
  G <- find_pdg(act, q)
  expect_equal_to_reference(G, "pdg_two_level.rds")
})

test_that("reordering model frame keeps devfun unchanged", {
  devfun <- lme4::mkGlmerDevfun(modfr$fr, modfr$X, modfr$reTrms, modfr$family)
  elim_order <- c(2:5, 1)
  modfr2 <- reorder_modfr(modfr, elim_order)
  devfun2 <- lme4::mkGlmerDevfun(modfr2$fr, modfr2$X, modfr2$reTrms,
                                 modfr2$family)
  expect_equal(devfun(1), devfun2(1))
})
