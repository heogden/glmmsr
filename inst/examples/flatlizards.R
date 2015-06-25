library(BradleyTerry2)
y <- rep(1, nrow(flatlizards$contests))

loser <- flatlizards$contests$loser
winner <- flatlizards$contests$winner

# The throat spectrum is missing for lizards 96 and 99.
# Since `glmmsr` can't yet handle missing values, we manually match
# the default behaviour of `BradleyTerry2`, adding a separate
# predictor for each lizard with missing values in the covariates
# of interest.

flatlizardspred <- as.data.frame(flatlizards$predictors)
flatlizardspred[is.na(flatlizardspred$throat.PC3), ] <- 0
liz96 <- rep(0, length(flatlizards$predictors$id))
liz96[flatlizards$predictors$id == 96] <- 1
liz99 <- rep(0, length(flatlizards$predictors$id))
liz99[flatlizards$predictors$id == 99] <- 1

lizdat <- c(list(y = y, winner = winner, loser = loser, liz96 = liz96,
liz99 = liz99), flatlizardspred)


mod <- glmerSR(y ~ 0 + Sub(ability[winner] - ability[loser]),
               ability[liz] ~ 0 + liz96[liz] + liz99[liz] + throat.PC1[liz] +
                      throat.PC3[liz] + head.length[liz] + SVL[liz] + (1 | liz),
               data = lizdat, family = binomial(link = "probit"))

summary(mod)

# compare with the same model fit with BradleyTerry2

mod_BTm <- BTm(y, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
              head.length[..] + SVL[..] + (1|..),
              family = binomial(link = "probit"), data = flatlizards)
summary(mod_BTm)

# create and split model frame
modfr <- glFormulaSub(y ~ 0 + Sub(ability[winner] - ability[loser]),
                      ability[liz] ~ 0 + liz96[liz] + liz99[liz] + throat.PC1[liz] +
                        throat.PC3[liz] + head.length[liz] + SVL[liz] + (1 | liz),
                      data = lizdat, family = binomial(link = "probit"))
lmodfr <- split_modfr(modfr)
q <- nrow(modfr$reTrms$Zt)
#save_lmodfrs(lmodfr, q, file = "lmodfr_lizards.txt")

# extract normal approximation
devfun_lme4 <- lme4::mkGlmerDevfun(modfr$fr, modfr$X, modfr$reTrms, modfr$family,
                    control = glmerControl(check.response.not.const = "ignore"))
devfun_lme4 <- updateGlmerDevfun(devfun_lme4, modfr$reTrms)

theta <- 0.5
beta <- rep(0, 6)
-devfun_lme4(c(theta, beta))/2

PR <- get("pp", environment(devfun_lme4))
L_tot <- expand(PR$L())
L <- L_tot$L
P <- L_tot$P
t(P)%*%L%*%t(L)%*%P

Sigma_inv <- t(P)%*%tcrossprod(L)%*%P
mu <- PR$delu

#save_normal(mu, Sigma_inv, file = "normal_lizards.txt")

Sigma <- solve(Sigma_inv)
