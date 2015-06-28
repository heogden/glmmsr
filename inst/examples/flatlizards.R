library(BradleyTerry2)
result <- rep(1, nrow(flatlizards$contests))

loser <- flatlizards$contests$loser
winner <- flatlizards$contests$winner

# The throat spectrum is missing for lizards 96 and 99.
# If we match the default behaviour of `BradleyTerry2`, adding a separate
# predictor for each lizard with missing values in the covariates
# of interest, lme4 fails to converge. So instead, we set these missing
# values to zero, although this is not really valid.

flatlizardspred <- as.data.frame(flatlizards$predictors)
flatlizardspred[is.na(flatlizardspred$throat.PC3), ] <- 0

lizards <- c(list(result = result, winner = winner, loser = loser), flatlizardspred)
lizards_BT <- list(contests = flatlizards$contests, predictors = flatlizardspred)

lizards_mod <- glmerSR(result ~ 0 + Sub(ability[winner] - ability[loser]),
               ability[liz] ~ 0 + throat.PC1[liz] +
                      throat.PC3[liz] + head.length[liz] + SVL[liz] + (1 | liz),
               data = lizards, family = binomial(link = "probit"))
summary(lizards_mod)


lizardsmod_BTm <- BTm(y, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                      head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"), data = lizards_BT)
summary(mod_BTm)


