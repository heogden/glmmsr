# fit the three-level model using lme4
# we can use the Laplace approximation
mod_Laplace <- lme4::glmer(response ~ covariate + (1 | cluster) + (1 | group),
                           data = three_level, family = binomial)
mod_Laplace

# if we try to increase the number of adaptive Gaussian quadrature points,
# we get an error message
mod_10 <- lme4::glmer(response ~ covariate + (1 | cluster) + (1 | group),
                      data = three_level, family = binomial, nAGQ = 10)


# we can fit the same model using the sequential reduction approximation
mod_SR <- glmerSR(response ~ covariate + (1 | cluster) + (1 | group),
                  data = three_level, family = binomial, nAGQ = 10,
                  k = 3, verbose = 1)
mod_SR

# the estimates of the random effects standard deviations
# are larger than those using the Laplace approximation
