# fit the two-level model using lme4
# we can use the Laplace approximation
mod_Laplace <- lme4::glmer(response ~ covariate + (1 | cluster),
                           data = two_level, family = binomial)
mod_Laplace

# or increase the number of adaptive Gaussian quadrature points
mod_10 <- lme4::glmer(response ~ covariate + (1 | cluster),
                      data = two_level, family = binomial, nAGQ = 10)
mod_10

# we can fit the same model using the sequential reduciton approximation
mod_10_SR <- glmerSR(response ~ covariate + (1 | cluster),
                     data = two_level, family = binomial, nAGQ = 10, k = 1)
mod_10_SR
