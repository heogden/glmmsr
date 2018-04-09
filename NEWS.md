# glmmsr 0.2.2
* remove deprecated use of `rBind`

# glmmsr 0.2.1
* add `coef` and `vcov` methods for a fitted glmmFit object
* fix compatibility with `lme4` 1.1-14 (#5)

# glmmsr 0.2.0
* fix to avoid adding attribute on a symbol (#4)
* bound the size of random effects variance parameters to be less than
  5, to avoid poor-quality likelihood approximations for very large 
  values of these variance parameters.
* check of quality of first-order Laplace approximation, with
  `control = list(checkLaplace = TRUE)`
* allow second-order Laplace approximation, with 
  `method = "Laplace", control = list(order = 2)`
* give error for `family = gaussian`, which is not yet handled correctly (#3)

# glmmsr 0.1.1
* Make C++ code more portable, by removing ambiguous `pow(int, int)` 
  and `sqrt(int)` (#2)
* Prevent segfaults when compiled with clang (#1)

# glmmsr 0.1.0
* Initial version
