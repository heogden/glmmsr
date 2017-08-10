<!-- README.md is generated from README.Rmd. Please edit that file -->
`glmmsr`: fit GLMMs with various approximation methods
------------------------------------------------------

Generalized linear mixed models (GLMMs) are an important and widely-used model class. In R, we can fit these models with the [`lme4`](https://github.com/lme4/lme4) package, but there are some limitations. First, except in very simple cases, `lme4` uses a Laplace approximation to the likelihood for inference, which may be of poor quality in some cases. Second, it is difficult to fit some GLMMs, such as pairwise comparison models, with `lme4`. The `glmmsr` package offers progress on both of these problems.

A user must choose which method to use to approximate the likelihood. In addition to the Laplace and adaptive Gaussian quadrature approximations, which are borrowed from `lme4`, the likelihood may be approximated by the sequential reduction approximation, or an importance sampling approximation. These methods provide an accurate approximation to the likelihood in some situations where it is not possible to use adaptive Gaussian quadrature.

The vignette provides more information about the different approximations.

The interface of `glmmsr` allows easy fitting of pairwise comparison and many other interesting models, which are difficult to fit with `lme4`. See the vignette for some examples.

Installing `glmmsr`
-------------------

You can `glmmsr` from CRAN with

``` r
install.packages("glmmsr")
```

You can install the development version of `glmmsr` from GitHub by running

``` r
devtools::install_github("heogden/glmmsr")
```

Documentation
-------------

To view the vignette for `glmmsr`, use

``` r
browseVignettes("glmmsr")
```

or see [here](https://CRAN.R-project.org/package=glmmsr/vignettes/glmmsr-vignette.pdf)
