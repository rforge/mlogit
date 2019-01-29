# mlogit <img src="logo.png" align="right" alt="" width="120" />

## The mlogit package

`mlogit`, first posted on `CRAN` in 2008, was the first `R` package
allowing the estimation of random utility models (**RUM**) and
provides:

- enhanced `data.frame` to deal with data sets for discrete choice models,
- enhanced `formula` to describe the models to be estimated, 
- a versatile function to fit a wide set of **RUM** models,
- specific methods and functions to extract the relevant results,
- specific testing functions and methods for the `waldtest` and the
  `lrtest` of the `lmtest` package.

## Installation

To install the released version on `CRAN`:

`install.packages("mlogit")`

To install the development version on `RForge`:

`install.packages("mlogit", repos = 'http://R-Forge.R-project.org'))`

## Documentation

A gentle introduction of `mlogit` is available as vignettes which are
accessed in the *Articles* entry of the website:

- The first 7 vignettes provide an extensive introduction to `mlogit`,
  surveying the theoritical background and providing examples of
  recent researchs to illustrate the use of `mlogit` functions,
- The last 4 vignettes are corrected exercises concerning the
  estimation of the 4 major **RUM** (the multinomial, nested and mixed
  effects logit models and the multinomial probit model); these
  exercises were written by [Kenneth
  Train](https://eml.berkeley.edu/~train/distant.html) as companions
  to his book *"Discrete Choice Modeling with Simulations"* and have
  been adapted to `R` and `mlogit` by Yves Croissant.

Full documentation of all the exported functions and methods of
`mlogit` can be found in the *Reference* entry of the website.
