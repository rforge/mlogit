\name{scoretest}
\alias{scoretest}
\alias{scoretest.mlogit}
\alias{scoretest.default}
\alias{waldtest.mlogit}
\alias{lrtest.mlogit}
\title{The three tests for mlogit models}
\description{
  Three tests for mlogit models: specific methods for the Wald test and
  the likelihood ration test and a new function for the score test
}
\usage{
scoretest(object, ...)
\method{scoretest}{mlogit}(object, ..., heterosc = FALSE, nests = NULL,
        rpar = NULL, un.nest.el = FALSE, halton = NULL, panel = FALSE,
        correlation = FALSE, R = 40)
\method{waldtest}{mlogit}(object, ..., hyp = NULL)
\method{lrtest}{mlogit}(object, ...)
}
\arguments{
  
  \item{object}{an object of class \code{mlogit} or a formula,}

  \item{hyp}{the hypothesis to be tested in two settings. If
  \code{object} is a correlated random parameter model, \code{hyp}
  should be one of \code{"uncorrelated"} and \code{"fixed"} to test
  respectively the hypothesis of uncorrelated random effects and no
  random effects. If \code{object} is a nested logit model with
  different nest elasticities, \code{hyp} should be one of
  \code{"un.nest.el"} and \code{"no.nests"} to test respectively the
  hypothesis of a unique nest elasticity and no nests.}

  \item{nests}{a named list of characters vectors, each names being a
    nest, the corresponding vector being the set of alternatives that
    belong to this nest,}

  \item{heterosc}{a boolean, if \code{TRUE}, the heteroscedastic logit
    model is estimated,}
  
  \item{rpar}{a named vector whose names are the random parameters and
    values the distribution : \code{'n'} for normal, \code{'l'} for
    log-normal, \code{'t'} for truncated normal, \code{'u' } for uniform,}

  \item{un.nest.el}{a boolean, if \code{TRUE}, the hypothesis of unique
    elasticity is imposed for nested logit models,}

  \item{halton}{only relevant if \code{rpar} is not \code{NULL}, if not
    \code{NULL}, halton sequence is used instead of pseudo-random
    numbers. If \code{halton=NA}, some default values are used for the
    prime of the sequence (actually, the primes are used in order) and for
    the number of elements droped. Otherwise, \code{halton} should be a
    list with elements \code{prime} (the primes used) and \code{drop} (the
    number of elements droped).}
  
  \item{panel}{only relevant if \code{rpar} is not \code{NULL} and if
    the data are repeated observations of the same unit ; if \code{TRUE},
    the mixed-logit model is estimated using panel techniques,}
  
  \item{correlation}{only relevant if \code{rpar} is not \code{NULL}, if
    true, the correlation between random parameters is taken into
    account,}

  \item{R}{the number of function evaluation for the gaussian quadrature
    method used if \code{heterosc=TRUE}, the number of draws of
    pseudo-random numbers if \code{rpar} is not \code{NULL},}
  
  \item{...}{further arguments passed to the testing functions.}
  
}
\value{
  an object of class \code{"htest"}.
}
\details{
  
  The \code{"scoretest"} function and \code{"mlogit"} method for
  \code{"waldtest"} and \code{"lrtest"} from the \code{"lmtest"} package
  provides the infrastructure to compute the three tests of hypothesis
  for \code{"mlogit"} objects.

  The first argument must be a \code{"mlogit"} object. If the second one
  is a fitted model or a formula, the behaviour of the three functions
  is the one of the default methods of \code{"waldtest"} and
  \code{"lrtest"}: the two models provided should be nested and the
  hypothesis tested is that the constrained model is the `right' model.

  If no second model is provided and if the model provided is the
  constrained model, some specific arguments of \code{"mlogit"} should
  be provided to descibe how the initial model should be updated. If the
  first model is the unconstrained model, it is tested versus the
  `natural' constrained model; for example, if the model is a
  heteroscedastic logit model, the constrained one is the multinomial
  logit model. 
  
}

\examples{
library("mlogit")
library("lmtest")
data("TravelMode", package = "AER")
ml <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode")
hl <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode",
             method = "bfgs", heterosc = TRUE)
lrtest(ml, hl)
waldtest(hl)
scoretest(ml, heterosc = TRUE)
}
\author{Yves Croissant}
\keyword{htest}