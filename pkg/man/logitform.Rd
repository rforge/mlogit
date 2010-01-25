\name{logitform}
\alias{logitform}
\alias{is.logitform}
\alias{logitform.formula}
\alias{model.matrix.logitform}
\alias{model.frame.logitform}
\title{Model formula for logit models }
\description{
  Two kinds of variables are used in logit models: alternative specific and
  individual specific variables. \code{logitform} provides a relevant
  class to deal with this specificity and suitable methods to extract
  the elements of the model.
}
\usage{
logitform(object)
\method{logitform}{formula}(object)
is.logitform(object)
\method{model.matrix}{logitform}(object, data, ...)
\method{model.frame}{logitform}(formula, data, ..., lhs = NULL, rhs = NULL)
}
\arguments{
  \item{object}{for the \code{logitform} function, a formula or a list,
    for the \code{update} and \code{model.matrix} methods, a
    \code{logitform} object,}
  \item{formula}{a \code{logitform} object,}
  \item{data}{a \code{data.frame},}
%  \item{id}{the indvidual index in case of panel data,}
  \item{lhs}{see \code{Formula}}
  \item{rhs}{see \code{Formula}}
  \item{...}{further arguments.}
    
}

\value{

  an object of class \code{logitform}.
}


\details{

a \code{logitform} is a formula for which the right hand side may
contain two parts: the first one contains the alternative specific
variables and the second one the individual specific variables. The two
parts are separeted by a ``\code{|}'' sign. If a standard formula is writen, it
is assumed that there are no individual specific variables. Specific
methods are provided to build correctly the model matrix and to update
the formula. The \code{logitform} function is not intended to be use
directly. While using the \cite{\code{mlogit}} function, the first
argument is automaticaly coerced to a \code{logitform} object.
}

\examples{
v <-logitform(y~x1+x2|z1+z2)
update(v,.~.+x3-x1|.+z3-z1)
update(v,.~.-x1-x2|.)
update(v,.~.|.-z1-z2)
v <-logitform(y~x1+x2)
update(v,.~.-x1+x3)
update(v,.~.|z1+z2)

data("Fishing",package="Ecdat")
colnames(Fishing)[4:11] <- c("pr.beach","pr.pier","pr.boat","pr.charter",
                             "ca.beach","ca.pier","ca.boat","ca.charter")
Fish <- mlogit.data(Fishing,varying=c(4:11),shape="wide",choice="mode")
f <- logitform(mode~pr+ca|income)
m <- model.frame(f, Fish)
X <- model.matrix(f,data=m)
X[1:6,]

}
\author{Yves Croissant}
\keyword{models}