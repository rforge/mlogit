\name{rpar}
\alias{rpar}
\alias{print.rpar}
\alias{summary.rpar}

\title{random parameter objects}
\description{\code{rpar} objects contain the relevant information about
  estimated random parameters. The homonymous function extract on
  \code{rpar} object from a \code{mlogit} object.
}
\usage{
rpar(x, par = 1, ...)
}
\arguments{
  \item{x}{a \code{mlogit} object,}
  \item{par}{the name or the index of the parameter to be extracted,}
  \item{...}{further arguments.}
}
\value{
  a \code{rpar} object, which contain :
  \item{dist}{the name of the distribution,}
  \item{mean}{the first parameter of the distribution,}
  \item{sigma}{the second parameter of the distribution,}
  \item{name}{the name of the parameter,}
  \item{norm}{the name of the coefficient which is used for
    normalisation, if any.}
}

\details{
  \code{mlogit} objects contain an element called rpar which contain a list
  of \code{rpar} objects, one for each estimated random parameter. The
  \code{print} method prints the name of the distribution and the
  parameter, the \code{summary} behave like the one for numeric vectors.
}
\seealso{
  \code{\link{mlogit}} for the estimation of a random parameters logit model.
}

\author{Yves Croissant}
\keyword{regression}