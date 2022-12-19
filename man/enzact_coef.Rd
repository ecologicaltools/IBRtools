\name{enzact_coef}
\alias{enzact_coef}
\docType{data}
\title{
Coefficient values for each biomarker within different treatments
}
\description{
This data set gives coefficient values for each biomarker to be used for IBR index (functions ibr_index and ibr_std). Values were randomly assigned.
}
\usage{data("enzact_coef")}
\format{
  A data frame with 3 observations on the following 8 variables.
  \describe{
    \item{\code{treatment}}{a character vector}
    \item{\code{biomarker1}}{a numeric vector}
    \item{\code{biomarker2}}{a numeric vector}
    \item{\code{biomarker3}}{a numeric vector}
    \item{\code{biomarker4}}{a numeric vector}
    \item{\code{biomarker5}}{a numeric vector}
    \item{\code{biomarker6}}{a numeric vector}
    \item{\code{biomarker7}}{a numeric vector}
  }
}
\examples{
data(enzact_coef)
## maybe str(enzact_coef) ; plot(enzact_coef) ...
}
\keyword{datasets}
