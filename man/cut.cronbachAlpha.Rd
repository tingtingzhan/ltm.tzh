% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cronbachAlpha.R
\name{cut.cronbachAlpha}
\alias{cut.cronbachAlpha}
\title{Categorize \code{cronbachAlpha}}
\usage{
\method{cut}{cronbachAlpha}(
  x,
  breaks = c(-Inf, 5:9/10, Inf),
  labels = c("unacceptable", "poor", "questionable", "acceptable", "good", "excellent"),
  right = FALSE,
  include.lowest = TRUE,
  ordered_result = TRUE,
  ...
)
}
\arguments{
\item{x}{an object of class \code{'cronbachAlpha'},
returned from function \link[ltm]{cronbach.alpha}}

\item{breaks, labels, right, include.lowest, ordered_result, ...}{see function \link[base]{cut.default}}
}
\description{
An S3 method dispatch of generic function \link[base]{cut}.
}
\examples{
library(ltm)
m = cronbach.alpha(LSAT, CI = TRUE, B = 500) 
m |> cut()
}
\references{
\url{https://www.researchgate.net/figure/Range-of-reliability-and-its-coefficient-of-Cronbachs-alpha_tbl1_326698967}
}
