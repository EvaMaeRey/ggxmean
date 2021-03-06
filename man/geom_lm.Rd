% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_lm.R
\name{geom_lm}
\alias{geom_lm}
\title{ordinary least squares model}
\usage{
geom_lm(formula = y ~ x, ...)
}
\arguments{
\item{...}{}
}
\value{

}
\description{
ordinary least squares model
}
\examples{
library(ggplot2)
ggplot(cars) + aes(x = speed, y = dist) + geom_point() + geom_lm()
}
