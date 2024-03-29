% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_coordinates.R
\name{compute_group_coordinates}
\alias{compute_group_coordinates}
\title{A Function to Label Points}
\usage{
compute_group_coordinates(data, scales)
}
\arguments{
\item{data}{this is the data set where the points are found}

\item{scales}{}
}
\value{
this function returns the coordinates of the point as: (x,y)
}
\description{
A Function to Label Points
}
\examples{
library(dplyr)
library(magrittr)
cars \%>\%
  mutate(x = speed, y = dist) \%>\%
  select(x,y) \%>\%
  compute_group_coordinates()
}
