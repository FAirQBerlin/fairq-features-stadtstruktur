% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_geo_utils.R
\name{lines_to_points}
\alias{lines_to_points}
\title{Sample points from lines while keeping attributes}
\usage{
lines_to_points(lines_sf_obj, n = 1, points = TRUE)
}
\arguments{
\item{lines_sf_obj}{sf object (lines)}

\item{n}{numeric, number of points to be sampled from lines per metre (if crs
unit is metres)}

\item{points}{shall return value be of geometry_type POINTS? Defaults to TRUE.
If FALSE, return value me be of geometry_type MULTIPOINTS}
}
\value{
sf object (points) with attributes of the input lines sf obj
}
\description{
Sample points from lines while keeping attributes
}
