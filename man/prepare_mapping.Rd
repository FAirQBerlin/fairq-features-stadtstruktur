% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_reprojection.R
\name{prepare_mapping}
\alias{prepare_mapping}
\title{Prepare mapping table from reprojection.}
\usage{
prepare_mapping(df)
}
\arguments{
\item{df}{data.frame with lat, lon and geometry}
}
\value{
data.frame
}
\description{
Pull out x, y coordinates as single columns
convert lat, lon to integer.
}
