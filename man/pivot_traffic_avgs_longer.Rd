% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_traffic_intensity.R
\name{pivot_traffic_avgs_longer}
\alias{pivot_traffic_avgs_longer}
\title{Get average street lengths per raster cell by street type}
\usage{
pivot_traffic_avgs_longer(traffic_avgs_wide)
}
\arguments{
\item{traffic_avgs_wide}{data.frame, e.g. get_traffic_avgs()}
}
\value{
data.frame in long format with columns "strassenklasse" and "length"
}
\description{
Get average street lengths per raster cell by street type
}
