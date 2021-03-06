% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_raster.R
\name{plot_raster}
\alias{plot_raster}
\title{plot raster files}
\usage{
plot_raster(r, num_color = 10, num_decimal = 1, style = "quantile")
}
\arguments{
\item{r}{SpatRaster to be ploted}

\item{num_color}{integer number of colors}

\item{num_decimal}{integer decimal places in legend}

\item{style}{character "sd", "equal", "pretty", "quantile",
"kmeans"}
}
\description{
plot raster files
}
\examples{
# plot_raster(r = dem, num_color = 8, num_decimal = 2, style = "quantile")
}
