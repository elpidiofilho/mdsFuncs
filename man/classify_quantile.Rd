% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{classify_quantile}
\alias{classify_quantile}
\title{Classify by quantile}
\usage{
classify_quantile(r, q = c(0.25, 0.5, 0.75), decimal_places = 2)
}
\arguments{
\item{r}{SpatRaster raster to be classified}

\item{q}{numeric quantiles do be used (interval 0 to 1)}

\item{decimal_places}{define the number of decimal places to legend}
}
\value{
SpatRaster returns a categorical raster
}
\description{
Classify a integer or numeric raster using quantiles interval
}
\examples{
# classify_quantile(r = dem, q = c(0.25, 0.5, 0.75), decimal_places = 0)
}
