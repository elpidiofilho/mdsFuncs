% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{classify_equal_interval}
\alias{classify_equal_interval}
\title{Classify by equal interval}
\usage{
classify_equal_interval(r, num_interval = 5, decimal_places = 2)
}
\arguments{
\item{r}{SpatRaster raster to be classified}

\item{num_interval}{integer numero of intervals}

\item{decimal_places}{define the number of decimal places to legend}
}
\value{
SpatRaster with a categorical raster
}
\description{
Classify a integer or numeric raster using equal intervals
   defined by (max - min) / num_intervals
}
\examples{
# classify_equal_interval(r = dem, num_interval = 5, decimal_places = 2)
}
