% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stack_dummy.R
\name{stack_dummy}
\alias{stack_dummy}
\title{raster stack to dummy}
\usage{
stack_dummy(l, threshold = 10)
}
\arguments{
\item{l}{character list of raster filenames with path}

\item{threshold}{numeric maximum number of unique values to be a factor raster}
}
\value{
SpatRaster stack of binary layers and name of factor raster layers
}
\description{
convert categorical raster layers to dummy(binary) raster layers
}
\examples{
# rd = stack_dummy(l = l, threshold = 8)
}
