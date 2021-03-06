% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moda.R
\name{mode_freq}
\alias{mode_freq}
\title{Frequency of modal values}
\usage{
mode_freq(r, ncores = 2)
}
\arguments{
\item{r}{SpatRaster}

\item{ncores}{integer number of cores for multiprocessing}
}
\value{
SpatRaster
}
\description{
Calculates the frequency of occurrence of the
    modal value for each cell/pixel of a raster stack
}
\examples{
# modalfreq = mode_freq(r = r1, ncores = 2)
}
\author{
<elpidio@ufv.br>
}
