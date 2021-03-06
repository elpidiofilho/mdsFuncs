% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_latlong_rasters.R
\name{create_lat_long_rasters}
\alias{create_lat_long_rasters}
\title{Create lat long rasters}
\usage{
create_lat_long_rasters(r, todisk = FALSE, filetype = ".tif", folder = NULL)
}
\arguments{
\item{r}{reference raster}

\item{todisk}{logical whether or not to write files to disk}

\item{filetype}{raster files extension .tif, .asc...}

\item{folder}{Folder where the output files will be saved}
}
\value{
A list with lat e long raster
}
\description{
Creates latitude and longitude rasters
}
\examples{
# create_lat_long_rasters(r, todisk = FALSE)
}
\author{
Elpidio Filho
}
