% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dftosf.R
\name{df_to_point}
\alias{df_to_point}
\title{df_to_point - dataframe to point}
\usage{
df_to_point(
  df,
  x = "X",
  y = "Y",
  crs,
  todisk = FALSE,
  folder = NULL,
  filename = NULL
)
}
\arguments{
\item{df}{dataframe data}

\item{x}{character name of column with x coordinates}

\item{y}{character name of column with y coordinates}

\item{crs}{characeter CRS in format "EPSG:XXXXX"}

\item{todisk}{boolean save the sf object do disk}

\item{folder}{character folder where file will be saved (must exist)}

\item{filename}{character file name + extension}
}
\value{
sf object of point type
}
\description{
convert a dataframe with coordinates to a point feature
}
\examples{
# df = data.frame(X = c(-42.3,-42.4,-42.5), Y = c(-20.1, -20.2, -20.3))
# pt1 = df_to_point(df, x = 'X', y = 'Y', crs = 'EPSG:4326',
#                todisk = TRUE, folder = './pontos',
#                filename = 'pontos.shp')
}
