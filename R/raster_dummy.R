#' Converts Categorical Raster to Dummy Raster
#' @description Convert raster file containing n classes
#'     in n files containing one class
#'     type presence (1) and absence (0)
#'     Output a multiple raster (SpatRaster) from the terra packet
#' @param r raster with categorical data
#' @return SpatRaster with n layers one for each factor level
#' @export
#' @importFrom terra rast ext res unique `add<-`
#' @examples
#' #dumr = raster_dummy(r)
raster_dummy <- function(r) {
  if (class(r) != "SpatRaster") {
    r <- rast(r)
  }
  rbin <- terra::rast()
  terra::ext(rbin) <- terra::ext(r)
  terra::res(rbin) <- terra::res(r)
  rnames <- vector()
  nr <- nrow(terra::unique(r))
  u <- terra::unique(r)
  u <- u[, 1]
  u <- as.numeric(u)
  for (j in 1:nr) {
    b <- r
    b[] <- ifelse(b[] == u[j], 1, 0)
    rbin <- terra::`add<-`(rbin, b)
    rnames <- append(rnames, paste(names(r),
                                   paste("level", u[j], sep = "."), sep = "_"))
  }
  names(rbin) <- rnames
  return(rbin)
}



