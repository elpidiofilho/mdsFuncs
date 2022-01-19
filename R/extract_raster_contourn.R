#' Extract raster contour
#' @description Extract vector of raster contour and create a polygon
#' @param r raster in SpatRaster format
#' @return a polygon with contour of raster in sf format
#' @author <Cassio Moquedace>
#' @export
#' @importFrom terra classify as.polygons
#' @examples
#' # extract_raster_contour(r)
extract_raster_contour <- function(r) {
  if (class(r) %in% c("RasterLayer")) {
    r <- rast(r)
  } else {
    if (class(r) != "SpatRaster") {
      stop("Error : file must be in RasterStack or SpatRaster format")
    }
  }
  if (is.na(terra::crs(r))) {
    stop("r does not have a coordinate system")
  }
  r_rec <- terra::classify(r[[1]],
                           matrix(c(-Inf, +Inf, 1), ncol = 3, byrow = TRUE))
  vect_count <- terra::as.polygons(r_rec)
  return(vect_count)
}
