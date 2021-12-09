
#' Extract raster contour
#'
#' @description Extract vector of raster contour and create a polygon
#'
#' @param r raster in SpatRaster format
#'
#' @return a polygon with contour of raster
#' @export
#' @importFrom terra classify as.polygons
#' @examples
#' extract_raster_contourn(r)
#'
extract_raster_contour <- function(r) {
  r_rec <- terra::classify(r,
                             matrix(c(-Inf, +Inf, 1),
                                    ncol = 3, byrow = TRUE))
  vect_count <- terra::as.polygons(r_rec)
  return(vect_count)
}
