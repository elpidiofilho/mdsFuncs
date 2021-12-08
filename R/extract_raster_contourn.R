
#' Extract vector of raster contour and
#' create a polygon
#'
#' @param rst  raster in SpatRaster format
#'
#' @return a polygon with contourn of raster
#' @export
#' @importFrom terra classify as.polygons
#' @examples
#' extract_raster_contourn(r)
#'
extract_raster_contour <- function(rst) {
  rst_rec <- terra::classify(rst,
                             matrix(c(-Inf, +Inf, 1),
                                    ncol = 3, byrow = TRUE))
  vect_count <- terra::as.polygons(rst_rec)
  return(vect_count)
}
