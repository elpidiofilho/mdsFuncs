#' cut raster
#' @description cut raster by polygon
#' @param r  raster
#' @param poly mask polygon
#' @return raster cutted and masked
#' @export
#' @importFrom terra crop mask
#' @examples
#' #cut_raster <- function(r, poly)
cut_raster <- function(r, poly) {
  if (class(r) %in% c('RasterStack', 'RasterLayer')) {
    r = rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('Error : file must be in RasterStack, RasterLayer or SpatRaster format')
    }
  }

  r1 <- r |>
    terra::crop(poly) |>
    terra::mask(poly)
  return(r1)
}
