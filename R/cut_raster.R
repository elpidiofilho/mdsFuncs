#' cut raster
#'
#' @param r  raster
#' @param poly poligono
#'
#' @return raster
#' @export
#' @importFrom terra crop mask
#' @examples
cut_raster <- function(r, poly) {
  r1 = r |> terra::crop(poly) |> terra::mask(poly)
  return(r1)
}
