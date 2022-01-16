#' Region Group
#' @description associates a unique id for each group of connected pixels of same class
#' @param r SpatRaster
#' @return SpatRaster
#' @importFrom sf st_as_sf st_cast
#' @importFrom dplyr mutate row_number
#' @importFrom fasterize fasterize
#' @export
#' @examples
#' #
region_group <- function(r) {
  if (class(r) == 'RasterLayer') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  if (terra::nlyr(r) > 1) {warning( 'Multilayer file used. Only first layer will be
                                   processed')}
  tic = proc.time()
  vt = terra::as.polygons(r[[1]]) |>
    sf::st_as_sf() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    dplyr::mutate(id = dplyr::row_number())
  ft = fasterize::fasterize(sf = vt, raster = raster::raster(r[[1]]),
                            field = 'id') |>
    rast(ft)
  names(ft) = 'region_group'
  rt = c(ft, r[[1]])
  tac = proc.time()
  t = (tac - tic)[['elapsed']]
  print(paste("time elapsed:", round(t, 3), 'seconds'))
  return(rt)
}
