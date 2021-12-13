#' raster stack to dummy
#' @description convert categorical raster layers to dummy(binary) raster layers
#' @param l character list of raster filenames with path
#' @param threshold numeric maximum number of unique values to be a factor raster
#' @importFrom terra rast
#' @return SpatRaster stack of binary layers and name of factor raster layers
#' @export
#' @examples
#' # rd = stack_dummy(l = l, threshold = 8)
stack_dummy <- function(l, threshold = 10) {
  nl = length(l)
  if (nl > 0 ) {
    for (i in 1:nl) {
      r = rast(l[i])
      un = nrow(terra::unique(r))
      if (un <= threshold) {
        r = mdsFuncs::raster_dummy(r)
      }
      if (i == 1) {
        rr = r
      } else {
        rr = c(rr, r)
      }
    }
    return(rr)
  }
}
