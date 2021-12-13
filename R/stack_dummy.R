#' raster stack to dummy
#' @description convert categorical raster layers to dummy(binary) raster layers
#' @param l character list of raster filenames with path
#' @param threshold numeric maximum number of unique values to be a factor raster
#' @importFrom dplyr filter pull
#' @importFrom terra rast
#' @importFrom stringr str_detect
#' @return SpatRaster stack of binary layers and name of factor raster layers
#' @export
#' @examples
#' # rd = stack_dummy(l = l, threshold = 8)
stack_dummy <- function(l, threshold = 10) {
  r = rast(l)
  runi = mdsFuncs::unique_value_raster(r) |>
    rename(un = unique)
  sel = runi |> dplyr::filter(un <= threshold) |>
    dplyr::pull(raster)
  nl = length(sel)
  if (nl > 0 ) {
    for (i in 1:nl) {
      if (nl == 0) stop('error no name file found to transform')
      l1 = l[stringr::str_detect(string = l, pattern = sel[i])]
      if (length(l1) > 0) {
        r1 = terra::rast(l1)
        rd1 = mdsFuncs::raster_dummy(r1)
      }
      if (i == 1) {
        rr = rd1
      } else {
        rr = c(rr, rd1)
      }
    }
    return(rr)
  } else {
    warning('no raster was found within the threshold to be considered categorical')
    return(NULL)
  }
}
