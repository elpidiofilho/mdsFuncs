#' Frequency of modal values
#' @description Calculates the frequency of occurrence of the
#'     modal value for each cell/pixel of a raster stack
#' @param r SpatRaster
#' @param ncores integer number of cores for multiprocessing
#' @author <elpidio@ufv.br>
#' @return SpatRaster
#' @export
#' @importFrom terra app
#' @examples
#' # modalfreq = mode_freq(r = r1, ncores = 2)

mode_freq <- function(r, ncores = 2) {
  if (class(r) == 'RasterStack') {
    r = rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('Error : file must be in RasterStack or SpatRaster format')
    }
  }
  Mode_count <- function(x) {
    ux <- unique(x)
    ux <- ux[!is.na(ux)]
    mode <- ux[which.max(tabulate(match(x, ux)))]
    mc <- sum(x == mode, na.rm = TRUE)
    return(mc)
  }
  freq <- terra::app(r, fun = Mode_count, cores = ncores)
  return(freq)
}

