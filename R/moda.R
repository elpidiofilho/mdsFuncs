#' Frequency of modal values
#'
#' @param r SpatRaster
#' @param ncores integer number of cores for multiprocessing
#' @author <elpidio@ufv.br>
#' @return SpatRaster
#' @export
#' @importFrom terra app
#' @examples

mode_freq <- function(r, ncores = 4) {
  Mode_count <- function(x) {
    ux <- unique(x)
    ux = ux[!is.na(ux)]
    mode = ux[which.max(tabulate(match(x, ux)))]
    sum(x == mode, na.rm = TRUE)
  }

  freq <- terra::app(r, fun = Mode_count, cores = ncores)
  return(freq)
}

