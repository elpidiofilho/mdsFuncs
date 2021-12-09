#' Frequency of modal values
#'
#' @param r SpatRaster
#' @param ncores integer number of cores for multiprocessing
#' @author <elpidio@ufv.br>
#' @return SpatRaster
#' @export
#' @importFrom terra app
#' @examples
#' rmfreq = mode_freq(r = r1, ncores = 2)

mode_freq <- function(r, ncores = 4) {
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

