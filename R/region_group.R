#' Region Group
#' @description associates a unique id for each group of connected pixels of same class
#' @param r SpatRaster
#' @return SpatRaster
#' @export
#' @examples
#' #
region_group <- function(r) {
  if (class(r) == 'RasterLayerStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  tic = proc.time()
  l1 = r[[1]]
  terra::setMinMax(l1)
  mn = terra::unique(l1)[,1]
  flag = FALSE
  for (i in seq_along(mn)) {
    ix = mn[i]
    l2 = l1
    l2[l2 != ix] <- NA
    rg = terra::patches(l2, directions = 8, zeroAsNA = FALSE)
    if (flag == FALSE) {
      st = rg
      flag = TRUE
    } else {
      mn1 = terra::minmax(st[[lasti]])
      rg =  rg + mn1[2]
      st = c(st, rg)
    }
    lasti = i
  }
  sumst = app(st, fun = sum, na.rm = TRUE)
  sumst = c(sumst, l1)
  names(sumst) = c('region_group', names(l1))
  terra::setMinMax(sumst)
  tac = proc.time()
  t = (tac - tic)[['elapsed']]
  print(paste("time elapsed:", round(t,3), 'seconds'))
  return(sumst)
}
