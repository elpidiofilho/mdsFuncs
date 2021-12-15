#' euc_distance - Euclidian distante to a vector
#' @description Calculates euclidean distance to point, line or polygon
#' @param r SpatRaster raster
#' @param vct SpatVector point, line or polygon
#' @param todisk boolean if true save to disk
#' @param path character path to save
#' @param filename character raster filename
#' @param ext character file extentension  (.tif, .asc, .img)
#' @importFrom terra distance writeRaster
#' @importFrom here here
#' @return SpatRaster
#' @export
#'
#' @examples
#' # rdist = euc_dist(r1, vec_pt, todisk = FALSE)

euc_dist <- function(r, vct, todisk = FALSE, path = NULL,
                     filename = NULL, ext = '.tif') {
  clv = class(vct)
  clr = class(r)
  if (clv[1] %in% c('sf', 'sp')) {
    vct = terra::vect(vct)
  } else {
    if (clv[1] != ('SpatVector'))  {
      stop('vct class must be one of sf, sp or SpatVector')
    }
  }
  if (clr[1] %in% c("RasterLayer", "RasterStack")) {
    r = rast(r)
  } else {
    if (clr[1] != "SpatRaster") {
      stop('raster must be in one of formats : RasterLayer, RasterStack
           or SpatRaster')
    }
  }

  euc_dist <- terra::distance(x = r, y = vct)
  cont <- mdsFuncs::extract_raster_contour(r)
  eucdistcut <- mdsFuncs::cut_raster(euc_dist, cont)
  if (todisk == TRUE) {
    fn = here::here(path, paste0(filename, ext))
    terra::writeRaster(x = eucdistcut, filename = fn, overwrite = TRUE)
  }
  return(eucdistcut)
}

