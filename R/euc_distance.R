#' euc_distance - Euclidean distance to a vector
#' @description Calculates euclidean distance to point, line or polygon
#' @param r SpatRaster raster
#' @param vct SpatVector point, line or polygon
#' @param todisk Boolean if true save to disk
#' @param folder character path to save
#' @param filename character raster filename
#' @param ext character file extension  (.tif, .asc, .img)
#' @importFrom terra distance writeRaster
#' @importFrom here here
#' @return SpatRaster
#' @export
#'
#' @examples
#' # rdist = euc_dist(r, vec_pt, todisk = FALSE)
euc_dist <- function(r, vct, todisk = FALSE, path = NULL,
                     filename = NULL, ext = ".tif") {
  if (class(r) %in% c("RasterLayer")) {
    r <- rast(r)
  } else {
    if (class(r) != "SpatRaster") {
      stop("Error : file must be in RasterStack or SpatRaster format")
    }
  }

  if (todisk == TRUE & is.null(filename)) {
    stop("if todisk = TRUE a filename name must be defined")
  }


  clv <- class(vct)
  clr <- class(r)
  if (clv[1] %in% c("sf", "sp")) {
    vct <- terra::vect(vct)
  } else {
    if (clv[1] != ("SpatVector")) {
      stop("vct class must be one of sf, sp or SpatVector")
    }
  }
  if (clr[1] %in% c("RasterLayer", "RasterStack")) {
    r <- rast(r)
  } else {
    if (clr[1] != "SpatRaster") {
      stop("raster must be in one of formats : RasterLayer, RasterStack
           or SpatRaster")
    }
  }

  euc_dist <- terra::distance(x = r, y = vct)
  cont <- mdsFuncs::extract_raster_contour(r)
  eucdistcut <- mdsFuncs::cut_raster(euc_dist, cont)
  if (todisk == TRUE) {
    fn <- here::here(path, paste0(filename, ext))
    if (dir.exists(file.path(here(path))) == FALSE) {
      stop("ERROR: path does not exists")
    }
    terra::writeRaster(x = eucdistcut, filename = fn, overwrite = TRUE)
  }
  return(eucdistcut)
}
