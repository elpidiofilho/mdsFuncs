#' Easily obtain UTM EPSG code
#'
#' Determine the UTM (WGS84) EPSG code of the center of the extent of a `SpatRaster` or `SpatVector`
#' @author Vincent Godard https://github.com/VincentGodard
#' @param obj  input raster or vector (`SpatRaster` or `SpatVector`)
#'
#' @return EPSG code
#' @importFrom terra is.lonlat ext project
#' @export
#'
#' @examples
#' #
get_utm <- function(obj) {
  dem0 <-  NULL
  if (!class(obj)[1] %in% c("SpatRaster", "SpatVector"))
    {stop("Argument must be a SpatRaster or SpatVector")}
  if (terra::is.lonlat(obj)) {
    ext0 = terra::ext(obj)
  } else {
    ext0 <- terra::ext(terra::project(dem0, "epsg:4326"))
  }
  epsg = as.numeric(32700 - round((45 + (ext0[3] + ext0[4]) / 2) / 90, 0) * 100 +
                      round((183 + (ext0[1] + ext0[2]) / 2) / 6, 0))
  epsg_e = as.numeric(32700 - round((45 + (ext0[3] + ext0[4]) / 2) / 90, 0) * 100 +
                        round((183 + ext0[1]) / 6, 0))
  epsg_w = as.numeric(32700 - round((45 + (ext0[3] + ext0[4]) / 2) / 90, 0) * 100 +
                        round((183 + ext0[2]) / 6, 0))
  if ((epsg != epsg_e) | (epsg != epsg_w))
    {warning(print("Object probably extends over several UTM zones"))}
  return(epsg)
}
