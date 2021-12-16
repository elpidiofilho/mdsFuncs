#' Adjust Raster epsg
#' @description Add epsg information for all rasters of a vector
#'     containing path + raster filenames
#' @param l character filename with path of rasters files
#' @param epsg character epsg in format: 'epsg:xxxxx'. Ex.'epsg:32723'
#' @return SpatRaster
#' @export
#' @importFrom stringr str_detect
#' @importFrom terra rast crs writeRaster
#' @examples
#' #l = c("./rasters/mde.tif', './rasters/slope.tif")
#' #epsg_adjust(l, "epsg:32723")
epsg_adjust <- function(l, epsg) {
  if (stringr::str_detect(epsg, "epsg:") != TRUE) {
    stop("epsg must be written in the format epsg:xxxxx.
         Example:'epsg:32723'")
  }
  nf <- file.exists(l)
  ne <- l[!nf]
  if (length(ne > 0)) {
    print(ne)
    stop("non-existing files")
  }

  dd <- tempdir()
  ll <- length(l)
  for (i in seq_len(ll)) {
    r <- terra::rast(l[i])
    terra::crs(r) <- epsg
    fn <- paste0(dd, "\\", basename(l[i]))
    terra::writeRaster(r, fn, overwrite = TRUE)
  }
  result <- file.copy(from = l, to = "./covar", overwrite = TRUE)
  if (sum(result) != length(result)) {
    stop("Problem copying files from temporary folder")
  } else {
    file.remove(l)
    print("successful operation")
    return("Ok")
  }
}
