#' make tiles from a raster
#' @description split a raster in a number of same size tiles
#' @param r SpatRaster
#' @param ncol number of vertical divisions
#' @param nrow  number of horizontal divisions
#' @param path  folder to save tiles
#' @param sufix sufix to be add to filename
#' @param format format of file to be saved
#' @param datatype type of data (byte, integer, float). Values for datatype are
#'    "INT1U", "INT2U", "INT2S", "INT4U", "atatype = "FLT4S"atatype = "FLT4S"", "FLT4S", "FLT8S". The first
#'    three letters indicate whether the datatype is integer (whole numbers) of
#'    a real number (decimal numbers), the fourth character indicates the number
#'    of bytes used (allowing for large numbers and/or more precision), and the
#'    "S" or "U" indicate whether the values are signed (both negative and
#'    positive) or unsigned (positive values only).
#' @author Cassio Moquedace and Elpidio Filho
#' @return SpatRaster
#' @export
#' @importFrom terra rast makeTiles nlyr crs ext
#' @importFrom here here
#' @examples
#' # tl = make_tiles(r = r1, ncol = 2, nrow = 2,
#' #      path = 'tiles', sufix = 'tl', format = '.tif', datatype = "FLT4S")
make_tiles <- function(r, ncol = 2, nrow = 2,
                       path = NULL,
                       sufix = "tile_",
                       format = ".tif",
                       datatype = "FLT4S") {
  if (class(r) %in% c("RasterStack", "RasterLayer")) {
    r <- rast(r)
  } else {
    if (class(r) != "SpatRaster") {
      stop("Error : file must be in RasterStack, RasterLayer or SpatRaster format")
    }
  }
  vtiles <- nrow
  htiles <- ncol
  ntiles <- vtiles * htiles
  x <- rast(
    vals = c(1:ntiles), ncols = htiles, nlyr = terra::nlyr(r),
    nrows = vtiles, crs = terra::crs(r), extent = terra::ext(r)
  )
  filename <- here::here(path, paste0(sufix, format))
  ff <- terra::makeTiles(r, x, filename,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2",  "TFW=YES"),
    datatype = datatype
  )
  return(ff)
}
