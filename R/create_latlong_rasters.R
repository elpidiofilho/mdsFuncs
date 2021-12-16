#' Create lat long rasters
#' @description Creates latitude and longitude rasters
#' @param rr reference raster
#' @param todisk logical whether or not to write files to disk
#' @param filetype raster files extension .tif, .asc...
#' @param folder Folder where the output files will be saved
#' @importFrom terra crds crop mask writeRaster rast
#' @importFrom here here
#' @return A list with lat e long raster
#' @export
#' @author Elpidio Filho
#' @examples
#' # create_lat_long_rasters(rr, todisk = FALSE)
create_lat_long_rasters <- function(rr, todisk = FALSE,
                                 filetype = ".tif",
                                 folder = NULL) {

  r1 <- rr[[1]]
  lat <- long <- r1
  xy <- terra::crds(long, na.rm = FALSE)# obtém as coordenadas de cada célula
  long[] <- xy[, 1]
  lat[] <- xy[, 2]
  contorno <- extract_raster_contour(r1)
  long <- long |> terra::crop(contorno) |>
    terra::mask(contorno)
  lat <- lat |> terra::crop(contorno) |>
    terra::mask(contorno)
  names(lat) <- "lat"
  names(long) <- "long"
  if (todisk == TRUE) {
    terra::writeRaster(long, here(folder, paste0("long", filetype)),
                       overwrite = TRUE)
    terra::writeRaster(lat, here(folder, paste0("lat", filetype)),
                       overwrite = TRUE)
  }
  return(list(lat, long))
}
