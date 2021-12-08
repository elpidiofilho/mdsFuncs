#' Create lat long rasters
#' @description cria rasters de latitude e longitude
#' @param rr raster de referencia
#' @param todisk boolean gravar ou não os arquivos no disco
#' @param extensao extensão dos arquivos raster .tif, .asc...
#' @param dirSaida pasta onde será gravado os arquivos de saída
#' @importFrom terra crds crop mask writeRaster rast
#' @importFrom here here
#' @return lat e long em uma lista
#' @export
#'
#' @examples
create_lat_long_rasters <- function(rr, todisk = TRUE,
                                 extensao = ".tif",
                                 dirSaida = getwd()) {

  r1 <- rr[[1]]
  lat <- long <- r1
  xy <- terra::crds(long, na.rm = FALSE) ### obtém as coordenadas de cada célula
  long[] <- xy[, 1]
  lat[] <- xy[, 2]
  contorno = extract_raster_contour(r1)
  long = long |> terra::crop(contorno) |> terra::mask(contorno) ###
  lat = lat |> terra::crop(contorno) |> terra::mask(contorno) ###
  names(lat) = 'lat'
  names(long) = 'long'
  if (todisk == TRUE) {
    terra::writeRaster(long, here(dirSaida, paste0('long',extensao)), overwrite = TRUE)
    terra::writeRaster(lat,  here(dirSaida, paste0('lat',extensao)), overwrite = TRUE)
  }
  return(list(lat, long))
}
