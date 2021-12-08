#' Create lat long rasters
#' @description cria rasters de latitude e longitude
#' @param dir_raster pasta onde estão os rasters
#' @param extensao extensão dos arquivos raster
#' @param dirSaida pasta onde será gravado os arquivos de saída
#' @importFrom terra crds crop mask writeRaster rast
#' @return
#' @export
#'
#' @examples
create_lat_long_rasters <- function(dir_raster = dir_raster,
                                 extensao = "*.tif",
                                 dirSaida = dir_raster) {

  l = list.files(dir_raster, glob2rx(extensao), full.names = TRUE)
  if (length(l) == 0)
  {stop(paste('nao existe arquivos com a extensao', extensao,
              'na pasta', dir_raster))}
  rr = terra::rast(l)
  r1 <- rr[[1]]
  lat <- long <- r1
  xy <- terra::crds(long, na.rm = FALSE) ### obtém as coordenadas de cada célula
  long[] <- xy[, 1]
  lat[] <- xy[, 2]
  contorno = extract_contour(r1)
  long = long |> terra::crop(contorno) |> terra::mask(contorno) ###
  lat = lat |> terra::crop(contorno) |> terra::mask(contorno) ###
  names(lat) = 'lat'
  names(long) = 'long'
  terra::writeRaster(long, here(dir_raster,'long.asc'), overwrite = TRUE)
  terra::writeRaster(lat,  here(dir_raster,'lat.asc'), overwrite = TRUE)
}
