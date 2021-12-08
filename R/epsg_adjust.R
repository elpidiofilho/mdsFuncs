#' Adjust epsg
#'
#' @description Adiciona informação do epsg para todos os rasters de um vetor
#' contendo path + nome dos arquivos raster
#'
#' @param epsg
#' @param l vector of character with rasters filenames with path of
#'
#' @return
#' @export
#'
#' @examples
epsg_adjust <- function(epsg, l) {
  if (stringr::str_detect(epsg,'epsg:') != TRUE) {
    stop('epsg deve ser escrito no formato epsg:xxxxx. Exemplo: "epsg:32723"')
  }
  nf = file.exists(l)
  ne = l[!nf]
  if (length(ne > 0)) {
    print(ne)
    stop('arquivos não existentes')
  }

  dd = tempdir()
  for (i in 1:length(l)) {
    r = terra::rast(l[i])
    terra::crs(r) <- epsg
    fn = paste0(dd,'\\',basename(l[i]))
    terra::writeRaster(r, fn, overwrite = TRUE)
  }
  result = file.copy(from = l, to = "./covar", overwrite = TRUE)
  if (sum(result) != length(result)) {
    stop('problema na cópia dos arquivos da pasta temporaria')
  } else {
    file.remove(l)
    print('operacao bem sucedida' )
    return('Ok')
  }
}

