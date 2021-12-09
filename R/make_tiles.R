#' make tiles from a raster
#' @description
#' @param r SpatRaster
#' @param ncol número de divisões verticais
#' @param nrow  número de divisões horizontais
#' @param path  pasta onde será salvo os tiles (com / no final)
#' @param sufix sufixo a ser adicionada ao nome dos arquivos
#' @param format formato do arquivo a ser gravado (com ponto no inicio)
#' @author Cassio Moquedace e Elpidio Filho
#' @return SpatRaster
#' @export
#' @importFrom terra rast makeTiles
#' @examples
#' tl = make_tiles(r = r1, ncol = 2, nrow = 2,
#'      path = './tiles', sufix = 'tl', format = '.tif'))
make_tiles <- function(r, ncol = 2, nrow = 2,
                       path = "./tiles/",
                       sufix = "tile_",
                       format = ".tif") {
  vtiles <- nrow
  htiles <- ncol
  ntiles <- vtiles * htiles
  x <- rast(vals = c(1:ntiles), ncols = htiles, nlyr = nlyr(r),
            nrows = vtiles, crs = crs(r), extent = ext(r))
  plot(x)
  filename <- paste0(path, sufix, format)
  ff <- makeTiles(r, x, filename, overwrite = TRUE,
                  gdal = c("COMPRESS=LZW"))
  return(ff)
}
