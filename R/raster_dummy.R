#' Converte Raster categórico para Raster Dummy
#' @description
#' converte arquivo raster contendo n classes
#' em n arquivos contendo uma classe
#' tipo presença (1) e ausência (0)
#' Saída um raster múltiplo (SpatRaster) do pacote terra

#' @param r raster with categorical data
#'
#' @return SpatRaster with n layers one for each factor level
#' @export
#' @importFrom terra rast ext res unique `add<-`
#' @examples
raster_dummy <- function(r) {
  if (class(r) != "SpatRaster") {
    r <- rast(r)
  }
  rbin <- terra::rast()
  terra::ext(rbin) <- terra::ext(r)
  terra::res(rbin) <- terra::res(r)
  rnames <- vector()
  nr <- nrow(terra::unique(r))
  u <- terra::unique(r)
  u <- u[, 1]
  u <- as.numeric(u)
  for (j in 1:nr) {
    b <- r
    b[] <- ifelse(b[] == u[j], 1, 0)
    rbin <- terra::`add<-`(rbin, b)
    rnames <-  append(rnames, paste(names(r),
                                   paste("level", u[j], sep = "."), sep = "_"))
  }
  names(rbin) <- rnames
  return(rbin)
}



