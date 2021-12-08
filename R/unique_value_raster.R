
#' Determine raster unique valuese
#'
#' @param r
#'
#' @return named character vector with layer names and unique values
#' @export
#' @importFrom terra unique
#' @examples
#' unique_value_raster(r)

unique_value_raster <- function(r) {
  u <- terra::unique(r, incomparables = TRUE)
  x <- lapply(u, unique)
  ur <- sapply(x, length)
  names(ur) <- names(r)
  return(ur)
}
