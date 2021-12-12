
#' Determine raster unique valuese
#'
#' @param r SpatRaster unique ou raster stack to be preocessed
#' @return named character vector with layer names and unique values
#' @export
#' @importFrom terra unique
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename
#' @examples
#' unique_value_raster(r)

unique_value_raster <- function(r) {
  n = terra::nlyr(r)
  vu = as.numeric(n)
  for (i in 1:n) {
    u <- terra::unique(r[[i]], incomparables = TRUE)
    x <- lapply(u, unique)
    ur <- sapply(x, length)
    vu[i] = ur
    names(vu)[i] <- names(r)[i]
  }
  dfu = data.frame(vu) |>
    tibble::rownames_to_column('raster') |>
    dplyr::rename(unique = vu) |>
    dplyr::arrange(unique)
  return(dfu)
}
