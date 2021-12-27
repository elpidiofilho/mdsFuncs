#' Determine raster unique values
#'
#' @param r SpatRaster unique layer or  raster stack to be processed
#' @return named character vector with layer names and unique values
#' @export
#' @importFrom terra rast unique nlyr
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename arrange
#' @examples
#' # unique_value_raster(r)
unique_value_raster <- function(r) {
  if (class(r) %in% c("RasterStack", "RasterLayer")) {
    r <- rast(r)
  } else {
    if (class(r) != "SpatRaster") {
      stop("Error : file must be in RasterStack or SpatRaster format")
    }
  }

  n <- terra::nlyr(r)
  vu <- as.numeric(n)
  for (i in 1:n) {
    u <- terra::unique(r[[i]], incomparables = TRUE)
    x <- lapply(u, unique)
    ur <- sapply(x, length)
    vu[i] <- ur
    names(vu)[i] <- names(r)[i]
  }
  dfu <- data.frame(vu) |>
    tibble::rownames_to_column("raster") |>
    dplyr::rename(unique = vu) |>
    dplyr::arrange(unique)
  return(dfu)
}
