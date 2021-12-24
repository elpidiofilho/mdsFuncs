#' df_to_point - dataframe to point
#' @description convert a dataframe with coordinates to a point feature
#' @param df dataframe data
#' @param x character name of column with x coordinates
#' @param y character name of column with y coordinates
#' @param crs characeter CRS in format "EPSG:XXXXX"
#' @param todisk boolean save the sf object do disk
#' @param folder character folder where file will be saved (must exist)
#' @param filename character file name + extension
#' @return sf object
#' @export
#' @importFrom sf st_as_sf write_sf
#' @examples
#' # df = data.frame(X = c(-42.3,-42.4,-42.5), Y = c(-20.1, -20.2, -20.3))
#' # pt1 = df_to_point(df, x = 'X', y = 'Y', crs = 'EPSG:4326',
#' #                todisk = TRUE, folder = './pontos',
#' #                filename = 'pontos.shp')
df_to_point <- function(df, x = "X", y = "Y", crs, todisk = FALSE,
                        folder = NULL, filename = NULL) {
  if (class(df) != "data.frame") {
    stop("df must be a dataframe")
  }
  if (x %in% colnames(df) == FALSE) {
    stop("x must be a valid column name")
  }
  if (y %in% colnames(df) == FALSE) {
    stop("y must be a valid column name")
  }

  if (todisk == TRUE & is.null(folder)) {
    stop("if todisk = TRUE a folder name must be defined")
  }
  if (todisk == TRUE & is.null(filename)) {
    stop("if todisk = TRUE a filename name must be defined")
  }
  pt <- sf::st_as_sf(df, coords = c(x, y), crs = crs)
  if (todisk == TRUE) {
    fn <- here::here(folder, filename)
    sf::write_sf(pt, fn, append = FALSE)
  }
  return(pt)
}
