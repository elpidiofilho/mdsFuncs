#' df_to_point - dataframe to point
#' @description convert a dataframe with coordinates to a point feature
#' @param df dataframe data
#' @param x character name of column with x coordinates
#' @param y character name of column with y coordinates
#' @param crs characeter CRS in format "EPSG:XXXXX"
#' @param todisk boolean save the sf object do disk
#' @param pathname character path + name + extension of file to be saved
#' @return sf object
#' @export
#' @importFrom sf st_as_sf write_sf
#' @examples
#' df = data.frame(X = c(-42.3,-42.4,-42.5), Y = c(-20.1, -20.2, -20.3))
#' pt1 = df_to_point(df, x = 'X', y = 'Y', crs = 'EPSG:4326',
#'                todisk = TRUE, pathname = './pontos/pontos.shp')

df_to_point <- function(df, x = 'X', y = 'Y', crs, todisk= FALSE,
                     pathname = NULL) {
  pt = sf::st_as_sf(df, coords = c(x, y), crs = crs)
  if (todisk == TRUE) {
    sf::write_sf(pt, pathname, append = FALSE)
  }
  return(pt)
}
