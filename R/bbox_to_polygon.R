#' bbox to polygon
#' @description extract a bounding box of a vector file and convert
#'     to a sf polygon
#' @param vector in sf or terra vect format
#' @importFrom sf st_bbox  st_as_sfc
#' @return sf polygon
#' @export
#'
#' @examples
#' # bbox_to_polygon(hidro)
bbox_to_polygon <- function(vector) {
  cv = class(vector)
  if ('SpatVector' %in% cv ) {
    vector <- sf::st_as_sf(vector)
  } else {
    if (!('sf' %in% cv )) {
      stop('vector is not a sf class object')
    }
  }
  poly <- vector |>
    st_bbox() |>
    st_as_sfc(crs = st_crs(vector))
  return(poly)
}


raster_template <- function(vector, resolution, crs) {
  if (class(vector)[1] == 'sf') {
    vector = terra::vect(vector)
  }

  rt = rast(ext(vector), resolution = resolution, crs = crs)

}

