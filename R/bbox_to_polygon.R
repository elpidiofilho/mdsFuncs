#' bbox to polygon
#' @description extract a bounding box of a vector file and convert
#'     to a sf polygon
#' @param vector in sf ou vect format
#' @importFrom sf st_bbox  st_as_sfc
#' @return sf polygin
#' @export
#'
#' @examples
#' # bbox_to_polygon(hidro)
bbox_to_polygon <- function(vector) {
  poly2 <- vector |>
    st_bbox() |>
    st_as_sfc()
  return(poly2)

}


raster_template <- function(vector, resolution, crs) {
  if (class(vector)[1] == 'sf') {
    vector = terra::vect(vector)
  }

  rt = rast(ext(vector), resolution = resolution,
            crs = crs)

}

