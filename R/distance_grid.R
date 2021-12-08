#' Distance grid
#'
#' @param px numeric vector of x coordinates
#' @param py numeric vector of y coordinates
#' @param nx numeric number or particions in x direction
#' @param ny numeric number of partitions in y direction
#'
#' @return dataframe with point distances to grid
#' @export
#'
#' @examples
distance_grid <- function(px, py, nx, ny) {
  maxy <- max(py)
  maxx <- max(px)
  miny <- min(py)
  minx <- min(px)
  vx <- seq(from = minx, to = maxx, length.out = nx)
  vy <- seq(from = miny, to = maxy, length.out = ny)
  gr <- expand.grid(x = vx, y = vy)
  l <- length(py)
  c <- nrow(gr)
  df1 <- matrix(nrow = l, ncol = c)
  cont <- 1
  for (i in 1:c) {
    df1[, cont] <- sqrt( (gr$y[i] - py) ^ 2 + (gr$x[i] - px) ^ 2)
    names(df1)[i] <- paste("dist_", i, sep = "")
    cont <- cont + 1
  }
  df1 <- data.frame(df1)
  return(df1)
}
