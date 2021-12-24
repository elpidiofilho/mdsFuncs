#' plot raster files
#' @description  plot raster files
#' @param r SpatRaster to be ploted
#' @param num_color integer number of colors
#' @param num_decimal integer decimal places in legend
#' @param style character "sd", "equal", "pretty", "quantile",
#'     "kmeans"
#' @return NULL
#' @importFrom raster raster ratify levels unique
#' @importFrom terra minmax rast setMinMax
#' @importFrom classInt classIntervals
#' @importFrom rasterVis levelplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' # plot_raster(r = dem, num_color = 8, num_decimal = 2, style = "quantile")
plot_raster <- function(r, num_color = 10, num_decimal = 1, style = "quantile") {
  if (terra::is.factor(r)) {
    rc1 <- raster::raster(r)
    rc2 <- raster::ratify(rc1)
    rat <- raster::levels(rc2)[[1]]
    rat$classe <- 1:nrow(rat)
    levels(rc2) <- rat
    num_color <- length(raster::unique(rc1))
    myColorbar1 <- seq(1, nrow(rat), length.out = num_color)
    break1 <- classInt::classIntervals(rc2[!is.na(rc2)],
      thr = 0.05,
      n = num_color,
      style = "equal"
    )
    break1$brks <- seq(1, nrow(rat), length.out = num_color)
    myColorbar1 <- break1$brks
    # rasterVis::levelplot(rc2)
  } else {
    if (class(r) == "RasterLayer") {
      nm <- names(r)
      r <- terra::rast(r)
      names(r) <- nm
    }
    terra::setMinMax(r)
    mn <- terra::minmax(r)
    myColorbar1 <- seq(mn[1], mn[2], length.out = num_color)
    break1 <- classInt::classIntervals(r[!is.na(r)],
      thr = 0.35,
      n = num_color,
      style = style
    )
    myColorbar1 <- break1$brks
    rc1 <- r
  }

  ck <- list(
    at = break1$brks,
    labels = list(
      at = c(myColorbar1[1], myColorbar1),
      labels = c("1000000", round(myColorbar1, num_decimal)),
      col = c("white", rep("black", length(myColorbar1)))
    )
  )

  p1 <- rasterVis::levelplot(rc1,
    main = names(r),
    margin = FALSE,
    xlab = NULL,
    ylab = NULL,
    scales = list(draw = FALSE),
    col.regions = grDevices::colorRampPalette(RColorBrewer::brewer.pal(
      num_color,
      "RdYlGn"
    )),
    at = break1$brks,
    colorkey = ck
  )
  print(p1)
}
