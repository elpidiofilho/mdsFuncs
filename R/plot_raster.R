#' plot raster files
#' @description  plot raster files
#' @param r SpatRaster to be ploted
#' @param num_color integer number of colors
#' @param num_decimal integer decimal places in legend
#' @param style character "sd", "equal", "pretty", "quantile",
#'     "kmeans"
#' @return
#' @export
#' @examples
#' # plot_raster(r = dem, num_color = 8, num_decimal = 2, style = "quantile")

plot_raster <- function(r, num_color = 10, num_decimal = 1, style = "quantile") {
#  all_style = c("fixed", "sd", "equal", "pretty", "quantile",
#                "kmeans", "hclust", "bclust", "fisher",
#                "jenks", "dpih" ,"headtails")

  if (class(r) == 'RasterLayer') {
    nm = names(r)
    r = rast(r)
    names(r) = nm
  }
  setMinMax(r)
  mn = minmax(r)
  if (is.factor(r) == TRUE) {
    num_color = nrow(unique(r))
  }
  myColorbar1 <- seq(mn[1] ,mn[2], length.out = num_color)
  break1 <- classInt::classIntervals(r[!is.na(r)],
                                     thr = 0.35,
                                     n = num_color,
                                     style = estilo)
  myColorbar1 = break1$brks
  p1 <- rasterVis::levelplot(r,
                             main = names(r),
                             margin = FALSE,
                             xlab = NULL,
                             ylab = NULL,
                             scales = list(draw = FALSE),
                             col.regions = colorRampPalette(brewer.pal(num_color,
                                                                       'RdYlGn')),
                             at = break1$brks,
                             colorkey = list(at = break1$brks,
                                             labels = list(at = c(myColorbar1[1],
                                                                  myColorbar1),
                                                           labels = c("1000000",
                                                                      round(myColorbar1,
                                                                            num_decimal)),
                                                           col = c("white",
                                                                   rep("black",
                                                                       length(myColorbar1)))
                                             )
                             )
  )
  print(p1)
}
