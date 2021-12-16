#' Classify by quantile
#' @description Classify a inteter or numeric raster using quantiles interval
#' @param r SpatRaster raster to be classified
#' @param q numeric quantiles do be usse (interval 0 to 1)
#' @param decimal_places define the number of decimal places to legend
#' @return SpatRaster returns a categorical raster
#' @export
#' @examples
#' # classify_quantile(r = dem, q = c(0.25, 0.5, 0.75), decimal_places = 0)
classify_quantile <- function(r, q = c(0.25, 0.5, 0.75), decimal_places = 2){
  if (!mdsFuncs::IsEqual(a = max(q), b = 1)) {
    q = c(q, 1)
  }
  setMinMax(r)
  mm = terra::minmax(r)
  lq = length(q)
  c = seq(1:lq)
  qtf <- terra::quantile(r[], q, na.rm = TRUE)
  qti = c(mm[1], qtf[-lq])
  mx =  cbind(qti, qtf, c)
  qp2 = q * 100
  qp1 = c(0, qp2[-lq])
  vn = paste(qp1, paste0(qp2, '%'), sep = '-')
  row.names(mx) = vn
  colnames(mx) = c('min', 'max', 'class')
  rc = terra::classify(r, mx, include.lowest = TRUE)
  vn = paste0(mx[,3],' ',
              round(mx[,1],decimal_places), ' - ',
              round(mx[,2], decimal_places))
  rc1 = rc - 1
  levels(rc1) <- vn
  names(rc1) = paste0(names(rc1), '-reclass')
  print(mx)
  return(rc1)
}


#' Classify by equal interval
#' @description Classify a integer or numeric raster using equal intervals
#'    defined by (max - min) / num_intervals
#' @param r SpatRaster raster to be classified
#' @param num_interval integer numero of intervals
#' @param decimal_places define the number of decimal places to legend
#' @return SpatRaster with a categorical raster
#' @export
#' @examples
#' classify_equal_interval(r = dem, num_interval = 5, decimal_places = 2)
#'
classify_equal_interval <- function(r, num_interval = 5, decimal_places = 2){
  setMinMax(r)
  mm = terra::minmax(r)
  interval = (mm[2] - mm[1]) / num_interval
  qti = seq(mm[1], mm[2], interval)
  qtf = qti[-1]
  qti = qti[-(num_interval + 1)]
  lq = num_interval
  c = seq(1:lq)
  mx =  cbind(qti, qtf, c)
  vn = paste(round(qti, decimal_places), round(qtf,decimal_places), sep = '-')
  row.names(mx) = vn
  colnames(mx) = c('min', 'max', 'class')
  rc = terra::classify(r, mx, include.lowest = TRUE)
  vn = paste0(mx[,3],' ',
              round(mx[,1],decimal_places), ' - ',
              round(mx[,2], decimal_places))
  rc1 = rc - 1
  levels(rc1) <- vn
  names(rc1) = paste0(names(rc1), '-reclass')
  print(mx)
  return(rc1)
}
