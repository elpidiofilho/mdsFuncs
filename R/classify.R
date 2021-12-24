#' Classify by quantile
#' @description Classify a integer or numeric raster using quantiles interval
#' @param r SpatRaster raster to be classified
#' @param q numeric quantiles do be used (interval 0 to 1)
#' @param decimal_places define the number of decimal places to legend
#' @return SpatRaster returns a categorical raster
#' @importFrom terra minmax setMinMax quantile classify
#' @export
#' @examples
#' # classify_quantile(r = dem, q = c(0.25, 0.5, 0.75), decimal_places = 0)
classify_quantile <- function(r, q = c(0.25, 0.5, 0.75), decimal_places = 2){

  if (class(r) %in% c('RasterStack', 'RasterLayer')) {
    r = rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('Error : file must be in RasterStack, RasterLayer or SpatRaster format')
    }
  }

    if (!mdsFuncs::IsEqual(a = max(q), b = 1)) {
    q = c(q, 1)
  }
  terra::setMinMax(r)
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
#' @importFrom terra minmax setMinMax classify
#' @export
#' @examples
#' #classify_equal_interval(r = dem, num_interval = 5, decimal_places = 2)
#'
classify_equal_interval <- function(r, num_interval = 5, decimal_places = 2){
  terra::setMinMax(r)
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

#' Classify by kmeans
#' @description Classify a integer or numeric raster using intervals calculated
#'    by kmeans cluster
#' @param r SpatRaster raster to be classified
#' @param num_cluster number of clusters to be processed
#' @param decimal_places define the number of decimal places to legend
#' @return SpatRaster with a categorical raster
#' @importFrom terra minmax setMinMax quantile classify
#' @importFrom stats kmeans
#' @importFrom dplyr group_by summarise arrange select mutate
#' @importFrom stats na.omit
#' @export
#' @examples
#' #rk = classify_kmeans(r, 4, decimal_places = 0)
   #plot(rk)

classify_kmeans <- function(r, num_cluster = 5, decimal_places = 2){
  classe = NULL
  terra::setMinMax(r)
  valor <- stats::na.omit(terra::values(r))[,1]
  kmncluster <-  stats::kmeans(valor, centers = num_cluster,
                       iter.max = 500,
                       nstart = 5,
                       algorithm = "Lloyd")

  dfk = data.frame(valor = valor, classe = kmncluster$cluster)
  dfminmax = dfk |>
    dplyr::group_by(classe) |>
    dplyr::summarise(min = min(valor), max = max(valor)) |>
    dplyr::arrange(min) |>
    dplyr::select(min, max, classe) |>
    dplyr::mutate(classe = seq(1, length(classe),1)) |>
    as.matrix()

  mx =  dfminmax
  vn = paste(round(mx[,1], decimal_places), round(mx[,2], decimal_places),
             sep = '-')
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

Classify_by_vector <- function(r, old, new) {
  df = data.frame(old, new)
  mx = matrix(df, ncol = 2)


}
