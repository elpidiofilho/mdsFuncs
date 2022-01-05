#' Compare values ==
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsEqual <- function(a, b) {
  if (class(all.equal(a, b)) == "logical") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Compare values <=
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsSmallerOrEqual <- function(a, b) {
  if (class(all.equal(a, b)) == "logical" && (a < b | all.equal(a, b))) {
    return(TRUE)
  } else if (a < b) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
#' Compare values >=
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsBiggerOrEqual <- function(a, b) {
  if (class(all.equal(a, b)) == "logical" && (a > b | all.equal(a, b))) {
    return(TRUE)
  } else if (a > b) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'
exit <- function() {
  invokeRestart("abort")
}


#' Get epsg SIRGAS
#' @description get the epsg to convert geo to UTM projection and SIRGAS datum
#' @param lonlat numeric vector of size 2 with values of longitud and latitud
#' @return numeric epsg number
#' @export
#' @examples
#' get_epsg_SIRGAS(c(-42, -20))
get_epsg_SIRGAS = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] < 0) {
    utm + 31960
  } else{
    stop('Use WGS84 instead SIRGAS')
  }
}


#' Get epsg WGS
#' @description get the epsg to convert geo to UTM projection and WGS datum
#' @param lonlat numeric vector of size 2 with values of longitud and latitud
#' @return numeric epsg number
#' @export
#' @examples
#' get_epsg_WGS(c(-42, -20))
get_epsg_WGS = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
