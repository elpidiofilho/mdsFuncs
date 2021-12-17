#' Compare values ==
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsEqual <- function(a, b) {
  if (class(all.equal(a, b)) == "logical" ) { return(TRUE)
  } else { return(FALSE) }
}

#' Compare values <=
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsSmallerOrEqual <- function(a, b) {
  if (class(all.equal(a, b)) == "logical" && (a < b | all.equal(a, b))) { return(TRUE)
  } else if (a < b) { return(TRUE)
  } else { return(FALSE) }
}
#' Compare values >=
#' @description https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#' @param a first value to be compared
#' @param b last value to be compared
#' @export
#' @author Erdogan CEVHER
IsBiggerOrEqual <- function(a,b) {
  if (class(all.equal(a, b)) == "logical" && (a > b | all.equal(a, b))) {
    return(TRUE)
  } else if (a > b) { return(TRUE)
  } else { return(FALSE) }
}

#'
exit <- function() { invokeRestart("abort") }
