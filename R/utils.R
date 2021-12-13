#' Compare values ==
#' @export
#' @author Erdogan CEVHER
IsEqual <- function(a,b) {
  if (class(all.equal(a, b)) == "logical" ) { return(TRUE)
  } else { return(FALSE) }
}

#' Compare values <=
#' @export
#' @author Erdogan CEVHER
IsSmallerOrEqual <- function(a,b) {
  if (class(all.equal(a, b)) == "logical" && (a < b | all.equal(a, b))) { return(TRUE)
  } else if (a < b) { return(TRUE)
  } else { return(FALSE) }
}
#' Compare values >=
#' @export
#' @author Erdogan CEVHER
IsBiggerOrEqual <- function(a,b) {
  if (class(all.equal(a, b)) == "logical" && (a > b | all.equal(a, b))) { return(TRUE)
  } else if (a > b) { return(TRUE)
  } else { return(FALSE) }
}
