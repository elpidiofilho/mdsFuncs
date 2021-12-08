#' Create seeds to RFE function
#'
#' @param repeats numeric number of repeats
#' @param number numeric number of folds
#' @param subsets numeric vector of subsets
#'
#' @return list of seeds
#' @export
#'
#' @examples

create_rfe_seeds <- function(repeats = 1, number = 5, subsets = (2:10)) {
  if (length(subsets) == 0) {
    stop('subsets deve ter pelo menos um valor')
  }
  if (repeats == 0) {
    stop('valor repeats deve ser maior que zero')
  }
  if (number == 0) {
    stop('valor number deve ser maior que zero')
  }
  sizes = subsets
  nl = repeats * number
  seeds = vector(mode = "list", length = nl )
  set.seed(313, kind = "Mersenne-Twister", normal.kind = "Inversion")
  for (i in 1:nl) seeds[[i]] = sample.int(n = 10000, size = length(sizes) + 1)
  seeds[[nl + 1]] = sample.int(10000, 1)
  return(seeds)
}
