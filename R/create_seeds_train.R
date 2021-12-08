# Gera seeds para train ---------------------------------------------------

#' Create caret train seeds
#'
#' @param tuneLength numeric number of levels of hyperparameters
#' @param model character name of model to be used
#' @param repeats numeric number of repeats
#' @param number numeric number of CV
#' @importFrom caret modelLookup
#' @return list of seeds
#' @export
#'
#' @examples
create_train_seeds <- function(tuneLength, model = 'rf',
                             repeats = 1, number = 10) {
  models = tryCatch(
    {caret::modelLookup(modelo)},
    error = function(e){
      st = paste('modelo', modelo, 'não existe')
      stop(st)
    })
  if (tuneLength == 0) {
    stop('tuneLength deve ser maior que zero')
  }
  if (repeats == 0) {
    stop('valor repeats deve ser maior que zero')
  }
  if (number == 0) {
    stop('valor number deve ser maior que zero')
  }
  nr = tuneLength ^ nrow(models)
  nl = repeats * number

  seeds = vector(mode = "list", length = nl + 1)
  for (i in 1:nl) seeds[[i]] = sample.int(10000,  nr)
  seeds[[nl + 1]] = sample.int(10000, 1)
  return(seeds)
}
