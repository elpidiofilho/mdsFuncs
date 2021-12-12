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
#' seed_train = create_train_seeds(tuneLength = 5, model = 'rf',
#'                                 repeats = 3, number = 10)
create_train_seeds <- function(tuneLength, model = "rf",
                             repeats = 1, number = 10) {
  models <- tryCatch({
    caret::modelLookup(model)},
    error = function(e) {
      st <- paste("model", model, "does not exist")
      stop(st)
    })
  if (tuneLength == 0) {
    stop("tuneLength must be greater than zero")
  }
  if (repeats == 0) {
    stop("repeats value must be greater than zero")
  }
  if (number == 0) {
    stop("value number must be greater than zero")
  }
  nr <- tuneLength ^ nrow(models)
  nl <- repeats * number

  seeds <- vector(mode = "list", length = nl + 1)
  for (i in 1:nl) seeds[[i]] <- sample.int(10000,  nr)
  seeds[[nl + 1]] <- sample.int(10000, 1)
  return(seeds)
}
