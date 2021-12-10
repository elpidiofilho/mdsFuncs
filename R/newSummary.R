#' New summary function
#'
#' @param data
#' @param lev
#' @param model
#' @author Gustavo Veloso
#' @return
#' @export
#' @importFrom hydroGOF rmse mae
#' @importFrom epiR epi.ccc
#' @examples
newSummary <- function(data, lev = NULL, model = NULL)
{
  isNA <- is.na(data$pred)
  isNA1 <- is.na( data$obs)
  isNA2 = intersect(isNA, isNA1)

  pred <- data$pred[!isNA2]
  obs <- data$obs[!isNA2]

  mu <- mean(obs)
  mse <- mean( (obs - pred) ^ 2)
  mo <- mean( (obs - mu) ^ 2)
  nse <- 1 - (mse / mo)

  sum_pred_ob = sum(pred - obs)
  sum_obs = sum(obs)
  rmse = hydroGOF::rmse(pred, obs, na.rm = TRUE)
  mae = hydroGOF::mae(pred, obs, na.rm = TRUE)
  CCC = epiR::epi.ccc( pred,obs)
  LCCC = CCC$rho.c[1,1]
  rss <- sum((pred - obs) ^ 2)  ## residual sum of squares
  tss <- sum((obs - mean(obs)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  pbias = sum_pred_ob/sum_obs

  useless <- nullModel(y = pred)
  obs1 = as.data.frame(obs)
  data_null = predict(useless, obs1)

  rmse_null = hydroGOF::rmse(obs, data_null, na.rm = TRUE)
  mae_null = hydroGOF::mae(obs, data_null, na.rm = TRUE)
  RmseRelatNull  = 1 - (rmse /rmse_null)
  MaeRelatNull   = 1 - (mae/mae_null)
  out = c(nse, rmse, rsq, mae, LCCC, pbias, RmseRelatNull, MaeRelatNull)
  names(out) <- c('NSE', 'RMSE', 'Rsquared', 'MAE', "LCCC", "PBIAS",
                  "RmseRelatNull" ,"MaeRelatNull")

  if (any(is.nan(out)))
    out[is.nan(out)] <- NA
  out
}
