#' New summary function
#' @description summary function with new stats added
#' @param data  data
#' @param lev level
#' @param model model
#' @author Gustavo Veloso
#' @return
#' @export
#' @importFrom epiR epi.ccc
#' @importFrom caret nullModel
#' @examples
#' # caret::rfeFuncs$summary = newSummary
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

  sum_pred_ob <- sum(pred - obs)
  sum_obs <- sum(obs)
  mbe = mean(obs - pred) # mean bias error
  rmse <- sqrt(mean((obs - pred) ^ 2))
  # Root Relative Squared Error RRSE
  rrse <- sqrt(sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2))
  mae = mean(abs(obs - pred))
  #Relative Absolute Error (rae)
  rae <- sum(abs(obs - pred)) / sum(abs(obs - mean(obs)))
  CCC <- epiR::epi.ccc(pred, obs)
  LCCC <- CCC$rho.c[1, 1]
  rss <- sum((pred - obs) ^ 2)  ## residual sum of squares
  tss <- sum((obs - mean(obs)) ^ 2)  ## total sum of squares
  rsq <- summary(lm(pred~obs))$r.squared
  pbias <- sum_pred_ob/sum_obs
  useless <- caret::nullModel(y = pred)
  obs1 <- as.data.frame(obs)
  data_null <- predict(useless, obs1)
  rmse_null <- sqrt(mean((obs - data_null) ^ 2))
  mae_null <- mean(abs(obs - data_null))
  #rmse_null <- hydroGOF::rmse(obs, data_null, na.rm = TRUE)
  #mae_null <- hydroGOF::mae(obs, data_null, na.rm = TRUE)
  RmseRelatNull <- 1 - (rmse / rmse_null)
  MaeRelatNull <- 1 - (mae / mae_null)
  out <- c(nse, rsq, mbe, rmse, rrse,  mae, rae, LCCC, pbias, RmseRelatNull, MaeRelatNull)
  names(out) <- c("NSE", "Rsquared", "MBE", "RMSE", "RRSE", "MAE", "RAE",
                  "LCCC", "PBIAS", "RmseRelatNull", "MaeRelatNull")

  if (any(is.nan(out)))
    out[is.nan(out)] <- NA
  out
}
