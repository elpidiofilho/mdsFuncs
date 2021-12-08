## Faz a seleção de variáveis a partir do RFE
## quando existem níveis de fatores selecionados
## A função remove os níveis dos fatores e adiciona
## a variável original que produziu os níveis.
## dftreino = dataset usado no treino do rfe
## varsel = vetor com as variáveis selecionadas pelo modelo
##
#dftreino = treino; varsel = var_sel
#


seleciona_var_fator <- function(dftreino, varsel) {
  library(stringr)
  namefactor = dftreino[,-1] |>
    dplyr::select_if(is.factor) |>
    names()
  vs = character()

  for (i in 1:length(namefactor)) {
    t = str_detect(varsel, namefactor[i])
    st = sum(t)
    st
    if (st > 0) {
      varsel = varsel[!t]
      vs = c(vs, namefactor[i])
    }
  }
  varsel = c(varsel, vs)
  return(varsel)
}


library(Cubist)
library(caret)
rfFuncs$fit
cubistFuncs = rfFuncs
cubistFuncs$fit = function(x, y, first, last, ...)
{
  loadNamespace("Cubist")
  Cubist::cubist(x, y, committees = 10, ...)
}

pad3 <- function(s) {
  s = stringr::str_pad(s, 3, side = 'left', pad = '0')
  return(s)
}





gbm_custom <- getModelInfo("gbm")$gbm

gbm_custom[["predictors"]] <- function(x, ...) {
  vi <- gbm::relative.influence(x, n.trees = x$tuneValue$n.trees)
  names(vi)[vi > 0]
}

gbm_custom[["varImp"]] <- function(object, numTrees = NULL, ...) {
  if(is.null(numTrees)) numTrees <- object$tuneValue$n.trees
  varImp <- gbm::relative.influence(object, n.trees = numTrees)
  out <- data.frame(varImp)
  colnames(out) <- "Overall"
  rownames(out) <- object$var.names
  out
}




# Detecta fatores ---------------------------------------------------------


## Detecta fatores a partir de um limiar
## estabelecido. O limiar representa o
## numero máximo de valores únicos que o layer
## pode ter.
##
## r = rasterstack
## limiar = valor limite abaixo do qual a
## variável é considerada como fator
##
## A função retorna o nome das variáveis
## candidatas a fator para o limiar estabelecido
## Resultado deve ser analisado  pelo usuário
##

detect_factor <- function(r, limiar = 10) {
  vu = unique_value_raster(r)
  vf = vu[vu <= limiar]
  nf = names(vf)
  return(nf)
}

##l = list.files('./raster', glob2rx('*.asc'), full.names = TRUE)
##rr = terra::rast(l)
##vv = detect_factor(rr, limiar = 100)
##vv



#diag = raster_info(l)

#epsg = "32723"; arquivos = l


# Ajusta epsg -------------------------------------------------------------
#
# Adiciona informação do epsg para todos os rasters de um vetor
# contendo path + nome dos arquivos raster
#
#

#ajusta_epsg(epsg = "epsg:32723", l)




##




newSumm_regre <- function(data, lev = NULL, model = NULL) {
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
  rmse = hydroGOF::rmse( pred,obs,na.rm=TRUE)
  mae = hydroGOF::mae( pred,obs,na.rm=TRUE)
  hydroGOF

  CCC = epiR::epi.ccc( pred,obs)
  LCCC = CCC$rho.c[1,1]
  rss <- sum((pred - obs) ^ 2)  ## residual sum of squares
  tss <- sum((obs - mean(obs)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  pbias = sum_pred_ob/sum_obs
  epiR::epi.ccc()
  useless <- nullModel(y = pred)
  obs1= as.data.frame(obs)
  data_null =predict(useless,obs1)

  rmse_null =hydroGOF::rmse( obs,data_null,na.rm=TRUE)
  mae_null =hydroGOF::mae(  obs,data_null,na.rm=TRUE)
  RmseRelatNull  = 1 - (rmse /rmse_null)
  MaeRelatNull   = 1 - (mae/mae_null)

  out = c(nse,rmse,rsq,mae, LCCC,pbias, RmseRelatNull ,   MaeRelatNull)
  names(out) <- c('NSE','RMSE','Rsquared', 'MAE',"LCCC","PBIAS","RmseRelatNull" ,"MaeRelatNull")
}



