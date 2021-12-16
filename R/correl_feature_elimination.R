
#' High correlation feature
#' @description select variables with correlation is greater then
#'    a threshold value
#' @param mcor matrix correlation matrix
#' @param testcor numeric values of max correlation to be tested
#' @param graph boolean plot graphic
#' @return
#' @export
#' @examples
#' #cor_high = correl_feat_elimin(mcor, testcor = c(0.80, 0.90, 0.95, 0.975))
correl_feat_elimin <- function(mcor, testcor = c(0.90, 0.925, 0.95, 0.975),
                                graph = TRUE) {
  df = data.frame()
  nmax = length(testcor)
  i = 1
  flag = FALSE
  for (i in 1:nmax) {
    vc = caret::findCorrelation(mcor, cutoff = testcor[i], names = TRUE)
    if (length(vc) == 0) {
      next
    } else {
      if (flag == FALSE) {
        df1 = data.frame(var_ = vc, correl = testcor[i])
        df = df1
        flag = TRUE
      } else {
        df1 = data.frame(var_ = vc, correl = testcor[i])
        df = rbind(df, df1)
      }
    }
  }
  print(nrow(df))
  if (!IsEqual(nrow(df),0)) {
    dfremove = df  |>  dplyr::group_by(var_)  |>
      dplyr::slice(which.max(correl))
    if (graph == TRUE) {
      g = ggplot(dfremove, aes(x = correl, y = reorder(var_, correl))) +
        geom_col(show.legend = FALSE, fill = 'light blue') +
        geom_text(aes(label = correl)) +
        labs(x = 'Threshold correlation', y = 'Eliminated features',
             title = 'Features elimination by correlation') +
        theme_bw()
      print(g)
    }
    return(dfremove)
  } else {
    warning('No features with high correlation')
    return(NULL)
  }
}
