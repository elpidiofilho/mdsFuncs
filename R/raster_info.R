
#' Create dataframe with characteriscts of a list of raster
#'
#' @param l vector of rater filenames inclunding path
#'
#' @return dataframe with rasters characteristics.
#' @export
#' @importFrom terra rast nlyr ncell nrow ncol yres xres crs
#' @importFrom dplyr summarise_at pull rename_all vars n_distinct select one_of
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#' @examples
#'
raster_info <- function(l) {
  nf <- file.exists(l)
  ne <- l[!nf]
  if (length(ne > 0)) {
    print(ne)
    stop("arquivos não existentes")
  }

  rl <- length(l)
  nl <- 0
  cont <- 1
  dfinfo <- data.frame(layer = character(rl),
                      nrow = integer(rl), ncol = integer(rl),
                      nlyr = integer(rl),
                      ncell = integer(rl), xres = numeric(rl),
                      yres = numeric(rl),
                      datum = character(rl), epsg = numeric(rl))
  for (i in 1:rl) {
    r <- terra::rast(l[i])
    nl <- terra::nlyr(r)
    if (nl > 1) {

    } else {
      dfinfo$layer[cont] <- names(r)
      dfinfo$nrow[cont] <- terra::nrow(r)
      dfinfo$ncol[cont] <- terra::ncol(r)
      dfinfo$nlyr[cont] <- terra::nlyr(r)
      dfinfo$ncell[cont] <- terra::ncell(r)
      dfinfo$xres[cont] <- terra::xres(r)
      dfinfo$yres[cont] <- terra::yres(r)
      dfinfo$datum[cont] <- terra::crs(r, describe = TRUE)[1]
      ll <- data.frame(terra::crs(r, describe = TRUE)[2]) |>
        dplyr::pull(EPSG) |> as.numeric()
      dfinfo$epsg[cont] <- ll
      cont <- cont + 1
    }
  }

  lt <- table(dfinfo$nrow, exclude = NULL)
  length(lt)
  lt <- table(dfinfo$epsg, exclude = NULL)
  length(lt)

  dfunique <- dfinfo |>
    dplyr::summarise_at(dplyr::vars(-layer), dplyr::n_distinct)

  vprob <- dfunique |> t() |> data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename_all(~c("var", "freq")) |>
    dplyr::filter(freq > 1) |>
    dplyr::pull(var)

  if (length(vprob) > 0) {
    print("Problemas. Parametros dos rasters não são iguais.")
    dfprob <- dfinfo |>
      dplyr::select(one_of(vprob))
    dfprob <- data.frame(layer = dfinfo$layer, dfprob)
    print(dfprob)
    print("Problemas. Parametros dos rasters não são iguais.")
    print(paste("Problema detectado em:", paste(names(dfprob)[-1],
                                                collapse = ", ")))
  } else {
    print("Ok. Parametros dos rasters são iguais.")
    print(dfinfo)
    print("Ok. Parametros dos rasters são iguais.")
  }
  return(dfinfo)
}
