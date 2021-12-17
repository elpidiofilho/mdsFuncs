#' Recover info from spatRaster
#' Create dataframe with characteristcs of a list of raster
#' @description Create dataframe with characteriscts of a list of raster
#' @param l vector of rater filenames inclunding path
#' @return dataframe with rasters characteristics.
#' @export
#' @importFrom terra rast nlyr ncell nrow ncol yres xres crs
#' @importFrom dplyr summarise_at pull rename_all vars n_distinct select one_of
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#' @importFrom stats lm predict reorder sd var
#' @importFrom utils glob2rx setTxtProgressBar txtProgressBar
#' @examples
#' # raster_info(l)
#'
raster_info <- function(l) {
  EPSG <- freq <- NULL
  nf <- file.exists(l)
  ne <- l[!nf]
  if (length(ne > 0)) {
    print(ne)
    stop("non-existing files")
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

  vprob <- dfunique |> t() |>
    data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename_all(~c("var", "freq")) |>
    dplyr::filter(freq > 1) |>
    dplyr::pull(var)

  if (length(vprob) > 0) {
    print("Problems. Raster parameters are not the same.")
    dfprob <- dfinfo |>
      dplyr::select(one_of(vprob))
    dfprob <- data.frame(layer = dfinfo$layer, dfprob)
    print(dfprob)
    print("Problems. Raster parameters are not the same..")
    print(paste("Problem detected in:", paste(names(dfprob)[-1],
                                                collapse = ", ")))
  } else {
    print(dfinfo)
    print("Okay. Raster parameters are the same.")
  }
  return(dfinfo)
}
