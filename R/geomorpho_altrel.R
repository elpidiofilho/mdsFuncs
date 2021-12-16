#' relative height metrics
#' @description calculates various relative height metrics
#'     Slope_Height, Valley_Depth, Normalized_Height,
#'     Standardized_Height, Mid_Slope_Positon using SAGA GIS
#' @param saga Rsagamcd object
#' @param mde SpatialRaster raster
#' @param folder character folder name to save files
#' @param todisk logical write the generated raster to disk
#' @return SpatialRaster with 5 layers
#' @export
#' @importFrom Rsagacmd saga_gis
#' @examples
#' # frel = geomorpho_altrel (saga, mde = dem, folder = './morpho',
#' #                          todisk = TRUE)
geomorpho_altrel  <- function(saga, mde, folder, todisk = TRUE) {

  relheights = saga$ta_morphometry$relative_heights_and_slope_positions(dem = mde)
  nm = c('Slope_Height', "Valley_Depth", 'Normalized_Height',
         'Standardized_Height', 'Mid_Slope_Positon'  )
  names(relheights) = nm
  if (todisk == TRUE) {
    list_to_fileraster(terrain = relheights, path = folder)
  }
  for (i in 1:length(nm)) {
    names(relheights[[i]]) = nm[i]
  }
  return(relheights)
}



#' Title convert lista de rasters em uma pilha de rasters
#' @param terrain list or SpatRaster
#' @param path character folder
#' @return SpatRaster
#' @importFrom terra rast setMinMax nlyr writeRaster
#' @importFrom here here
#' @importFrom sf write_sf
#' @examples
#' # rl = list_to_fileraster(alrel, "morpho")

list_to_fileraster <- function(terrain, path) {
  if (class(terrain) == 'list') {
    nl = length(terrain)
  } else {
    if (class(terrain) == 'RasterLayer') {
      nm = names(terrain)
      terrain = terra::rast(terrain)
      names(terrain) = nm
      terra::setMinMax(terrain)

      nl = 1
    } else {
      if (class(terrain) == 'SpatRaster') {
        nl = terra::nlyr(terrain)
      }
    }
  }

  for (i in 1:nl) {
    rt = terrain[[i]]
    cl = class(rt)
    if (cl[1] == "RasterLayer" | cl[1] == 'SpatRaster' ) {
      if (cl[1] == "RasterLayer" ) {rt = rast(rt)}
      names(rt) = paste0(names(terrain)[i])
      fn = paste0(names(terrain)[i],  '.tif')
      fp = here::here(path, fn)
      print(fp)
      terra::setMinMax(rt)
      terra::writeRaster(rt, filename = fp , overwrite = TRUE )
    } else {
      fn = paste0(names(terrain)[i], '.shp')
      fp = here::here(path,fn)
      print(fp)
      sf::write_sf(rt, fp, append = FALSE)
    }

  }
}

