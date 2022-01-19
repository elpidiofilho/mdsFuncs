#' @export
#'
#' @title Align Rasters
#' @author Tate Brasel https://github.com/tabrasel
#' @description Aligns multiple rasters with a single reference raster. A raster
#' will be aligned if it doesn't match the dimensions, resolution, extent,
#' origin, or CRS projection of the reference raster.
#'
#' @details Projecting a raster requires that its cell values be estimated in
#' accordance with its new projection. Continuous variable rasters will use
#' bilinear interpolation while categorical (factor) rasters will use
#' nearest-neighbor sampling.
#'
#' @param referenceRaster A \code{SpatRaster} object to be aligned with.
#' @param inputRasters A list of \code{SpatRaster} objects to align with the
#' \code{referenceRaster}.
#'
#' @return A list of \code{SpatRaster} objects that share the same grid and
#' projection as \code{referenceRaster}.
#'
#' @examples
#' \donttest{
#' #library(TerrainWorksUtils)
#'#
#' #referenceRaster <- terra::rast("C:/Work/netmapdata/Puyallup/elev_puy.flt")
#'#
#' #inputRasters <- list(
#' #   gradient = terra::rast("C:/Work/netmapdata/Puyallup/grad_15.tif"),
#'#   lithology = terra::rast("C:/Work/netmapdata/Puyallup/litho.tif")
#'# )
#'#
#' #alignedRasters <- alignRasters(referenceRaster, inputRasters)
#' #}

alignRasters <- function(referenceRaster = NULL, inputRasters = NULL) {

  # Validate parameters --------------------------------------------------------

  if (!("SpatRaster" %in% class(referenceRaster)))
    stop("Argument 'referenceRaster' must be a 'SpatRaster' object.")

  if (!("list" %in% class(inputRasters)))
    stop("Argument 'inputRasters' must be a list")

  # Align rasters --------------------------------------------------------------

  alignedRasters <- list()

  # For each input raster
  for (i in seq_along(inputRasters)) {
    inputRaster <- inputRasters[[i]]

    if (!("SpatRaster" %in% class(inputRaster)))
      stop("inputRaster[[", i, "]] must be a 'SpatRaster' object.")

    # Compare raster extents
    tryCatch({
      extentMatch <- terra::ext(inputRaster) == terra::ext(referenceRaster)
    },
    error = function(err) {
      message("Error comparing extent of inputRaster[[", i, "]] with referenceRaster:")
      stop(err)
    })

    # Compare raster dimensions
    tryCatch({
      dimensionMatch <- all(dim(inputRaster) == dim(referenceRaster))
    },
    error = function(err) {
      message("Error comparing dimensions of inputRaster[[", i, "]] with referenceRaster:")
      stop(err)
    })

    # Compare raster resolutions
    tryCatch({
      resolutionMatch <- all(terra::res(inputRaster) == terra::res(referenceRaster))
    },
    error = function(err) {
      message("Error comparing resolutions of inputRaster[[", i, "]] with referenceRaster:")
      stop(err)
    })

    # Compare raster resolutions
    tryCatch({
      originMatch <- all(terra::origin(inputRaster) == terra::origin(referenceRaster))
    },
    error = function(err) {
      message("Error comparing origins of inputRaster[[", i, "]] with referenceRaster:")
      stop(err)
    })

    # Compare raster coordinate reference systems
    tryCatch({
      crsMatch <- terra::crs(inputRaster) == terra::crs(referenceRaster)
    },
    error = function(err) {
      message("Error comparing coordinate reference systems of inputRaster[[", i, "]] with referenceRaster:")
      stop(err)
    })

    # Reproject the input raster if it doesn't align with the reference raster
    if (!extentMatch || !dimensionMatch || !resolutionMatch || !originMatch || !crsMatch) {
      tryCatch({
        # Determine what estimation method to use based on variable type (continuous/categorical)
        estimationMethod <- ifelse(terra::is.factor(inputRaster), "near", "bilinear")
        inputRaster <- terra::project(inputRaster, referenceRaster, method = estimationMethod)
      },
      error = function(err) {
        message("Error trying to project inputRaster[[", i, "]] onto referenceRaster:")
        stop(err)
      })
    }

    # Store the aligned input raster
    alignedRasters[[i]] <- inputRaster
  }

  # Return ---------------------------------------------------------------------

  return(alignedRasters)

}
