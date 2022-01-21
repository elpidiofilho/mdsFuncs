#' Calls the glcm package with useful settings
#' @note for the use of glcm_texture a glcm wrapper function
#'       a raster* object is required
#' @param x rasterLayer or a rasterStack containing different channels
#' @param nrasters vector of channels to use from x. Default =nlayers(x)
#' @param kernelSize vector of numbers indicating the environment sizes for which the textures are calculated
#' @param stats string vector of parameters to be calculated.
#' @param n_grey number of grey values.
#' @param parallel logical value indicating whether parameters are calculated parallel or not
#' @param min_x for each channel the minimum value which can occur. If NULL then the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then the maximum value from the rasterLayer is used.
#' This functions calls the glcm function from with standard settings
#' and returns list of RasterStacks containing the texture parameters for each combination of channel and kernelSize
#' @param  shift =list(c(0,1), c(1,1), c(1,0),c(1,-1))
#' @author Hanna Meyer
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%do%` `%dopar%`
#' @importFrom glcm glcm
#' @return a raster* object
#' @note More information at:
#' \href{https://prism.ucalgary.ca/handle/1880/51900}{texture tutorial}
#' Keep in mind that:\cr
#' Homogeneity is correlated with Contrast,  r = -0.80\cr
#' Homogeneity is correlated with Dissimilarity, r = -0.95\cr
#' GLCM Variance is correlated with Contrast,  r= 0.89\cr
#' GLCM Variance is correlated with Dissimilarity,  r= 0.91\cr
#' GLCM Variance is correlated with Homogeneity,  r= -0.83\cr
#' Entropy is correlated with ASM,  r= -0.87\cr
#' GLCM Mean and Correlation are more independent. For the same image, GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial. GLCM Correlation shows  r<0.5 with any other measure.
#' for a review of a lot of feature extraction algorithms look at: \href{https://doi.org/10.1117/1.JEI.21.2.023016}{Williams et al, 2012, J. of Electronic Imaging, 21(2), 023016 (2012)}\cr
#' glcm <-> haralick "mean" <-> "advanced 1", "variance" <-> "advanced 2", "homogeneity" <-> "simple 4", "contrast"<-> "simple 5", "dissimilarity" <-> "advanced 2", "entropy" <-> "simple 2", "second_moment"<-> "simple 4", "correlation" <-> "simple 3"
#' Furthermore using stats will cover mean and variance while dissimilarity is highly correlated to homogeneity data.
#' @seealso \href{https://CRAN.R-project.org/package=glcm }{glcm package}
#' @export glcm_texture
#' @examples
#'
#' #require(glcm)
#' ## example on how to calculate texture with glcm
#' #data("pacman")
#' ## call glcm wrapper
#' #result <- glcm_texture(pacman,
#' #                      nrasters=1:3,
#' #                       stats=c("mean", "variance", "homogeneity"),
#' #                        parallel = FALSE)
#' #
#' ##plot the result:
#' #raster::plot(result[[1]])


glcm_texture <- function(x,
                         nrasters=1:raster::nlayers(x),
                         kernelSize=c(3),
                         stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                                 "second_moment", "correlation"),
                         shift=list(c(0,1), c(1,1), c(1,0),c(1,-1)),
                         parallel=TRUE,
                         n_grey = 8,
                         min_x=NULL,
                         max_x=NULL){


  if (parallel){
    doParallel::registerDoParallel(parallel::detectCores()-1)
  }


  #set values larger than the max/min value to the max/minvalue.
  #Otherwise NA would be used
  if(!is.null(min_x)){
    if (length(nrasters)>1){
      for (i in nrasters){
        x[[i]]=raster::reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
        x[[i]]=raster::reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
      }
    } else { # only one raster
      x=raster::reclassify(x, c(max_x,Inf,max_x))
      x=raster::reclassify(x, c(-Inf,min_x,min_x))
    }
  }


  glcm_filter<-list()
  for (j in 1:length(kernelSize)){
    if ((class (x)=="RasterStack")||(class (x)=="RasterBrick")){
      if (parallel){
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                           .packages= c("glcm","raster"))%dopar%{
                                             glcm::glcm(x[[i]],
                                                        window = c(kernelSize[j], kernelSize[j]),
                                                        shift=shift,
                                                        statistics=stats,n_grey=n_grey,
                                                        min_x=min_x[i],max_x=max_x[i],
                                                        na_opt="center")
                                           }
      } else {
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                           .packages= c("glcm","raster"))%do%{
                                             raster::mask(glcm::glcm(x[[i]],
                                                                     window = c(kernelSize[j], kernelSize[j]),
                                                                     shift=shift,
                                                                     statistics=stats,n_grey=n_grey,
                                                                     min_x=min_x[i],max_x=max_x[i],
                                                                     na_opt="center"), x[[i]])
                                           }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-raster::mask(glcm::glcm(x, window = c(kernelSize[j], kernelSize[j]),
                                                shift=shift,
                                                statistics=stats,n_grey=n_grey,
                                                min_x=min_x,max_x=max_x,
                                                na_opt="center"), x)
    }
  }
  doParallel::stopImplicitCluster()
  names(glcm_filter)<-paste0("size_",kernelSize)
  return(glcm_filter)
}
