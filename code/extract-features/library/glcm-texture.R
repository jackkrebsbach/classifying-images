library(stringr)
library(terra)
library(GLCMTextures)
library(tictoc)
source("code/lib/parse-file-path.R")

calculate_texture <- function(inpath, window = 5, statistic = "glcm_contrast", layer = 1L, doComp = TRUE) {
  #Texture Path to Write
  out_path <- getTexturePath(inpath, window, statistic, layer)
  
  if (!file.exists(inpath)) {
    doComp <- FALSE
    message(paste0(inpath, " does not exist"))
  }
  
  if (doComp) {
    inpath<- "./clean_data/quadrats/quadrat34/rgb.tif"
    statistic <- "glcm_contrast"
    layer <- 1L
    window <- 5
    #Take in file and calculate texture
    tic()
    red <-  inpath %>%
      terra::rast(lyrs = layer)
    
    contrast <- glcm_textures(
        red,
        metrics = statistic,
        w = c(window, window),
        n_levels = 16,
        shift = list(c(0, 1), c(1, 1), c(1, 0), c(1, -1)),
        quantization = "equal prob"
      )# %>%
    
    toc()
    
    
      terra::writeRaster(filename = out_path,
                  format = "GTiff",
                  overwrite = TRUE)
  }
  return(out_path)
}

texture_calculations_function <- function(inpaths, windows, statistics, layers, doComp = FALSE, overWrite = FALSE){
  doCompVector <- rep(doComp, length(inpaths))
  
  if (!overWrite & doComp) {
    doCompVector <- list(inpaths, windows, statistics, layers) %>%
      pmap(.f = getTexturePath) %>%
      unlist() %>%
      file.exists() %>%
      !.
  }
  
  args <- list(inpaths, windows, statistics, layers, doCompVector)
  args %>%
    pmap(.f = calculate_texture) %>%
    unlist() %>%
    return()
}
