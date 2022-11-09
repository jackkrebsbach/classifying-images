library(stringr)
library(raster)
library(glcm)
source("code/lib/parse-file-path.R")

calculate_texture <- function(inpath, window = 5, statistic = "contrast", layer = 1L, doComp = TRUE) {
  #Texture Path to Write
  out_path <- getTexturePath(inpath, window, statistic, layer)
  
  if (!file.exists(inpath)) {
    doComp <- FALSE
    message(paste0(inpath, " does not exist"))
  }
  
  if (doComp) {
    #Take in file and calculate texture
    inpath %>%
      raster(band = layer) %>%
      glcm(
        statistics = statistic,
        window = c(window, window),
        shift = list(c(0, 1), c(1, 1), c(1, 0), c(1, -1))
      ) %>%
      writeRaster(filename = out_path,
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
