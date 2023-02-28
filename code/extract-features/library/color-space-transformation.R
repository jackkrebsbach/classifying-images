library(raster)
library(terra)
library(tidyverse)
library(magick)
library(imager)
source("code/lib/parse-file-path.R")

# Define custom color space transforms here (transformations not in the imager package)
# color_transform takes function names from imager package
# "RGBtoHSV" "RGBtoLab" "RGBtoHSL" "RGBtoHSI" "RGBtoYCbCr" "RGBtoYUV"

color_transform <- function(inpath, transform = "RGBtoHSV", doComp = TRUE) {
  
  #Get Path to Write
  out_path <- getColorPath(inpath, transform)
  print(out_path)
  color_space <- getColorSpace(out_path)
  if(!file.exists(inpath)){ doComp <- FALSE; message(paste0(inpath, " does not exist")) }
  
  if (doComp) {
    # read in image with imager
    #Cant get working with imager so trying magick
    
    image <- inpath %>%
      magick::image_read() %>%
      imager::magick2cimg()
    
    # get transform from environment
    clr_transform <- transform %>%
      get()
    
    # do transform
    rast <- image %>%
      clr_transform() %>%
      drop() %>%
      as.array() %>%
      terra::rast() %>%
      terra::t() %>%
      terra::stretch()
    
    names(rast) <- paste0(color_space, "_", seq(terra::nlyr(rast)))
    
    rast[is.na(rast)] <- 254
    rast[is.nan(rast)] <- 254
    rast[rast == 255] <- 254
    NAflag(rast) <- 255
    
    terra::writeRaster(
      x = rast,
      filename = out_path,
      datatype = "INT1U",
      overwrite = TRUE
    )
    
    rm(rast)
  }
  # return the written file path
  return(out_path)
}


color_transforms_function <- function(inpaths, transforms, doComp = FALSE, overWrite = FALSE){
  
  doCompVector <- rep(doComp, length(inpaths))
  
  if(!overWrite & doComp){
    doCompVector <- list(inpaths, transforms) %>% 
      pmap(.f = getColorPath) %>% 
      unlist() %>% 
      file.exists() %>%
      !.
  } 
  args <- list(inpaths, transforms, doCompVector)
  args %>% 
    pmap(.f = color_transform) %>% 
    unlist() %>%
    return()
}
