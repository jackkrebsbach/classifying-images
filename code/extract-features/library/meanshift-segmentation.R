library(reticulate)

Sys.setenv(
  RETICULATE_PYTHON = '/opt/homebrew/Caskroom/miniforge/base/envs/meanshift/bin/python3'
)

source_python("code/extract-features/library/segment.py")


image_segmentation_function <- function(inpaths, spatial_radii, range_radii, min_densities, doComp = FALSE, overWrite = FALSE){
  
  #Need to set the environment here because in multidplyr the environment is not copied
  Sys.setenv(RETICULATE_PYTHON = '/opt/homebrew/Caskroom/miniforge/base/envs/meanshift/bin/python3')
  source_python("code/extract-features/library/segment.py")
  
  doCompVector <- rep(doComp, length(inpaths))
  
  if (!overWrite & doComp) {
    doCompVector <-
      list(inpaths, spatial_radii, range_radii, min_densities) %>%
      pmap(.f = getSegmentPath) %>%
      unlist() %>%
      file.exists() %>%
      !.
  }
  args <-
    list(inpaths,
         spatial_radii,
         range_radii,
         min_densities,
         doCompVector)
  args %>%
    pmap(.f = segment) %>%
    unlist() %>%
    return()
}


