#library(exactextractr)
#library(raster)

extract_pixels <-
  function(image_path, data, extent = c(0, 4032, 0, 3024)) {
    image <- raster::brick(image_path)
    
    if (extent) {
      extent(image) <- extent
    }
    
    pixels <-
      exactextractr::exact_extract(image, data, include_cell = TRUE) %>%
      bind_rows() %>%
      as_tibble()
    return(pixels)
  }
