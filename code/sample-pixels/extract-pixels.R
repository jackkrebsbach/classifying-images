#library(exactextractr)
#library(raster)

# extract_pixels <-
#   function(image_path, data) {
#     image <- raster::brick(image_path)
#     extent(image) <- c(0, 4032, 0, 3024) 
#     pixels <-
#       exactextractr::exact_extract(image, data, include_cell = TRUE) %>%
#       bind_rows() %>%
#       as_tibble()
#     return(pixels)
#   }


extract_pixels <-
  function(image_path, data) {
    image <- terra::rast(image_path)
    extent(image) <- c(0, 4032, 0, 3024) 
    pixels <-
      exactextractr::exact_extract(image, data, include_cell = TRUE) %>%
      bind_rows() %>%
      as_tibble()
    return(pixels)
  }