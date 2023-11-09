library(terra)

extract_pixels <- function(image_path, polygon){
  image <- terra::rast(image_path)
  poly_info <- polygon %>% 
    filter(row_number() == 1)%>%
    st_drop_geometry()
  pixels <- terra::extract(image, polygon, cells = TRUE, extent = c(0, 4032, 0, 3024)) %>%
    cross_join(poly_info) %>%
    dplyr::select(-ID)
  return(pixels)
}

# image_path <- "./clean_data/quadrats/quadrat34/rgb.tif"
# polys <- readRDS("./clean_data/polys_sf.rds")
# 
# poly <- polys %>% filter(quadrat == 1)
# pixels <- extract_pixels(image_path, poly)
# 
# print(format(object.size(pixels), units = "MB"))
# print(nrow(pixels))
# print(sum(poly$area))
