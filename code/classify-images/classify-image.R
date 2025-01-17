#Helpers to classify images
#library(raster)

classify_image <- function(quadrat_dir, rfit) {
  quadrat <- quadrat_dir %>%
    str_split("/") %>%
    .[[1]] %>%
    .[3]
  
  file_to_write <-
    paste0('clean_data/classified/', quadrat, '.tif')
  
  quadrat_dir %>%
    get_stack() %>%
    raster::predict(
      rf_fit,
      type = "class",
      fun = pred_fun,
      filename =  file_to_write,
      datatype = 'INT1U',
      format = "GTiff",
      overwrite = TRUE
    )
  
  return(file_to_write)
}


pred_fun <- function(...) {
  p <- predict(...)
  return(as.matrix(as.numeric(p[, 1, drop = TRUE])))
}

get_stack <- function(dir) {
  list.files(dir, pattern = ".tif", full.names = TRUE) %>%
    raster::stack(quick = TRUE) %>%
    return()
}
