plot_labeled <-
  function(im_path,
           json_path,
           maxpixels = 5e+05,
           out_path = NULL) {
    sf <- label_me_json_to_sf(json_path)
    ras <- brick(im_path)
    p <- ggplot() +
      ggRGB(
        ras,
        r = 1,
        g = 2,
        b = 3,
        ggLayer = TRUE,
        maxpixels = maxpixels
      ) +
      geom_sf(data = sf, mapping = aes(fill = label)) +
      theme_minimal()
    if (!is.null(out_path)) {
      ggsave(out_path)
    }
    return(p)
  }
