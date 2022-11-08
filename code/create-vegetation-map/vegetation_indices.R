NDVI <-
  function(subIm) {
    (subIm[[5]] - subIm[[3]]) / (subIm[[5]] + subIm[[3]])
  }

Renormalized <-
  function(subIm) {
    (subIm[[5]] - subIm[[3]]) / (sqrt(subIm[[5]] + subIm[[3]]))
  }

Simple <- function(subIm) {
  (subIm[[5]]) / (subIm[[3]])
}

VARI <-
  function(subIm) {
    (subIm[[2]] - subIm[[3]]) / (subIm[[2]] + subIm[[3]] - subIm[[1]])
  }

TGI <-
  function(subIm, ...) {
    (subIm[[2]]) - (0.39 * subIm[[3]]) - (0.61 * subIm[[1]])
  }

GNDVI <- function(x, ...) {
  (x[[5]] - x[[2]]) / (x[[5]] + x[[2]])
}

ARVI <-
  function(x, ...) {
    (x[[5]] - (2 * x[[3]] - x[[1]])) / (x[[5]] + (2 * x[[3]] - x[[1]]))
  }

NGRDI <- function(x, ...) {
  (x[[2]] - x[[3]]) / (x[[2]] + x[[3]])
}

TDVI <-
  function(x, ...) {
    1.5 ** ((x[[5]] - x[[3]]) / sqrt(x[[5]] ^ 2 + x[[3]] + 0.5))
  }

indnames <-
  c(
    'Normalized Difference Vegetation Index',
    'Renormalized Difference Vegetation Index',
    'Simple/Ratio Vegetation Index',
    'Visibily Atmospherically Resistant Index',
    "Triangular Green Vegetation Index",
    'Green Normalized Difference Vegetation Index',
    'Atmospherically Resistant Vegetation Index',
    'Normalized Green-Red Difference Index',
    'Transformed Vegetation Index'
  )

indices  <-
  c(NDVI, Renormalized, Simple, VARI, TGI, GNDVI, ARVI, NGRDI, TDVI)
