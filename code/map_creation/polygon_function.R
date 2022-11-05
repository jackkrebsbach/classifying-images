

# Takes 2-d vector and converts it to 3 dimensions
make3D <- function(t) {
  return(c(t[1], t[2], 0))
}

cross.product <- function(x, y) {
  x <- make3D(x)
  y <- make3D(y)
  
  boolean <- function(x) {
    if (x[3] > 0) {
      return("TRUE")
    }
    else{
      return("FALSE")
    }
  }
  return(boolean(cross(x, y)))
}

# Quadrant (0-3) is a subset of the quadrat
# See https://docs.google.com/document/d/1fZsO6068yEvLJnU3if5tpVF9bU1i7cqadtJvj_2KteU/edit
#
Polygon.Function <- function(Quadrant, coordinates) {
  c <- slot(subset(coordinates, corner == "c"), "coords")
  c <- c(as.numeric(c[1]), as.numeric(c[2]))
  i <- slot(subset(coordinates, corner == "i"), "coords")
  i <- c(as.numeric(i[1]), as.numeric(i[2]))
  ii <- slot(subset(coordinates, corner == "ii"), "coords")
  ii <- c(as.numeric(ii[1]), as.numeric(ii[2]))
  
  # Incase we mislabeled corners 1 and 2 we flip them.
  if (cross.product(ii - c, i - c)) {
    i <- i
    ii <- ii
  }
  else{
    i <- ii
    ii <- i
  }
  
  # Computing all coordinates that we need
  iii <- i + (ii - c)
  s1 <- (c + i) / 2
  s2 <- (i + iii) / 2
  s3 <- (ii + iii) / 2
  s4 <- (c + ii) / 2
  m <-  (c + iii) / 2
  
  # Compute coordinates for the Quadrant we want
  if (Quadrant == 0) {
    coords = matrix(c(c, s1, m, s4, c), ncol = 2, byrow = TRUE)
  }
  if (Quadrant == 1) {
    coords = matrix(c(s1, i, s2, m, s1), ncol = 2, byrow = TRUE)
  }
  if (Quadrant == 2) {
    coords = matrix(c(s4, m, s3, ii, s4), ncol = 2, byrow = TRUE)
  }
  if (Quadrant == 3) {
    coords = matrix(c(m, s2, iii, s3, m), ncol = 2, byrow = TRUE)
  }
  if (Quadrant == 'total') {
    coords = matrix(c(c, i, iii, ii, c), ncol = 2, byrow = TRUE)
  }
  
  P1 = Polygon(coords)
  quadrant <- SpatialPolygons(list(Polygons(list(P1), ID = "a")))
  return(quadrant)
}                                                                                                                    