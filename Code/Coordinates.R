########  Standardizing Trap Coordinates #######

library(zoo)
library(sgeostat)
library(seriation)

coords <- read.csv(file = "Data/xy_canal_traps18_NAsedited.csv")

#### Fill in missing trap coordinate via linear interpolation (zoo package)
# na.approx
na.approx(coords$lon)
## -78.73273
na.approx(coords$lat)
## 39.61775

coords$lon <- na.fill(na.approx(coords$lon), "extend")
coords$lat <- na.fill(na.approx(coords$lat), "extend")


###### Saving filled in vector to a csv and text file to use in GIS

write.table(coords, file = "Data/coords.txt", sep = "\t",
            row.names = FALSE, col.names = TRUE)
write.csv(coords, file = "Data/coords.csv", row.names = FALSE)


###### Creating a distance matrix from coordinate file

coords_dist <- round(GeoDistanceInMetresMatrix(coords))

## https://github.com/clipo/Seriation/blob/master/R/distanceCalculations.R

ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}
GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
    
  }
  
  n.geopoints <- nrow(df.geopoints)
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("trap", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$trap
  colnames(mat.distances) <- df.geopoints$trap
  return(mat.distances)
}

distance.mat.m <- GeoDistanceInMetresMatrix(coords)
#  Error in while (abs(lamda - lamda.old) > 1e-11) { : argument is of length zero 
