########  Standardizing Trap Coordinates #######

library(zoo)
library(rgdal)
# library(sgeostat)
# library(seriation)

coords <- read.csv(file = "Data/xy_canal_traps18_NAsedited.csv")
str(coords)
summary(coords)

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

# convert to utm to have distance in meters
coords_dd = SpatialPoints(coords[ , c("lon", "lat")], proj4string=CRS("+proj=longlat"))
coords_utm <- spTransform(coords_dd, CRS("+init=epsg:26917"))

dist_mat <- dist(as.data.frame(coords_utm))
summary(dist_mat)

summary(log(dist_mat))
log_matrix <- log(dist_mat)

################

# Take out sites H and I  **DONE
# Create a multisession EDF file (1-12 representing sites; pg. 193 and 221) ** DONE
# Plug multisession EDF (per species) and matrix ("Coordinates.R") into model
  ## Add site_num onto logit and P?
# Obtain session estimates and average density overall (pg. 222)

######

library(dplyr)
library(tidyr)
library(rjags)
library(parallel)

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)
head(EDF)
summary(EDF)

EDF <- EDF %>%
  filter(site != "H" & site != "I" & species == "CPIC")
EDF

#Add a new column for integer session values (session = site)

EDF$site_num <- as.integer(as.factor(EDF$site))
summary(EDF)


