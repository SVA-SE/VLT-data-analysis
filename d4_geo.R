library(rgdal)
library(stringr)
library(tidyverse)

data_dir <- "data/"
out_dir <- "outputs/geo"

shp <- readOGR(dsn = str_c(data_dir, "shapefiles/Lan_Sweref99TM_region.shp"), encoding = "UTF-8", stringsAsFactors = FALSE)



# geotagged <- vlt_relevant %>%
#   filter(!(is.na(Gisx) | is.na(Gisy)))
# 
# sweref99 <- geotagged %>%
#   filter(Gisy > 90 & Gisy < 1083427.2970)
# points_sweref99 <- SpatialPoints(cbind(sweref99$Gisx, sweref99$Gisy))
# proj4string(points_sweref99) <- proj_sweref99
# sweref_2_wgs84 <- spTransform(points_sweref99, proj_wgs84)
# 
# rt90 <- geotagged %>%
#   filter(Gisy >= 1083427.2970)
# points_rt90 <- SpatialPoints(cbind(rt90$Gisx, rt90$Gisy))
# proj4string(points_rt90) <- proj_rt90
# rt90_2_wgs84 <- spTransform(points_rt90, proj_wgs84)
# 
# spdf <- SpatialPointsDataFrame(cbind(geotagged$Gisx, geotagged$Gisy), geotagged)
# 
# proj4string(spdf) <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# # proj4string(spdf) <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0"
# 
# spdf <- spTransform(spdf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
