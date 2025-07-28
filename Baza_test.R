require(tidyverse)
require(rgdal)
require(sp)
require(sf)
library(raster)

# reading maps

a = readOGR("Deccan_Plateau","Deccan_Plateau")
b = readOGR("Deccan_Plateau_intensity","Deccan_Plateau_intensity")
s = raster::shapefile("Deccan_Plateau/Deccan_Plateau.shp")


b$observers

