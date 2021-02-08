library(sf)
library(rgdal)
library(osmdata)
library(terra)
library(luna)
library(leaflet)

nairobiNP <- opq ("Nairobi Kenya", timeout = 25*100) %>%
  add_osm_feature("name", "Nairobi National Park") %>%
  osmdata_sf()

nNP <- nairobiNP$osm_polygons

leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addPolygons(data = nNP, weight = 2, fillColor = NULL)

# # search image
# product <- "Landsat_8_OLI_TIRS_C1"
# sdate <- "2019-05-01" 
# edate <- "2019-05-30"
# area <- c(36.762014, -1.423142, 36.930003, -1.327116)
# dir <- getwd()
# f <- getLandsat(product, start_date=sdate, end_date=edate, aoi=area, download=FALSE, path=dir)
# 
# # list of files
# f[1:10]
# 
# # a simple function to return COG url: copy
# getCOG <- function(fu){
#   burl <- "http://landsat-pds.s3.amazonaws.com/c1/L8"
#   pr <- strsplit(fu, "_")[[1]][3]
#   path <- substr(pr,1,3)
#   row <- substr(pr,4,6)
#   imageID <- paste0(unlist(strsplit(fu, "_")) [1:7], collapse="_")
#   fp <- file.path(burl, path, row, imageID, fu)
#   turl <- file.path("/vsicurl", fp)  
#   return(turl)  
# }
# 
# # url for downloading one single file
# u1 <- getCOG(f[1])
# print(u1)
# 
# # read one file from the url 
# r1 <- rast(u1)
# r1
# 
# # try reading 4 bands: NIR (B5), RED(B4), GREEN(B3), BLUE(B2)
# f1 <- grep("B5|B4|B3|B2", f, value=TRUE)
# print(f1)
# 
# # COG url for 3 files
# fu1 <- sapply(f1, getCOG)
# print(fu1[1])
# 
# # read all three bands
# rr <- rast(fu1)
# rr
# 
# # Clip the raster
# prj <- crs(rr)
# prj
# ## [1] "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not_specified_based_on_custom_spheroid\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Sinusoidal\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
# new_nNP <- project(as(nNP, 'Spatial'), prj)
# crop(rr, spTransform(as(nNP, 'Spatial'),prj))
# 
# # plot bands ... it takes some time
# plotRGB(rr, axes = TRUE, r=3, g=2, b=1, stretch = "lin", main = "Landsat True Color Composite")

