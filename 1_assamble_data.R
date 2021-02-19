library(sf)
library(osmdata)
library(tidyverse)
library(raster)
library(leaflet)

## Get Kiambu data
kmb <- opq(bbox = 'Kiambu KE', timeout = 25*10) %>%
  add_osm_feature(key = 'name', value = "Kiambu", value_exact = T) %>%
  add_osm_feature(key = 'admin_level', value = "4", value_exact = T) %>%
  osmdata_sf()
## Get Nairobi Data
nrb <- opq(bbox = 'Nairobi KE', timeout = 25*10) %>%
  add_osm_feature(key = 'name', value = "Nairobi", value_exact = T) %>%
  add_osm_feature(key = 'admin_level', value = "4", value_exact = T) %>%
  osmdata_sf()
## UNION both and create a new AOI
aoi <- st_union(kmb$osm_multipolygons, nrb$osm_multipolygons, crs = 4326)
## Plot on Leaflet
leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addPolygons(data = aoi$geometry, weight = 2, fillColor = NULL)
## Start formatting the JSON data
weather.JSON <- rjson::fromJSON(file = "POWER_Regional_Daily_20170201_20210207_74d3dbce.json")
weather.JSON <- enframe(unlist(weather.JSON))
weather.JSON <- weather.JSON %>% 
  separate(name, into = c(paste0("name", 1:5)), fill = "right") %>%
  filter(name1 == "features" &
           name2 == "geometry" &
           name3 %in% c("coordinates1", "coordinates2") |
           (name1 == "features" &
              name2 == "properties" &
              name3 == "parameter")) %>%
  select(name5, value)
## Create new columns
weather.JSON <- weather.JSON %>%
  mutate(lat = if_else(is.na(name5), as.numeric(value), as.numeric(NA))) %>%
  mutate(lng = if_else(is.na(name5), as.numeric(lag(value, n = 1L)), as.numeric(NA))) %>% # Use the cell above as Longitude... This has to do with the way the JSON is structured
  mutate(date = if_else(is.na(name5), as.Date(NA), as.Date(name5, "%Y%m%d"))) %>% # Format the dates
  mutate(t2m = if_else(is.na(name5), as.numeric(NA), as.numeric(value))) %>% # Extract the measured values
  select(lat, lng, date, t2m) %>%
  filter(!is.na(lat) & !is.na(lng) |
           (!is.na(date) & !is.na(t2m)))
## This removes stand alone coordinates (only X or XY) without T2M values
while(length(ind <- which(is.na(weather.JSON$lat))) > 0){
  weather.JSON$lat[ind] <- weather.JSON$lat[ind -1]
}
while(length(ind <- which(is.na(weather.JSON$lng))) > 0){
  weather.JSON$lng[ind] <- weather.JSON$lng[ind -1]
}
weather.JSON <- weather.JSON %>%
  filter(!is.na(date) & !is.na(t2m))
## Create an sf object and assign CRS
weather.JSON <- st_as_sf(weather.JSON, coords = c("lng", "lat"), crs = 4326)
## Subset the data and start preparing for a RasterStack
sep2017Weather <- weather.JSON %>%
  filter(date > '2017-09-01' & date < '2017-10-04')
## From long-format table to wide-format table
sep2017Weather <- sep2017Weather %>%
  spread(date, t2m)
## Re-format column labels
colnames(sep2017Weather) <- gsub('-', '_', colnames(sep2017Weather), fixed=TRUE)
## Create RasterStack
st <- stack()
n <- 1
for(i in names(sep2017Weather)[!grepl("['geometry]", names(sep2017Weather))]){
  vals <- pull(sep2017Weather, i) # Read vector with`` the values of the date (i)
  r <- raster(ext = extent(st_bbox(sep2017Weather)) + 0.5, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
              resolution = 0.00833) # Create Raster object with the desired Target Resolution
  ras <- rasterize(x = sep2017Weather,
                   y = raster(ext = extent(st_bbox(sep2017Weather)) + 0.5, resolution = 0.5),
                   field = vals)
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ras <- resample(ras, r, method = "bilinear")
  st <- stack(st,ras)
  names(st[[n]]) <- as.character(i) # https://stackoverflow.com/questions/36844460/why-does-r-add-an-x-when-renaming-raster-stack-layers
  remove(vals,ras)
  n <- n + 1
}
br <- terra::rast(brick(st))
br
plot(br[[13]], col=rev(terrain.colors(10)), main = "T2M")
plot(aoi$geometry, axes = TRUE, add = T)
terra::writeRaster(br, filename = "multi.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
st_write(sep2017Weather, dsn = "pts.geojson", layer = "pts", driver = "GeoJSON")


dataCube <- function(file_name, tr){
  for(i in names(file_name)[!grepl("['geometry]", names(file_name))]){
    vals <- pull(file_name, i) # Read vector with`` the values of the date (i)
    r <- raster(ext = extent(st_bbox(file_name)) + 0.5, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                resolution = tr) # Create Raster object with the desired Target Resolution
    ras <- rasterize(x = file_name,
                     y = raster(ext = extent(st_bbox(file_name)) + 0.5, resolution = 0.5),
                     field = vals)
    crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ras <- resample(ras, r, method = "bilinear")
    st <- stack(st,ras)
    names(st[[n]]) <- as.character(i) # https://stackoverflow.com/questions/36844460/why-does-r-add-an-x-when-renaming-raster-stack-layers
    remove(vals,ras)
    n <- n + 1
  }
  br <- terra::rast(brick(st))
  return(br) 
}

br <- dataCube(sep2017Weather, 0.00833)


br <- terra::rast(brick(st))
br
plot(br[[1]], col=rev(terrain.colors(10)), main = "T2M")
plot(aoi$geometry, axes = TRUE, add = T)
terra::writeRaster(br, filename = "multi.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
st_write(sep2017Weather, dsn = "pts.geojson", layer = "pts", driver = "GeoJSON")

library(gdalUtils)
t <- raster::disaggregate(st[[1]], fact = 60)
r <- raster(ext = extent(st_bbox(sep2017Weather)) + 0.5, resolution = 0.00833)
t <- resample(st[[1]], 1, method="near")
gdal_translate(st[[1]], "test.tif", ot = "Float32", of = "GTiff", tr = c(0.00833,0.00833), r = "bilinear")

fava <- rasterize(sep2017Weather, raster::raster(), sep2017Weather$`2017_09_02`) 

fava <- st_rasterize(sep2017Weather, st_as_stars(st_bbox(sep2017Weather), values = NA_real_))

fava <- st_set_dimensions(sep2017Weather)




fava <- st_rasterize(sep2017Weather)
plot(fava, axes = TRUE)


leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addRasterImage(fava, )
  # addMarkers(data = weather.JSON1$geometry)


weather.JSON %>% 
  count(!is.na(lat) &
          !is.na(lng))

weather.JSON %>%
  group_by(Class, Survived) %>% 
  summarise(sum = sum(n)) %>%
  spread(Survived, sum)

st_point(c(as.numeric(weather.JSON[2,2]), as.numeric(weather.JSON[1,2])))
st_point(c(as.numeric(weather.JSON[2,2]), as.numeric(NA)))
st_point(c(as.numeric(NA), as.numeric(NA)))
if_else(
  is.na(weather.JSON$name5),
  st_point(c(as.numeric(lag(weather.JSON$value, n = 1L)),
             as.numeric(weather.JSON$value))),
  st_point(c(as.numeric(NA), 
             as.numeric(NA)))
)


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

