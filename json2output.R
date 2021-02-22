f_tblR.JSON <- function(file_name){
  require(rjson)
  require(tidyverse)
  require(sf)
  json <- enframe(unlist(fromJSON(file = file_name)))
  json <- json %>% 
    separate(name, into = c(paste0("name", 1:5)), fill = "right") %>%
    filter(name1 == "features" &
             name2 == "geometry" &
             name3 %in% c("coordinates1", "coordinates2") |
             (name1 == "features" &
                name2 == "properties" &
                name3 == "parameter")) %>%
    dplyr::select(name5, value)
  ## Create new columns
  json <- json %>%
    mutate(lat = if_else(is.na(name5), as.numeric(value), as.numeric(NA))) %>%
    mutate(lng = if_else(is.na(name5), as.numeric(lag(value, n = 1L)), as.numeric(NA))) %>% # Use the cell above as Longitude... This has to do with the way the JSON is structured
    mutate(date = if_else(is.na(name5), as.Date(NA), as.Date(name5, "%Y%m%d"))) %>% # Format the dates
    mutate(t2m = if_else(is.na(name5), as.numeric(NA), as.numeric(value))) %>% # Extract the measured values
    dplyr::select(lat, lng, date, t2m) %>%
    filter(!is.na(lat) & !is.na(lng) |
             (!is.na(date) & !is.na(t2m)))
  ## 
  while(length(ind <- which(is.na(json$lat))) > 0){
    json$lat[ind] <- json$lat[ind -1]
  }
  while(length(ind <- which(is.na(json$lng))) > 0){
    json$lng[ind] <- json$lng[ind -1]
  }
  ## This removes stand alone coordinates (only X or XY) without T2M values
  json <- json %>%
    filter(!is.na(date) & !is.na(t2m))
  return(json)
}

f_point.data <- function(file_name){
  require(sf)
  ## Input data
  data <- f_tblR.JSON(file_name)
  ## Create an sf object and assign CRS
  data <- st_as_sf(data, coords = c("lng", "lat"), crs = 4326)
  # ## Remove this
  # data <- data %>%
  #   filter(date > '2017-09-01' & date < '2017-10-04')
  # ## From long-format table to wide-format table
  data <- data %>%
    spread(date, t2m)
  ## Re-format column labels
  colnames(data) <- gsub('-', '_', colnames(data), fixed=TRUE)
  return(data)
}

# data <- f_point.data("POWER_Regional_Daily_20170201_20210207_74d3dbce.json")

f_data.Cube <- function(file_name, tr){
  pnt <- f_point.data(file_name)
  require(raster)
  st <- stack()
  n <- 1
  for(i in names(pnt)[!grepl("['geometry]", names(pnt))]){
    vals <- pull(pnt, i) # Read vector with`` the values of the date (i)
    r <- raster(ext = extent(st_bbox(pnt)) + 0.5, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                resolution = tr) # Create Raster object with the desired Target Resolution
    ras <- rasterize(x = pnt,
                     y = raster(ext = extent(st_bbox(pnt)) + 0.5, resolution = 0.5),
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

# data <- f_data.Cube("POWER_Regional_Daily_20170201_20210207_74d3dbce.json", 0.0833)
# plot(data[[13]], col=rev(terrain.colors(10)), main = "T2M")
# plot(aoi$geometry, axes = TRUE, add = T)

f_all.data <- function(file_name, resolution){
  tbl <- f_tblR.JSON(file_name)
  pnt <- f_point.data(file_name)
  br <- f_data.Cube(file_name, resolution)
  grp <- list("table" = tbl, "points" = pnt, "datacube" = br)
  return(grp)
}

# full.dataset <- f_all.data('POWER_Regional_Daily_20170201_20210207_74d3dbce.json', 0.0833)

# library(osmdata)
# ## Get Kiambu data
# kmb <- opq(bbox = 'Kiambu KE', timeout = 25*10) %>%
#   add_osm_feature(key = 'name', value = "Kiambu", value_exact = T) %>%
#   add_osm_feature(key = 'admin_level', value = "4", value_exact = T) %>%
#   osmdata_sf()
# ## Get Nairobi Data
# nrb <- opq(bbox = 'Nairobi KE', timeout = 25*10) %>%
#   add_osm_feature(key = 'name', value = "Nairobi", value_exact = T) %>%
#   add_osm_feature(key = 'admin_level', value = "4", value_exact = T) %>%
#   osmdata_sf()
# ## UNION both and create a new AOI
# aoi <- st_union(kmb$osm_multipolygons, nrb$osm_multipolygons, crs = 4326)

# library(leaflet)
# ## Plot on Leaflet
# leaflet() %>%
#   addTiles() %>% # Add default OpenStreetMap map tiles
#   # addRasterImage(raster(full.dataset$datacube[[1]]), colors = rev(terrain.colors(10)), opacity = 0.8, project = FALSE) %>%
#   addRasterImage(mask(raster(full.dataset$datacube[[1]]), st_as_sf(aoi$geometry)),
#                  colors = rev(terrain.colors(10)), opacity = 0.8, project = FALSE) %>%
#   addPolygons(data = aoi$geometry, weight = 2, fillColor = 'transparent')

# ## Write all data
# terra::writeRaster(full.dataset$datacube, filename = "POWER_Regional_Daily_201701_2021_02_07.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
# st_write(full.dataset$points, dsn = "POWER_Regional_Daily_201701_2021_02_07.geojson", layer = "pts", driver = "GeoJSON")
