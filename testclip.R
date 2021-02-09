library(raster)
# use state bounds from gadm website:
# us = shapefile("USA_adm1.shp")
us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
nestates <- c("Maine", "Vermont", "Massachusetts", "New Hampshire" ,"Connecticut",
              "Rhode Island","New York","Pennsylvania", "New Jersey",
              "Maryland", "Delaware", "Virginia", "West Virginia")

ne = us[match(toupper(nestates),toupper(us$NAME_1)),]


# create a random raster over the space:        
r = raster(xmn=-85,xmx=-65,ymn=36,ymx=48,nrow=100,ncol=100)
r[]=runif(100*100)

# plot it with the boundaries we want to clip against:
plot(r)
plot(ne,add=TRUE)

# now use the mask function
rr <- mask(r, ne)

# plot, and overlay:
plot(rr);plot(ne,add=TRUE)