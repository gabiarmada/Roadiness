# Assign our lat/long GPS points to roadiness grid cells 


library( sp)
library( raster)
library( spatialEco)
library( proj4)
library( dplyr)

# define coordinate reference system (from roadiness_1km_plot.R) 
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

## ====================================================== 
#  load unique commutes df 
## ====================================================== 
load("/Users/gabiarmada/Downloads/GMU URA /GPS/unique_commutes_df.Rdata")
gridPoints <- data.frame(unique_commutes_df)


## ====================================================== 
#  create Spatial Points object from commute data 
## ====================================================== 
# extract longitude and latitude (in that order)
xy <- gridPoints[,2:1]
gridPoints <- SpatialPoints(coords = xy, proj4string = CRS(p4s))

# project longitude / latitude coordinates using coordinate reference system defined in roadiness_1km_plot.R:  
gridPoints@coords <- project(coordinates(gridPoints), p4s)


## ====================================================== 
#  create Spatial Polygons object from roadiness.r 
## ====================================================== 
gridPolygon <- rasterToPolygons( roadiness.r)
gridPolygon@data$grid_cell <- 1:nrow(gridPolygon)


## ====================================================== 
#  assign our commute points to grid cells
## ====================================================== 
# find which polygons our lat/lon points intersect:  
pointinPolygon <- point.in.poly(gridPoints, gridPolygon, sp = TRUE)

# transform coordinates projection 
pointinPolygon@coords <- project(coordinates(pointinPolygon), p4s, inverse = TRUE)

# convert to data frame, clean up data frame, and save: 
points_gridcell <- as.data.frame(pointinPolygon)
points_gridcell <- points_gridcell[,-1]%>%
                      rename(Longitude = "X1", 
                             Latitude = "X2")

points_gridcell <- points_gridcell%>% 
                      relocate(c("Longitude", "Latitude"), c(1,2))

save(points_gridcell, file = "points_gricell.Rdata")
