# Roadiness
Roadiness, developed by Dr. Lucas Henneman, is a measure of how "roady" a particular area is based on its proximity to major highways & roadways. Roadiness measurements were assigned to 1 kilometer grids in the Northern Virginia / DC area. 
<br /><br />

The goal of the Roadiness project is to assign latitude & longitude GPS points collected by participants in Northern Virginia/ DC to roadiness grid cells. In doing so, we are able to observe the variability of roadiness within participant commutes and test for an association between roadiness and exposed PM 2.5 levels. <br /> 

> This repository references the Summary Commutes project, which can be found [here](https://github.com/gabiarmada/Summary-Commutes). 

## Assign lat/long GPS points to roadiness grid cells 
This section focuses on assigning unique lat/long participant GPS coordinates to roadiness levels and their corresponding grid cells. Refer to [commutes_to_gricell.R](https://github.com/gabiarmada/Roadiness/blob/main/commutes_to_gridcell.R). Let's step through this R code. <br /> 

After loading the required packages, we must first define a coordinate system. I am using the same coordinate system defined in Dr. Henneman's Roadiness plot: 

```
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
```

<br />
Next, load in the unique commutes data and assign it to a data frame `gridPoints`. This data frame contains only unique latitude & longitude GPS locations collected by participants: 

```
load(here("unique_commutes_df.Rdata"))
gridPoints <- data.frame(unique_commutes_df)
```

<br />
Next, create a SpatialPoints object from our commute data. The following lines of code extract the Longitude and Latitude columns from the `gridPoints` dataframe we initialized, and assign the output to `xy`. We use the function `SpatialPoints()`, with `xy` as the input for the coords parameter and `p4s` as the input for the proj4string parameter. Once the SpatialPoionts object has been created, we use `project()` from the `proj4` package in order to project our GPS coordinates onto the same coordinate reference system used by the Roadiness data. In doing so, our GPS coordinates data are now compatible with the Roadiness data: 

```
# extract longitude and latitude (in that order)
xy <- gridPoints[,2:1]
gridPoints <- SpatialPoints(coords = xy, proj4string = CRS(p4s))

# project longitude / latitude coordinates using coordinate reference system defined in roadiness_1km_plot.R:  
gridPoints@coords <- project(coordinates(gridPoints), p4s)
```

<br />
The following lines of code create a SpatialPolygons object from the Roadiness data. The function `rasterToPolygons()` converts the roadiness raster object to a SpatialPolygonsDataFrame, and assigns the output to the variable `gridPolygon`. Then, we number the roadiness grid cells by `1:nrow(gridPolygon)`: 

> Note: `roadiness.r` is a RasterLayer object that contains the variable `leng.distm2_scale` for each Northern Virginia & DC area grid cell. `leng.distm2_scale` is measurement for roadiness. There are 35,100 total grid cells with each one associated with a level of roadiness. Roadiness levels range from (-1.807038, 4.390482).


```
gridPolygon <- rasterToPolygons( roadiness.r)
gridPolygon@data$grid_cell <- 1:nrow(gridPolygon)
```

<br /> 
Finally, we assign our GPS commute locations to Roadiness grid cells. The function `point.in.poly()` intersects point and polygon feature classes and adds polygon attributes to points. We assign the output to the variable `pointinPolygon`, which is of class SpatialPointsDataFrame. Next, we use the function `project()` to transform our coordinates back to their original projection. <br /> <br />

We convert `pointinPolygon` to a readable dataframe using the function `as.data.frame()`, and assign the output to the variable `points_gridcell`. We remove the first column of `points_gridcell`  which contains row ID numbers; this information is unneeded. Next, we rename the Longitude & Latitude columns and relocate them to their respective order. Lastly, save the `points_gridcell` dataframe as .Rdata: 

```
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
```

The final result is a 729689 x 4 dataframe,`points_gridcell`, with column variables: `Longitude`, `Latitude`, `leng.distm2_scale`, and `gridcell`. 

## Roadiness dataset 
Let's create a roadiness dataset containing the following columns: 
> ID, date/time, commute PM, latitude of commute, longitude of commute, roadiness.  

Refer to [roadiness_dataset.R](https://github.com/gabiarmada/Roadiness/blob/main/roadiness_dataset.R). Let's step through this R code. <br /> 

<br />
After loading the required packages and `GMU_commute()` function, we must first create a commutes dataframe. Similar to the Summary Commutes Procedure, we create a list of GPS data file paths using the function `fs::dir_ls()`, and assign the output to the variable `file_paths`. We initialize the commutes dataframe and set the respective column names. 

<br /> <br />
Then, we loop our GPS data files into the `GMU_commute()` file parameter, with the output parameter set to "df". Within the loop, we mutate a column `participant` using the function `substr()` to extract participant IDs. The function `rbind()` is used to create a commutes dataframe containing all participant GPS data: 

> Note: change the start and stop parameters of `substr()` in order to reproduce this R code. 


```
# create a list of GPS data file paths 
file_paths <- fs::dir_ls(here("GPS data"))

# initialize GPS commutes data frame 
commutes_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(commutes_df) <- c("Date & Time", "Latitude", "Longitude", "Trip", "Trip duration", "Trip distance", "trip_total_time", "missing")

# loop GPS data files into GMU_commute() file parameter 
for (i in seq_along(file_paths)){
  df <- GMU_commute(file = file_paths[i], output = "df") %>%
        mutate(participant = substr(file_paths[i], start = 48, stop = 51))
  commutes_df <- rbind(commutes_df, df)
  rm(df)
}
```

<br /> 
Next, load the roadiness grid cell dataset created in the previous section: 

```
load(here("points_gricell.Rdata"))
```

<br /> 
The following lines of code are used to create a roadiness dataset. First, we transform our commutes dataframe `Date & Time` column to a POSIXct variable type using the lubridate package. Then, we select our columns of interest: `Date & Time`, `Longtiude`, `Latitude`, and `participant`. 

<br /> <br />
In order to perform a seamless left join with our commutes and roadiness grid cell data, we must first convert our Longitude and Latitude columns to character variable types. Then, we can perform a left join with `commutes_df` and `points_gridcell` by columns `Longitude` & `Latitude`, and assign the output to the variable `roadiness_commutes`. We use `na.omit()` to remove NA values from our `roadiness_commutes` dataframe. 

> Some participants yield NA values for roadiness. This is due to the fact that the roadiness grid only accounts for the region of Northern Virginia, and some participants may have driven outside of this area (i.e., to other states/ counties). Let’s remove the NA values so that only Northern Virginia commutes are included.

<br /> 
Finally, we reorder the columns of `roadiness_commutes` and save the resulting dataframe:

```
# transform commutes_df Date & Time column using lubridate package 
commutes_df$`Date & Time`<-  commutes_df$`Date & Time`%>% 
                            as_datetime(format = "%d %B %Y %H:%M:%S", tz = "America/New_York")

# select columns of interest 
commutes_df <- commutes_df[, -(4:8)]


# convert Longitude and Latitude columns to character type: 
# doing this allows for left_join to match up columns seamlessly 
commutes_df$Longitude <- as.character(commutes_df$Longitude)
commutes_df$Latitude <- as.character(commutes_df$Latitude)
points_gridcell$Longitude <- as.character(points_gridcell$Longitude)
points_gridcell$Latitude <- as.character(points_gridcell$Latitude)


# left join commutes and roadiness data : 
roadiness_commutes <-left_join(commutes_df, points_gridcell, by = c("Longitude", "Latitude"))
roadiness_commutes <- na.omit(roadiness_commutes)

# reorder columns 
roadiness_commutes <- roadiness_commutes[, c("Date & Time", "participant", "Longitude", "Latitude", "leng.distm2_scale", "grid_cell")]


# save 
save(roadiness_commutes, file = "roadiness_commutes.Rdata")
```

The final result is a 943899 x 6 dataframe,`roadiness_commutes`, with column variables: `Date & Time`, `participant`, `Longitude`, `Latitude`, `leng.distm2_scale`, and `grid_cell`.
