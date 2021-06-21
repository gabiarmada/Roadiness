# create Roadiness dataset


library(dplyr)
library(here)
library(fs)
library(lubridate)
library(data.table)
source(here("GMU_commute.R"))


## ============================================
# create commutes dataset : 
## ============================================
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


## ============================================
#  get roadiness gridcell data : 
## ============================================
load(here("points_gricell.Rdata"))


## ============================================
# create roadiness dataset : 
## ============================================
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

