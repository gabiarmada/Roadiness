# Gabi Armada 



library(dplyr)
library(here)
library(fs)
library(lubridate)
library(data.table)

## ===================================
# create GPS commutes df : 
## ===================================
source(here("GMU_commute.R"))
file_paths <- fs::dir_ls(here("GPS data"))

commutes_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(commutes_df) <- c("Date & Time", "Latitude", "Longitude", "Trip", "Trip duration", "Trip distance", "trip_total_time", "missing")

for (i in seq_along(file_paths)){
  df <- GMU_commute(file = file_paths[i], output = "df") %>%
        mutate(participant = substr(file_paths[i], start =51, stop = 54))
  commutes_df <- rbind(commutes_df, df)
  rm(df)
}

save(commutes_df, file = "commutes_df.Rdata")

## ===================================
# unique locations df :
## ===================================
# convert latitude & longitude columns to character variable types in order for distinct() to recognize unique locations 
commutes_df$Latitude <- as.character(commutes_df$Latitude)
commutes_df$Longitude <- as.character(commutes_df$Longitude)

unique_commutes_df <- commutes_df %>% ungroup() %>%
                        select("Latitude", "Longitude", "participant") %>% 
                        distinct(Latitude, Longitude, .keep_all = TRUE)

# convert latitude & longitude columns back to numeric variable types
unique_commutes_df$Latitude <- as.numeric(unique_commutes_df$Latitude)
unique_commutes_df$Longitude <- as.numeric(unique_commutes_df$Longitude)

save(unique_commutes_df, file = "unique_commutes_df.Rdata")

## ===================================
# df of usable trips by participant : 
## ===================================
# usable_trips from summary commutes procedure 
load(here("usable_trips.Rdata"))

participant_usable_data <- mutate(usable_trips, participant = substr(participant, 1, 4)) %>%
                              group_by(participant) %>%
                              summarize(nrows = sum(as.numeric(nrows)), 
                              usable_rows = sum(as.numeric(usable_rows)), 
                              percent_usable_data = round(mean(as.numeric(percent_usable_data))*100, digits = 2), 
                              total_trips = sum(as.numeric(total_trips)), 
                              actual_trips = sum(as.numeric(actual_trips)))


save(participant_usable_data, file = "participant_usable_data.Rdata")

## =================================================
# List of IDs and % commutes covered (with lat/lon) :
## =================================================
percent_commutes <- left_join(commutes_df, participant_usable_data, by = "participant")
percent_commutes <- percent_commutes %>% 
                      select(participant, percent_usable_data, Latitude, Longitude) %>%
                      relocate(participant, 1)

save(percent_commutes, file = "percent_commutes.Rdata") 

## =================================================
# Total # of lat/lon unique locations by participant : 
## ==================================================
n_unique_locations <- commutes_df %>% 
                        group_by(participant) %>%
                        summarise(n_unique_locations = n_distinct(Latitude, Longitude))

save(n_unique_locations, file = "n_unique_locations.Rdata") 


