# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(riem)
library(jsonlite)
library(sp)
library(fuzzyjoin)


#############
# load and process data from wastewater permit search
tx <- read_csv("tx/tx_dairies_feedlots.csv") 
names(tx) <- c("auth_no","permittee","sic_code","segment_no","county","region","city","site_location")
glimpse(tx)

# process urls into separate column
tx <- tx %>%
  separate(auth_no, into = c("auth_no","url"), sep = " ") %>%
  mutate(url = str_replace_all(url, "\\(|\\)","")) 

# scrape the detailed information for each permit
tx_detail <- tibble()

errors <- c()

for (u in tx$url) {
  tryCatch(
    {
      lines <- read_html(u) %>%
        html_nodes("p") %>%
        html_text() %>%
        str_squish()
      text <- paste(lines, sep = " ", collapse=" ")
      digester <- as.character(grepl("digester", text, ignore.case = TRUE))
      auth_no <- str_subset(lines, "Permit Number")
      auth_no <- str_split(auth_no, ":\\s*", simplify = TRUE)[, 2]
      print(auth_no)
      name <- str_subset(lines, "Site Name on Permit") 
      name <- str_split(name, ":\\s*", simplify = TRUE)[, 2]
      animal_type <- str_subset(lines, "animal type")
      animal_type <- str_split(animal_type, ":\\s*", simplify = TRUE)[, 2]
      latitude <- str_subset(lines, "Latitude")[1]
      latitude <- str_split(latitude, ":\\s*", simplify = TRUE)[, 2]
      longitude <- str_subset(lines, "Longitude")[1]
      longitude <- str_split(longitude, ":\\s*", simplify = TRUE)[, 2]
      tmp <- tibble(auth_no = auth_no, name = name, animal_type = animal_type, latitude = latitude, longitude = longitude, digester = digester)
      tx_detail <- bind_rows(tx_detail, tmp)
      rm(tmp,lines,text,auth_no,name,animal_type,latitude,longitude,digester)  
    },
    error = function(e) {
      errors <- append(u,errors)
    }
  )
}
rm(u)

tx_cattle <- tx_detail %>%
  unique() %>%
  filter(grepl("CATTLE",animal_type)) %>%
  rowwise() %>%
  mutate(cattle = str_extract_all(animal_type, "\\d+"),
         total = as.integer(cattle[1]),
         milking = as.integer(cattle[2]),
         milking = case_when(is.na(milking) ~ 0,
                             TRUE ~ milking),
         other = total - milking,
         type = case_when(milking == 0 ~ "Other",
                          TRUE ~ "Dairy"),
         latitude = as.double(latitude),
         longitude = as.double(longitude)) %>%
  ungroup() %>%
  select(-cattle)

# get temperature data

# get active weather stations in TX 
tx_stations <- riem_stations(network = "TX_ASOS") %>%
  filter(ymd_hms(archive_begin) <= as.Date("2022-01-01") & online == TRUE)

# get the mean temperatures in 2022 for these stations
mean_temps <- tibble()

# function to get mean temperatures for 2022 from Iowa Mesonet
get_mean_temp <- function(x) {
  temperature_data <- riem_measures(
    station = x,
    date_start = "2022-01-01",
    date_end = "2022-12-31")
  return(mean(temperature_data$tmpf, na.rm = TRUE))
}

for (i in unique(tx_stations$id)) {
  print(i)
  mean_temp = get_mean_temp(i)
  tmp <- tibble(id = i, mean_temp = mean_temp)
  mean_temps <- bind_rows(mean_temps,tmp)
  rm(tmp)
}

# remove any stations with no mean temperature data
mean_temps <- mean_temps %>%
  filter(!is.na(mean_temp))

tx_stations <- inner_join(tx_stations,mean_temps)

# convert tx cattle facilities and tx stations to spatial points data frames
sp_tx_cattle <- SpatialPointsDataFrame(tx_cattle[,c("longitude", "latitude")], tx_cattle)
sp_tx_stations <- SpatialPointsDataFrame(tx_stations[,c("lon", "lat")], tx_stations)

# calculate matrix of distances in meters between event sites and ca weather stations
dist_matrix <- raster::pointDistance(sp_tx_cattle, sp_tx_stations, lonlat = TRUE)

# get index of closest station to each event site
closest_indices <- apply(dist_matrix, 1, which.min)
# get the distances for these stations from the facility and convert to miles
distance <- apply(dist_matrix, 1, min)*0.000621371

# filter for the closest station to each facility
closest_stations <- tx_stations[closest_indices,] %>%
  cbind(distance) 

# join to TX cattle data and convert to Celsius
closest_stations <- as_tibble(closest_stations) %>%
  select(id,distance,mean_temp) %>%
  mutate(mean_temp_c = (mean_temp-32)*5/9)

tx_cattle <- bind_cols(tx_cattle, closest_stations)

#############
# calculate methane emissions

# load data for manure conversion factors by temperature 
manure <- read_csv("data/manure_emissions_factors.csv")

# difference join to Texas cattle data 
tx_cattle <- difference_left_join(tx_cattle, manure, 
                             by = c("mean_temp_c" = "temp"), 
                             max_dist = 0.5)

# calculate emissions
# division by 1000 is because we are calculating emissions in metric tons and emissions factors are in kg
tx_cattle <- tx_cattle %>%
  mutate(
         name = str_to_title(name),
         enteric_methane = (milking*138/1000) + (other*64/1000),
         manure_methane = (milking*dairy_manure_factor/1000) + (other*other_manure_factor/1000),
         total_methane = round(enteric_methane + manure_methane)
         )

write_csv(tx_cattle, "processed_data/tx_cattle.csv", na = "")

#############
# ratio of other cattle to milking cattle at TX dairies (needed for CA estimates, where head counts at dairies exclude these animals)
dairy_ratio_other_milking <- tx_cattle %>%
  filter(type == "Dairy") %>%
  summarize(milking = sum(milking),
            other = sum(other),
            ratio = other / milking)
# there are 1.093614 other cattle for every milking cow at TX dairies

# some manual cleaning here to fix case errors
# Circle T Dairy also reports a beef operation. This has been renamed Circle T to avoid confusion

# import cleaned data and add formatted tooltip for Datawrapper, remove extraneous columns
tx_cattle <- read_csv("processed_data/tx_cattle_case_cleaned.csv") %>%
  select(1:10,18:20) %>%
  mutate(labeltext = paste0("<b>Name: </b>",name,"</br>",
                            "<b>Type: </b>",type,"</br>",
                            "<b>Methane emissions: </b>",prettyNum(total_methane, big.mark = ",")," metric tons"))

tx_table <- tx_cattle %>%
  arrange(-total_methane) %>%
  select(name,type,total_methane)

# export processed data
write_csv(tx_cattle, "processed_data/tx_cattle.csv", na = "")
write_csv(tx_table, "processed_data/tx_table.csv", na = "")


  


