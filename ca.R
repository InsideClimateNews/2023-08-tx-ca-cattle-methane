# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(lubridate)
library(fuzzyjoin)
library(riem)
library(sp)
library(tidygeocoder)
library(readxl)
library(sf)

#############
# load and process data from wastewater permit search
ca <- read_csv("data/ca_dairies_feedlots.csv") %>%
  clean_names()

# classify facility types and estimate nonmilking cattle at dairies, which are not included in the CA head counts
# for this we assume 1.093614 other cattle for every milking cow, based on data for TX dairies
ca_cattle <- ca %>%
  mutate(latitude = as.double(latitude),
         longitude = as.double(longitude),
         cafo_population = as.integer(cafo_population),
         type = case_when(grepl("Calf feedlots|Heifers|Finishing|pairs", cafo_subtype) ~ "Other",
                          grepl("Mature dairy",cafo_subtype) ~ "Dairy",
                          TRUE ~ "Not cattle"),
         milking = case_when(type == "Other" ~ 0,
                             type == "Dairy" ~ cafo_population),
         other = case_when(type == "Other" ~ cafo_population,
                           type == "Dairy" ~ round(cafo_population * 1.093614)),
         total = milking + other,
         latitude = as.double(latitude),
         longitude = as.double(longitude)) %>%
  filter(!grepl("Not cattle", type) & cafo_population > 0) %>% # removes non-cattle CAFOs and those with no reported animals present
  select(facility_name,facility_address,latitude,longitude,county,cafo_population,cafo_subtype,type,milking,other,total) %>%
  unique()

# check if any facilities lack coordinates
no_coords <- ca_cattle %>%
  filter(is.na(latitude))

# geocode these and incorporate into data
geocoded <- geocode(no_coords, 
                    address = facility_address,
                    method = "arcgis", 
                    full_results = TRUE)
         
geocoded <- geocoded %>%
  mutate(latitude = lat, longitude = long) %>%
  select(facility_name,facility_address,latitude,longitude)
         
ca_cattle_coords <- anti_join(ca_cattle,no_coords)
         
ca_cattle_geocoded <- ca_cattle %>%
  select(-latitude,-longitude) %>%
  inner_join(geocoded)
  
ca_cattle <- bind_rows(ca_cattle_coords,ca_cattle_geocoded)
         
       
#############
# get 2022 mean temperature data from nearest station in Iowa Environmental Mesonet

# get active weather stations in CA
ca_stations <- riem_stations(network = "CA_ASOS") %>%
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

for (i in unique(ca_stations$id)) {
  print(i)
  mean_temp = get_mean_temp(i)
  tmp <- tibble(id = i, mean_temp = mean_temp)
  mean_temps <- bind_rows(mean_temps,tmp)
  rm(tmp)
}

# remove any stations with no mean temperature data
mean_temps <- mean_temps %>%
  filter(!is.na(mean_temp))

ca_stations <- inner_join(ca_stations,mean_temps)

# convert ca cattle facilities and ca stations to spatial points data frames
sp_ca_cattle <- SpatialPointsDataFrame(ca_cattle[,c("longitude", "latitude")], ca_cattle)
sp_ca_stations <- SpatialPointsDataFrame(ca_stations[,c("lon", "lat")], ca_stations)

# calculate matrix of distances in meters between facilities and ca weather stations
dist_matrix <- raster::pointDistance(sp_ca_cattle, sp_ca_stations, lonlat = TRUE)

# get index of closest station to each facility
closest_indices <- apply(dist_matrix, 1, which.min)
# get the distances for these stations from the facility and convert to miles
distance <- apply(dist_matrix, 1, min)*0.000621371

# filter for the closest station to each facility
closest_stations <- ca_stations[closest_indices,] %>%
  cbind(distance) 

# combine with CA cattle data and convert to Celsius
closest_stations <- as_tibble(closest_stations) %>%
  select(id,distance,mean_temp) %>%
  mutate(mean_temp_c = (mean_temp-32)*5/9)

ca_cattle <- bind_cols(ca_cattle, closest_stations)

#############
# calculate methane emissions

# load data for manure conversion factors by temperature 
manure <- read_csv("data/manure_emissions_factors.csv")

# difference join to CA data and account for any unmatched values outside of temperature range
ca_cattle <- difference_left_join(ca_cattle, manure, 
                                  by = c("mean_temp_c" = "temp"), 
                                  max_dist = 0.5) 
  
ca_cattle <- ca_cattle %>%
  mutate(dairy_manure_factor = case_when(mean_temp_c < 10 ~ 48,
                                         mean_temp_c > 28 ~ 112,
                                         TRUE ~ dairy_manure_factor),
         other_manure_factor = case_when(mean_temp_c < 10 ~ 1,
                                         mean_temp_c > 28 ~ 2,
                                         TRUE ~ other_manure_factor))
         

# calculate emissions
# division by 1000 is because we are calculating emissions in metric tons and emissions factors are in kg
ca_cattle <- ca_cattle %>%
  mutate(enteric_methane = (milking*138/1000) + (other*64/1000),
         manure_methane = (milking*dairy_manure_factor/1000) + (other*other_manure_factor/1000),
         total_methane = round(enteric_methane + manure_methane))

#############
# geospatial joins to biogas digester and manure management emissions mitigation projects

# load project  data, reading all columns as text initially
ca_projects <- read_excel("data/ca_mitigation.xlsx", col_types = rep("text", ncol(read_excel("data/ca_mitigation.xlsx"))), sheet = 2) 

# filter for biogas digesters and manure management projects and process data
ca_digesters_manure <- ca_projects %>%
  clean_names() %>%
  filter(grepl("digester|manure", sub_program_name, ignore.case = TRUE)) %>%
  select(1:20,date_operational,project_completion_date,funding_recipient) %>%
  separate(lat_long, into = c("latitude","longitude"), sep = ",") %>%
  mutate(
    latitude = as.double(latitude),
    longitude = as.double(longitude),
    date_operational = as.Date(as.integer(date_operational), origin = "1899-12-30"),
    project_completion_date = mdy(project_completion_date),
    total_project_ghg_reductions = as.double(total_project_ghg_reductions),
    total_project_cost = as.double(total_project_cost),
    total_program_ggrf_funding = as.double(total_program_ggrf_funding),  
    project_life_years = as.double(project_life_years),
    annual_project_ghg_reductions = total_project_ghg_reductions/project_life_years
  ) %>%
  filter(total_project_ghg_reductions > 1) %>% # removes research and demo projects
  mutate(date_operational = case_when(is.na(date_operational) | date_operational < "1900-01-01" ~ project_completion_date,
                                      TRUE ~ date_operational)) %>% # fixes problems with these dates
  arrange(-annual_project_ghg_reductions)

# geocode from project addresses
ca_digesters_manure <- geocode(ca_digesters_manure, address = address, method = "arcgis", full_results = TRUE) 

ca_digesters_manure <- ca_digesters_manure %>%
  select(1:29) %>%
  rename(lat_project = lat, 
         long_project = long,
         arcgis_address_project = arcgis_address,
         geocode_project_score = score) 

ca_digesters_manure <- ca_digesters_manure %>%
  mutate(lat_project = case_when(is.na(lat_project) ~ latitude,
                                 TRUE ~ lat_project),
         long_project = case_when(is.na(long_project) ~ longitude,
                                  TRUE ~ long_project)) %>%
  select(-latitude,-longitude)

ca_digesters_manure_sf <- ca_digesters_manure %>%
  st_as_sf(coords = c("long_project","lat_project"),
           crs = st_crs("EPSG:4326")) 

# geocode the CA cattle data from facility addresses
ca_cattle <- geocode(ca_cattle, address = facility_address, method = "arcgis", full_results = TRUE) 

ca_cattle <- ca_cattle %>%
  select(1:25)  %>%
  rename(lat_facility = lat,
         long_facility = long,
         arcgis_address_facility = arcgis_address,
         geocode_facility_score = score) %>%
  mutate(lat_facility = case_when(is.na(lat_facility) ~ latitude,
                                  TRUE ~ lat_facility),
         long_facility = case_when(is.na(long_facility) ~ longitude,
                                   TRUE ~ long_facility)) 

ca_cattle_sf <- ca_cattle %>%
  st_as_sf(coords = c("long_facility","lat_facility"),
           crs = st_crs("EPSG:4326"))

# find nearest facility to each project
nearest_features <- st_nearest_feature(ca_digesters_manure_sf,ca_cattle_sf)  
matched_ca_cattle_sf <- ca_cattle_sf[nearest_features, ]
matched_ca_cattle <- matched_ca_cattle_sf %>% 
  st_drop_geometry()
ca_digesters_manure <- ca_digesters_manure_sf %>%
  st_drop_geometry()

# compute distances
distances <- st_distance(matched_ca_cattle_sf, ca_digesters_manure_sf, by_element = TRUE)

distances <- as.numeric(distances/1609.34) # convert to miles

# combine data and write to csv
ca_mitigation_joined <- bind_cols(matched_ca_cattle, ca_digesters_manure) %>%
  mutate(project_distance = distances) %>%
  arrange(project_distance)

write_csv(ca_mitigation_joined, "processed_data/ca_mitigation_joined.csv", na = "")

# some manual editing in Google Sheets required here to fix incorrect joins

#############
# account for emissions reductions from the mitigation projects

# import the cleaned data
ca_mitigation_joined <- read_csv("processed_data/ca_mitigation_joined_cleaned.csv")

# incorporate into CA cattle data, remove extraneous columns
# also convert emissions reductions from metric tonnes carbon equivalent to metric tonnes of methane
ca_cattle_no_mitigation <- anti_join(ca_cattle, ca_mitigation_joined) 

ca_cattle_mitigation <- inner_join(ca_cattle,ca_mitigation_joined)

ca_cattle_inc_mitigation <- bind_rows(ca_cattle_mitigation,ca_cattle_no_mitigation) %>%
  select(1:11,19:21, project_id_number, sub_program_name, project_life_years, date_operational, annual_project_ghg_reductions) %>%
  mutate(annual_project_ghg_reductions = annual_project_ghg_reductions/25)

# calculate adjusted manure methane and total methane emissions
# many of the mitigation projects, especially the biogas digesters, claim emissions reductions that are larger than our estimates of manure emissions
# in these cases, set manure emissions to zero
ca_cattle_inc_mitigation <- ca_cattle_inc_mitigation %>%
  mutate(adjusted_manure_methane = case_when(!is.na(project_id_number) ~ manure_methane - annual_project_ghg_reductions,
                                             TRUE ~ manure_methane),
         adjusted_manure_methane = case_when(adjusted_manure_methane < 0 ~ 0,
                                             TRUE ~ adjusted_manure_methane),
         adjusted_total_methane = round(enteric_methane + adjusted_manure_methane))

# chart to look at estimated emissions before and after adjustment for farms with mitigation projects
ggplot(ca_cattle_inc_mitigation %>% filter(!is.na(project_id_number)), aes(x=total_methane, y = adjusted_total_methane)) + 
  geom_point(alpha = 0.5, aes(color = sub_program_name)) +
  geom_smooth(aes(color = sub_program_name), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_abline(intercept = 0, slope = 1, linewidth= 0.5, linetype = "dotted") +
  geom_hline(yintercept = 0, linewidth = 0.1) +
  scale_color_discrete(name = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") +
  xlab("Emissions before adjustment (metric tons)") +
  ylab("Adjusted emissions (metric tons)")

glimpse(ca_cattle_inc_mitigation)

# add formatted tooltip for Datawrapper and em dashes in facility names
ca_cattle_inc_mitigation <- ca_cattle_inc_mitigation %>%
  mutate(mitigation = case_when(grepl("Digester",sub_program_name) ~ "Dairy digester",
                                grepl("Manure",sub_program_name) ~ "Manure management",
                                TRUE ~ "—"),
         labeltext = case_when(grepl("Digester",sub_program_name) ~ paste0("<b>Name: </b>",facility_name,"</br>",
                                                                           "<b>Type: </b>",type,"</br>",
                                                                           "<b>Methane emissions: </b>",prettyNum(adjusted_total_methane, big.mark = ",")," metric tons","<br><br>",
                                                                           "Emissions adjusted to account for claimed reductions from a state-funded dairy digester project"),
                               grepl("Manure",sub_program_name) ~ paste0("<b>Name: </b>",facility_name,"</br>",
                                                                         "<b>Type: </b>",type,"</br>",
                                                                         "<b>Methane emissions: </b>",prettyNum(adjusted_total_methane, big.mark = ",")," metric tons","<br><br>",
                                                                         "Emissions adjusted to account for claimed reductions from a state-funded manure management project"),
                               TRUE ~ paste0("<b>Name: </b>",facility_name,"</br>",
                                             "<b>Type: </b>",type,"</br>",
                                             "<b>Methane emissions: </b>",prettyNum(adjusted_total_methane, big.mark = ",")," metric tons")),
         facility_name = gsub("-","—",facility_name))

ca_table <- ca_cattle_inc_mitigation %>%
  arrange(-adjusted_total_methane) %>%
  select(facility_name,type,mitigation,adjusted_total_methane)

# export processed data
write_csv(ca_cattle_inc_mitigation, "processed_data/ca_cattle.csv", na = "")
write_csv(ca_table, "processed_data/ca_table.csv", na = "")





