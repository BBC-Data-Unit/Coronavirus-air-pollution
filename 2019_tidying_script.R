.libPaths("D:/R/Libraries")
pacman::p_load(tidyverse, janitor, ggplot2, sf,rgdal, rgeos)

daily_data_2019_part_1 <- read_csv("Data downloads/compare_2019_to_05_04_v1.csv")
daily_data_2019_part_2 <- read_csv("Data downloads/compare_2019_to_05_04_v2.csv")

daily_data_2019 <- cbind(daily_data_2019_part_1, daily_data_2019_part_2)

rm(daily_data_2019_part_2, daily_data_2019_part_1)

#then select out columns for which row 1 has values x, y (include 'Time' if hourly)

daily_data_subset <- daily_data_2019[1,] %in% c("Date", "Nitrogen dioxide")
daily_data_2019 <- daily_data_2019[daily_data_subset]

#daily_data <- rbind(daily_data_2019, daily_data_2019)

#take 2019 as daily data and ram through:

daily_data_2019 <- daily_data_2019[-1,]
daily_data_2019 <- daily_data_2019 %>% 
  remove_empty("rows")
#removing surplus date column etc from cbind
daily_data_2019 <- daily_data_2019 %>% 
  select(-`X1.1`)
daily_data_2019 <- daily_data_2019 %>% 
  rename("Date"=X1)

#reshape:

daily_data_2019 <- daily_data_2019 %>% 
  pivot_longer(cols=2:163,
               names_to="Station", 
               values_to="Reading")

#daily_agg_mean:

#Reading column is still char, convert
daily_data_2019$Reading <- as.numeric(daily_data_2019$Reading)

daily_data_2019 <- daily_data_2019 %>% 
  filter(!Date=="End")

day_pivot_2019 <- daily_data_2019 %>% 
  group_by(Station) %>% 
  summarise(day_avg=mean(Reading, na.rm=T)) %>% 
  arrange(Station)

#join cmd (And comparison column creation)

day_pivot_joined <- full_join(day_pivot_2019, day_pivot_2020, by="Station")

day_pivot_joined <- day_pivot_joined %>%
  rename(day_avg_2019=day_avg.x, day_avg_2020=day_avg.y)

day_pivot_joined <- day_pivot_joined %>%
  mutate(percentage_difference=((day_avg_2020-day_avg_2019)/day_avg_2019)*100)

mean(day_pivot_joined$percentage_difference, na.rm = T)

write_csv("Station_2019_2020_readings_post_wfh.csv")


#now for lookups:
info_about_monitoring_stations <- read_csv("Defra info about monitoring stations.xlsx - Sheet1.csv")

info_about_monitoring_stations <- info_about_monitoring_stations %>% 
  rename(Station=site_name)

day_pivot_joined <- left_join(day_pivot_joined, info_about_monitoring_stations)

day_pivot_joined <- day_pivot_joined %>%
  select(-`AURN Pollutants Measured`)

#match lat-long to la, for GB regions

#convert points data to a sf, with geometry based on those two column names
#with an adjustment to make sure points are WGS84(proper long-lat) - crs must be matched in both dataframes

match_up  <- sf::st_as_sf(day_pivot_joined, coords=c("longitude","latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#read in geography of local authority boundaries as a shapefile - here ultra generalised
map <-  read_sf("LA_districts/Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp")
#transform that shapefile to have the same crs pattern for curvature of the earth as the points sf
#this conversion can be tricky to do later for some reason
map <- st_transform(map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#change the shp file into a sf (they are different formats but must be sf for later steps)
map <- sf::st_as_sf(map)
#select out the la names and geometry
map <- map %>% 
  select(lad17nm,geometry)
#check str of map is sound
str(map) 

#plot both sf as simultaenous layers to visually check overlap
plot(map, reset=FALSE)
plot(match_up, add=TRUE, reset=FALSE, pch=16, col="red", cex=1.5)

#check st_intersects returns an integer (if empty, then no overlap working)
int <- sf::st_intersects(map, match_up)
int

#create a new object of the points data matched onto the area for which it intersects in the la layer
stations_match <- match_up %>% mutate(
  intersection = as.integer(st_intersects(match_up , map))
  , area = map$lad17nm[intersection])

#quick view of tibble
stations_match

#are the remaining points all in NI?

stations_unmatched <- stations_match %>% 
  filter(is.na(area))

#yes they are

#repair to df to allow tidyverse functions (still has hangover sf properties)
stations_match <- as.data.frame(stations_match)
stations_match <- stations_match %>% 
  select(-intersection) %>% 
  rename(la=area)

#fix known issues in la lookup before match

BBC_England_Wales_regions_radio_local_authority_lookup <- read_csv("BBC regions_radio_local authority lookup - Sheet1.csv")

names(BBC_England_Wales_regions_radio_local_authority_lookup)
BBC_England_Wales_regions_radio_local_authority_lookup <- BBC_England_Wales_regions_radio_local_authority_lookup %>% 
  rename(la=LA_name)

stations_match$la <- gsub("Bristol, City of", "Bristol", stations_match$la)

day_pivot_joined_matched <- left_join(stations_match, BBC_England_Wales_regions_radio_local_authority_lookup)


#repair Northern Ireland non-matches (temp fix)
#find non-matched(all NI), insert NI as govt region, then replace
day_pivot_joined_NI <- day_pivot_joined_matched %>% 
  filter(is.na(la)) %>%
  mutate(Northern_Ireland="Northern Ireland") %>% 
  select(-govt_region) %>% 
  rename(govt_region=Northern_Ireland)

day_pivot_joined_matched <- day_pivot_joined_matched %>% 
  filter(!is.na(la))

day_pivot_joined_matched <- rbind(day_pivot_joined_NI, day_pivot_joined_matched)

#Scotland fix:

day_pivot_joined_matched_Scot <- day_pivot_joined_matched %>% 
  filter(is.na(govt_region)) %>%
  mutate(Scotland="Scotland") %>% 
  select(-govt_region) %>% 
  rename(govt_region=Scotland)

day_pivot_joined_matched <- day_pivot_joined_matched %>% 
  filter(!is.na(govt_region))

day_pivot_joined_matched <- rbind(day_pivot_joined_matched_Scot, day_pivot_joined_matched)

#write out and check with an eye/clean:

names(day_pivot_joined_matched)
day_pivot_joined_matched <- day_pivot_joined_matched %>% 
  select(8,14,7,1:4,5,6,9:11)

write_csv(day_pivot_joined_matched, "air_pollution_compare_radio.csv")