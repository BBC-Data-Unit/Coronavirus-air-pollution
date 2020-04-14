.libPaths("D:/R/Libraries")
pacman::p_load(tidyverse, janitor, ggplot2, sf,rgdal, rgeos)
#import, collate and tidy:

#read in both parts of monitoring data (DEFRA portal maxes out on number of columns allowed)
#NB that R can struggle to read in data 'as is' in downloaded format
#remove first two header rows in Excel before import to help it read csv properly:

daily_data_part_1 <- read_csv("daily_mean_mar_19_to_20_03.csv")
daily_data_part_2 <- read_csv("daily_mean_mar_19_to_20_03_part_2.csv")

daily_data <- cbind(daily_data_part_1, daily_data_part_2)

#then select out columns for which row 1 has values x, y (include 'Time' if hourly)
daily_data_subset <- daily_data[1,] %in% c("Date", "Nitrogen dioxide")
daily_data <- daily_data[daily_data_subset]

#basic tidying
daily_data <- daily_data[-1,]
daily_data <- daily_data %>% 
  remove_empty("rows")
#removing surplus date column etc from cbind
daily_data <- daily_data %>% 
  select(-`X1.1`)
daily_data <- daily_data %>% 
  rename("Date"=X1)

names(daily_data)

#reshape:

daily_data <- daily_data %>% 
  pivot_longer(cols=2:165,
               names_to="Station", 
               values_to="Reading")

#daily_agg_mean:

#Reading column is still char, convert
daily_data$Reading <- as.numeric(daily_data$Reading)

daily_data <- daily_data %>% 
  filter(!Date=="End")

day_pivot <- daily_data %>% 
  group_by(Date) %>% 
  summarise(day_avg=mean(Reading, na.rm=T)) %>% 
  arrange(Date)


ggplot(day_pivot) +
  aes(x = Date, weight = day_avg) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#compare this march with last march
march_compare <- daily_data %>% 
  filter(str_detect(Date, "/03/"))

#create function to mimic RIGHT() function in Excel to extract year value from date
#(lubridate requires yyyy/mm/dd format or similar to auto-parse, this is quicker here)
substrRight <- function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) } 

march_compare <- march_compare %>% 
  mutate(year=substrRight(Date,4))

march_compare_agg <- march_compare %>% 
  group_by(Date, year) %>% 
  summarise(day_avg=sum(Reading, na.rm=T)) %>% 
  arrange(Date)

marches <- ggplot(march_compare_agg, aes(x=Date, y=day_avg)) +
  facet_wrap(~year)+
  geom_bar(stat="identity",
         position="identity",
         fill="#1380A1")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
marches

marches <- ggplot(march_compare, aes(x=Date, y=Reading)) +
  facet_wrap(~year)+
  geom_boxplot(fill="#1380A1")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
marches

#argh facets aren't clean for some reason. Filter and plot separately:

march_19 <- march_compare %>% 
  filter(year==2019)
march_20 <- march_compare %>% 
  filter(year==2020)

plot_march_19 <- ggplot(march_19, aes(x=Date, y=Reading))+
  geom_boxplot(fill="#1380A1")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot_march_19

plot_march_20 <- ggplot(march_20, aes(x=Date, y=Reading))+
  geom_boxplot(fill="#1380A1")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot_march_20

#add in lookups for region/la/environment type:

info_about_monitoring_stations <- read_csv("Defra info about monitoring stations.xlsx - Sheet1.csv")

names(info_about_monitoring_stations)
info_about_monitoring_stations <- info_about_monitoring_stations %>% 
  rename(Station=site_name)

daily_data <- left_join(daily_data, info_about_monitoring_stations)

daily_data <- daily_data %>%
  select(-`AURN Pollutants Measured`)


#convert that points data to a sf, with geometry based on those two column names
#with an adjustment to make sure points are WGS84(proper long-lat) - crs must be matched in both dataframes

match_up  <- sf::st_as_sf(daily_data, coords=c("longitude","latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

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

daily_data_matched <- left_join(stations_match, BBC_England_Wales_regions_radio_local_authority_lookup)

#repair Northern Ireland non-matches (temp fix)
daily_data_NI <- daily_data_matched %>% 
  filter(is.na(la)) %>%
  mutate(Northern_Ireland="Northern Ireland") %>% 
  select(-govt_region) %>% 
  rename(govt_region=Northern_Ireland)

daily_data_matched <- daily_data_matched %>% 
  filter(!is.na(la))

daily_data_matched <- rbind(daily_data_NI, daily_data_matched)

#temporarily remove radio to leave regions for England/Wales/NI note:

daily_data_small <- daily_data_matched %>% 
  select(-bbc_radio_station,-bbc_online, -bbc_region, -bbc_region_uses_gov_name, -Notes)
  