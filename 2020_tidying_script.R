#run this script before 2019 (which includes formatting)
.libPaths("D:/R/Libraries")
pacman::p_load(tidyverse, janitor, ggplot2, sf,rgdal, rgeos)

daily_data_2020_part_1 <- read_csv("Data downloads/updated_data_to_05_04_2020_1.csv")
daily_data_2020_part_2 <- read_csv("Data downloads/updated_data_to_05_04_2020_2.csv")

daily_data_2020 <- cbind(daily_data_2020_part_1, daily_data_2020_part_2)

rm(daily_data_2020_part_2, daily_data_2020_part_1)

#then select out columns for which row 1 has values x, y (include 'Time' if hourly)
daily_data_subset <- daily_data_2020[1,] %in% c("Date", "Nitrogen dioxide")
daily_data_2020 <- daily_data_2020[daily_data_subset]

#take 2020 as daily data and ram through:

daily_data_2020 <- daily_data_2020[-1,]
daily_data_2020 <- daily_data_2020 %>% 
  remove_empty("rows")
#removing surplus date column etc from cbind
daily_data_2020 <- daily_data_2020 %>% 
  select(-`X1.1`)
daily_data_2020 <- daily_data_2020 %>% 
  rename("Date"=X1)

#reshape:

daily_data_2020 <- daily_data_2020 %>% 
  pivot_longer(cols=2:162,
               names_to="Station", 
               values_to="Reading")

#daily_agg_mean:

#Reading column is still char, convert
daily_data_2020$Reading <- as.numeric(daily_data_2020$Reading)

daily_data_2020 <- daily_data_2020 %>% 
  filter(!Date=="End")

day_pivot_2020 <- daily_data_2020 %>% 
  group_by(Station) %>% 
  summarise(day_avg=mean(Reading, na.rm=T)) %>% 
  arrange(Station)

#join cmd in 2019 code

