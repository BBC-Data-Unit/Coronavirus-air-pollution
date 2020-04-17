#the mutli-line comparative graph:

#expand out to index date:

daily_data_2019 <- daily_data_2019 %>% 
  pivot_wider(names_from = Station,
              values_from = Reading)

daily_data_2020 <- daily_data_2020 %>% 
  pivot_wider(names_from = Station,
              values_from = Reading)

#create matching index:
daily_data_2019$id <- seq.int(nrow(daily_data_2019))
daily_data_2020$id <- seq.int(nrow(daily_data_2019))

#melt down to join
daily_data_2019 <- daily_data_2019 %>% 
  pivot_longer(cols=2:163,
               names_to="Station", 
               values_to="Reading")

daily_data_2020 <- daily_data_2020 %>% 
  pivot_longer(cols=2:162,
               names_to="Station", 
               values_to="Reading")

#join on id and station to match data for comparative dates
daily_data_compare <- left_join(daily_data_2019, daily_data_2020, by=c("Station", "id"))

#rename columns for clarity:
daily_data_compare <- daily_data_compare %>% 
  rename(Reading_2019=Reading.x, Reading_2020=Reading.y,
         Date_2019=Date.x, Date_2020=Date.y, Index_date=id)

daily_avg_to_plot <- daily_data_compare %>% 
  select(Index_date, Station, Reading_2019, Reading_2020, Date_2020) %>%
  pivot_longer(cols = 3:4,
               names_to = "Year",
               values_to = "Reading")

daily_avg_to_plot <- daily_avg_to_plot %>% 
  group_by(Index_date, Year) %>% 
  summarise(avg_reading=mean(Reading, na.rm = T)) %>% 
  arrange(Index_date)


daily_data_2020_match <- daily_data_compare%>% 
  select(Date_2020, Index_date) %>% 
  distinct() %>% 
  filter(!is.na(Date_2020))

daily_avg_to_plot <- left_join(daily_avg_to_plot, daily_data_2020_match)

ggplot(daily_avg_to_plot, aes(x=Index_date, y=avg_reading, 
                                      group = Year, colour = Year))+
  geom_line() +
  geom_point()+
  labs(title = "Air pollution after lockdown", x="Day", y="Nitrogen dioxide levels")+
  theme(axis.text.x = element_text(angle=60, hjust=1)) 