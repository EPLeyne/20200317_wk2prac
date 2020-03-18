library(tidyverse)

BOM_data <- read_csv('data/BOM_data.csv')
BOM_stations <- read_csv('data/BOM_stations.csv')

#Q1
Q1 <- BOM_data %>% 
  select(c('Station_number', 'Temp_min_max', 'Rainfall')) %>% 
  separate(Temp_min_max, c('min', 'max'), sep = '/') %>% 
  filter(min != '-', max != '-', Rainfall != '-') %>% 
  group_by(Station_number) %>% 
  summarise(n_days = n())

#Q2 Which month saw the lowest average daily temperature difference?

Q2 <- BOM_data %>% 
  select(c('Station_number', 'Month', 'Temp_min_max', 'Rainfall')) %>% 
  separate(Temp_min_max, c('min', 'max'), sep = '/') %>% 
  filter(min != '-', max != '-', Rainfall != '-') %>%  
  mutate(t_diff = (as.numeric(max) - as.numeric(min))) %>% 
  group_by(Month) %>% 
  summarise(mean_diff = mean(t_diff)) %>% 
  filter(mean_diff == min(mean_diff))

#Q3 Which state saw the lowest average daily temperature difference?

tidy_stations <- BOM_stations %>% 
  gather(-info, key ='StationID',value = 'measurement') %>% 
  spread(info,measurement)

