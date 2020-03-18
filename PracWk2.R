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
  gather(-info, key ='Station_number',value = 'measurement') %>% 
  spread(info,measurement)

observationsQ3 <- BOM_data %>% 
  select(c('Station_number', 'Temp_min_max', 'Rainfall')) %>% 
  separate(Temp_min_max, c('min', 'max'), sep = '/') %>% 
  filter(min != '-', max != '-', Rainfall != '-')
observationsQ3$Station_number <- as.character(observationsQ3$Station_number)

Q3 <- inner_join(observationsQ3, tidy_stations, by = "Station_number") %>% 
  select(c('min','max','state')) %>% 
  group_by(state) %>% 
  summarise(mean_tdiff = mean((as.numeric(max) - as.numeric(min)))) %>% 
  filter(mean_tdiff == min(mean_tdiff))

#Q4 Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?
observationsQ4 <- BOM_data %>% 
  select(c('Station_number', 'Solar_exposure')) %>% 
  filter(Solar_exposure != '-')
observationsQ4$Station_number <- as.character(observationsQ4$Station_number)

Q4 <- inner_join(observationsQ4, tidy_stations, by = "Station_number") %>% 
  select(c('lon','Solar_exposure')) %>% 
  group_by(lon) %>%
  summarise(meanSolarExp = mean(as.numeric(Solar_exposure))) %>% 
  filter(lon == max(lon) | lon == min(lon))

