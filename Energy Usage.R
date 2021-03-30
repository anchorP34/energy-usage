# Energy Consumption Project

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

energy.data <- read_excel('Energy Usage.xlsx')

energy.data[,'Date'] <- mdy(energy.data$Date)
for (col in colnames(energy.data)[2:5]){
  energy.data[col] <- lapply(energy.data[col], as.numeric)
}

colnames(energy.data) <- c('Date','On_Peak','Super_Off_Peak','Off_Peak','Weather')

# Additional Info about dates
energy.data['Day_Of_Week'] <-wday(energy.data$Date, label = T)
energy.data['Month'] <-month(energy.data$Date, label = T)

# Unpivot data 
vertical.data <- energy.data %>%   
  select('Date','On_Peak','Super_Off_Peak','Off_Peak') %>% 
  pivot_longer(-Date, names_to = 'Energy_Type', values_to = 'Energy_Amount')

# Line Plot based on energy type
ggplot(vertical.data, mapping = aes(Date,Energy_Amount, color = Energy_Type)) +
  geom_line()+ 
  theme_bw()


# Stacked Bar Chart
ggplot(vertical.data, mapping = aes(Date,Energy_Amount, fill = Energy_Type)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_bw()


# Fill Bar Chart
ggplot(vertical.data, mapping = aes(Date,Energy_Amount, fill = Energy_Type)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_bw()


# Fill Percentage Area Chart
ggplot(vertical.data, mapping = aes(Date,Energy_Amount, fill = Energy_Type)) +
  geom_area(position = "fill", stat = "identity")+ 
  theme_bw()

# San Diego Weather Box Plot
energy.data %>% 
  select(Month,Weather) %>% 
  filter(Month != 'Dec') %>% 
  ggplot(., mapping = aes(x = Month, y = Weather)) +
  geom_boxplot()


# San Diego Weather Box Plot
energy.data %>% 
  select(Month,Weather) %>% 
  filter(Month != 'Dec') %>% 
  ggplot(., mapping = aes(x = Month, y = Weather)) +
  geom_boxplot()


energy.data %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  mutate(Total_Energy  = Super_Off_Peak + Off_Peak + On_Peak) %>% 
  select(Day_Of_Week, Total_Energy) %>% 
  ggplot(., mapping = aes(x = Day_Of_Week, y = Total_Energy)) +
  geom_boxplot()

# Energy Amounts by Month and Energy Type
energy.data %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  select(Month, On_Peak, Super_Off_Peak, Off_Peak) %>% 
  pivot_longer(-Month, names_to = 'Energy_Type', values_to = 'Energy_Amount') %>% 
  filter(Month != 'Dec') %>% 
  ggplot(., mapping = aes(x = Month, y = Energy_Amount, color = Energy_Type)) +
  geom_boxplot()



# Energy Amounts by Day of Week and Energy Type
energy.data %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  select(Day_Of_Week, On_Peak, Super_Off_Peak, Off_Peak) %>% 
  pivot_longer(-Day_Of_Week, names_to = 'Energy_Type', values_to = 'Energy_Amount') %>% 
  ggplot(., mapping = aes(x = Day_Of_Week, y = Energy_Amount, color = Energy_Type)) +
  geom_boxplot()


# Total Energy by Weather
energy.data %>% 
  mutate(Total_Energy = Super_Off_Peak + Off_Peak + On_Peak) %>% 
  select(Date, Total_Energy, Weather) %>% 
  group_by(Date, Weather) %>% 
  summarise(Total_Energy = sum(Total_Energy)) %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  ggplot(mapping = aes(x = Weather, y = Total_Energy)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

# Total Energy by Weather with 70 degree cutoff
energy.data %>% 
  mutate(Total_Energy = Super_Off_Peak + Off_Peak + On_Peak) %>% 
  select(Date, Total_Energy, Weather) %>% 
  group_by(Date, Weather) %>% 
  summarise(Total_Energy = sum(Total_Energy)) %>% 
  mutate(Seventy_Group = if (Weather < 70) "Yes" else "No") %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  ggplot(mapping = aes(x = Weather, y = Total_Energy, color = Seventy_Group)) +
  geom_point() +
  geom_smooth( formula = y ~ x)


# Energy by Weather, colored by Energy Type
energy.data %>%   
  select('Date','On_Peak','Super_Off_Peak','Off_Peak','Weather') %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  pivot_longer(-c(Date, Weather), names_to = 'Energy_Type', values_to = 'Energy_Amount') %>% 
  ggplot(mapping = aes(x = Weather, y = Energy_Amount ,color = Energy_Type)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F)




# Correlation Matrix between Energy and Weather
energy.data %>% 
  mutate(Total_Energy = Super_Off_Peak + Off_Peak + On_Peak) %>% 
  select(Date, Total_Energy, Weather) %>% 
  group_by(Date, Weather) %>% 
  summarise(Total_Energy = sum(Total_Energy)) %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
  ungroup(Date) %>% 
  select(Weather, Total_Energy) %>% 
  cor()


# Stacked Bar Chart
vertical.data %>% 
  filter(Date >= '2020-01-01' & !(Date >= '2020-05-05' & Date <= '2020-05-30')) %>% 
ggplot( mapping = aes(Date,Energy_Amount, fill = Energy_Type)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_bw()

# Stacked Bar Chart
vertical.data %>% 
  filter(Date >= '2020-05-30') %>% 
  ggplot( mapping = aes(Date,Energy_Amount, fill = Energy_Type)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_bw()


