# Shaivan Bhagat.        Project 1.         4926196

# load all essential libraries 
library(tidyverse)
library(lubridate)
library(readr)
library(rvest)
library(dplyr)
library(ggplot2)


# read in all the .csv files from the region SCHLD and store it as a data frame 
# read_csv2 uses ";" as the delim
df_SCHLD <- list.files(path = "Data/SCHLD_0001", full.names = TRUE) %>% 
  map_df(read_csv2)

# filter the SCHLD data frame to only contain the required variables
# add variable location so I know the observations are from which region
df_SCHLD_filtered <- df_SCHLD %>% 
  select(measurement_date, parameter_type, value_measured, value_calculated) %>% 
  mutate(
    location = "SCHLD"
  )

# read in the .txt file for the region VRSMR using read_csv2 which again uses ";" as a delim
VRSMR <- read_csv2("Data/VRSMR_0001_2000-2015.txt")

# filter the VRSMR data frame to create a new one containing only the required variables
# add variable location so I know the observations are from which region
VRSMR_filtered <- VRSMR %>% 
  select(measurement_date, parameter_type, value_measured, value_calculated) %>% 
  mutate(
    location = "VRSMR"
  )

# combine the two filtered data frames into a single one by joining the rows
combined_df <- rbind(df_SCHLD_filtered,VRSMR_filtered)

# removing all data with NA values in the combined data frame
# creating a new variable called observations which stores the rounded off value for the abundance
combined_df <- combined_df %>% 
  filter(!is.na(parameter_type)) %>% 
  filter(!is.na(value_measured)) %>% 
  filter(!is.na(value_calculated)) %>% 
  mutate(
    observations = floor(value_calculated/value_measured)
  ) 

# creating a data frame for diatoms from the combined data frame
# creating a new variable called year which stores just the year of the recorded observation making it easier for plotting
diatoms <- combined_df %>% 
  filter(
    parameter_type == "DIATM"
  )%>% 
  mutate(
    year = as.numeric(format(diatoms$measurement_date, "%Y"))
  ) 

# tidying the diatoms data frame by grouping the observations by year to contain the number of 
  # observations in each year and then using the logarithmic value of the yearly observations for 
    # ease of plotting
diatoms_tidy <- diatoms %>% 
  group_by(year) %>% 
  summarise(total_obs = sum(observations)) %>% 
  mutate(
    log_observations = log(total_obs)
  )

# creating a data frame for phytoplankton from the combined data frame
  # creating a new variable called year which stores just the year of the recorded observation making it easier for plotting
# for a few values for phytoplankton, data is 0. Thus filtering it to remove such observations
phytoplankton <- combined_df %>% 
  filter(
    parameter_type == "PHYTPT",
    value_measured != "0"
  ) %>% 
  mutate(
   year = as.numeric(format(phytoplankton$measurement_date, "%Y"))
  )

# tidying the phytoplankton data frame by grouping the observations by year to contain the number of 
  # observations in each year and then using the logarithmic value of the yearly observations for 
    # ease of plotting
phytoplankton_tidy <- phytoplankton %>% 
  group_by(year) %>% 
  summarise(total_obs = sum(observations)) %>% 
  mutate(
    log_observations = log(total_obs)
  )

# using the diatoms_tidy and phytoplankton_tidy data frames to plot a scatter plot
  # the year is plotted on the x-axis while the abundance(logarithmic scale) is plotted on the y-axis
# adding axes, graph, and legend title using labs()
# log of the observations are used for both data frames, since the relative abundance of phytoplankton is considerably
  # greater than that of diatoms and using actual observations messes up the scale of the graph
ggplot() +
  geom_point(data = diatoms_tidy, 
             mapping = aes(x = year, y = log_observations, color = "Diatoms"),
             color = "blue") +
  geom_smooth(data = diatoms_tidy, 
              mapping = aes(x = year, y = log_observations, color = "Diatoms"),
              se = FALSE) +
  geom_point(data = phytoplankton_tidy, 
             mapping = aes(x = year, y = log_observations, color = "Phytoplankton"),
             color = "orange") +
  geom_smooth(data = phytoplankton_tidy, 
              mapping = aes(x = year, y = log_observations, color = "Phytoplankton"),
              se = FALSE) + 
  labs(
    x = "Year",
    y = "Abundance (log)",
    title = "Abundance of diatoms and phytoplankton in Zeeland waters",
    color = "Legend"  
  ) +
  scale_color_manual(values = c("Diatoms" = "blue", "Phytoplankton" = "orange"))




# How has the abundance of diatoms and phytoplankton in Zeeland waters developed since the year 2000?

# From the graph produced, we can infer that the relative abundance of both diatoms and phytoplankton
  # has decreased between 2000-2015. 
# Phytoplankton and diatoms both had a massive bloom around 2001-2005 after which its abundance decreased considerably. 
# Another conclusion we can make is that the relative abundance of phytoplankton in Zeeland waters is signficantly
# higher than that of diatoms
# Since the abundance of diatoms and phytoplankton provides an indication of the ecological quality of aquatic ecosystems,
# such a decrease signifies that the ecological quality of water in Zeeland is deteriorating and is a signal for
# underlying environmental change
