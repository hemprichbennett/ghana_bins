# In this script we explore the geographic origin of the non-'unique' 
# BINs in our dataset


library(tidyverse)

oursamples_df <- read_csv('data/processed_data/bold_data_with_availability.csv')

