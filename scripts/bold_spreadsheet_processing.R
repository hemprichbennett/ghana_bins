# Code for reading in and formatting basic data from BOLD
# Setup -------------------------------------------------------------------

# load packages
library(readxl)
library(purrr)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(ggplot2)


# Read in the data

# this assumes that the input data has had the redundant first two rows
# (eyeroll) of the raw data on each spreadsheet removed
xl_path <- ('data/processed_data/bold_tidied.xlsx')


bold_data <- xl_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  # read all the sheets in as individual list items
  map(read_excel, path = xl_path) %>%
  # join  them all into a master sheet
  reduce(., left_join) %>%
  clean_names() %>%
  # make the dates usable, rather than a string
  mutate(collection_date = dmy(collection_date))



# Basic plotting ----------------------------------------------------------



# make a simple plot of the number of SAMPLES over time
collection_date_histogram <- ggplot(bold_data, aes(x = collection_date)) + 
  geom_histogram() +
  theme_bw() +
  xlab('Collection date') + 
  ylab('Number of samples sequenced by December 10th 2021')
# show it
collection_date_histogram

# save it
ggsave('figures/collection_date_histogram.jpeg', collection_date_histogram)


