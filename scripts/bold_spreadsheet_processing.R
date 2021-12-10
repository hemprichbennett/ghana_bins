library(readxl)
library(purrr)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
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
