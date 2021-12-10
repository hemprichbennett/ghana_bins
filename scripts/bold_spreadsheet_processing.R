library(readxl)
library(purrr)
library(dplyr)
xl_path <- ('data/processed_data/bold_tidied.xlsx')


bold_data <- xl_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  # read all the sheets in as individual list items
  map(read_excel, path = xl_path) %>%
  # join  them all into a master sheet
  reduce(., left_join)
