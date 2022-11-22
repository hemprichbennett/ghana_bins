library(tidyverse)

csvs <- list.files('data/earthcape_query', pattern = '.csv',
                   full.names = T)

# read in data manually (for-now)
extracts <- read_csv('data/earthcape_query/DNAextract.csv')
units <- read_csv('data/earthcape_query/Unit.csv')
locality_visits <- read_csv('data/earthcape_query/LocalityVisit.csv')

str(units)


bold_tidied <- read_csv('data/processed_data/our_organised_bold_data.csv')

# it looks like the data we have from earthcape relating
# to BOLD is a mix of metadata for sampling EVENTS, and samples.
identified_by_locality <- bold_tidied %>%
  filter(field_id %in% locality_visits$Name)

# the above are presumably samples which were sent in bulk
# and so are identifiable by the locality visit specifically,
# while the below are presumably samples which were sent in bulk

identified_by_unit <- bold_tidied %>%
  filter(field_id %in% units$UnitID)

# check if any samples have been identified by both 
n_both <- intersect(identified_by_locality$field_id, identified_by_unit$field_id)

if(is.na(n_both)){
  print('no samples identified by both methods, all is well')
}else{
    break('Samples have been identified by two types of metadata, abort!')
  }

# find samples which weren't in either
missing_field_ids <- bold_tidied %>%
  filter(!field_id %in% c(identified_by_locality$field_id, 
                          identified_by_unit$field_id)) %>%
  group_by(field_id) %>%
  summarise(n_samples = n())
  


with_unit_metadata <- bold_tidied %>%
  semi_join(units, by = c("field_id" = "UnitID")) %>%
  left_join(units, by = c("field_id" = "UnitID")) %>%
  write_csv(file = 'data/processed_data/bold_and_earthcape/with_unit.csv')


bold_tidied %>%
  semi_join(locality_visits, by = c("field_id" = "Name")) %>%
  left_join(locality_visits, by = c("field_id" = "Name")) %>%
  write_csv(file = 'data/processed_data/bold_and_earthcape/with_locality.csv')
