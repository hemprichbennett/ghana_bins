library(tidyverse)

ec_individuals <- read_csv('data/earthcape_app_query/Individuals.csv')
ec_lots <- read_csv('data/earthcape_app_query/Lots.csv')
ec_transects <- read_csv('data/earthcape_app_query/Transects.csv')
ec_units <- read_csv('data/earthcape_app_query/Units.csv')

bold_organised <- read_csv('data/processed_data/our_organised_bold_data.csv')

str(bold_organised)
unique(bold_organised$field_id)
bold_organised$field_id %in% ec_individuals$`Unit ID`

ec_referenced <- bold_organised %>%
  mutate(in_individuals = field_id %in% ec_individuals$`Unit ID`,
         in_transects = field_id %in% ec_transects$Name)

unmatched <- ec_referenced %>%
  filter(in_individuals == F & in_transects == F)

# Annoyingly in the data there seems to be a mix of hyphens and underscores
# used as delimiters for transect names. They're mostly paired fine, but BOLD
# has samples with field_id '2-MA-NE-2' but earthcape cals it '2_MA_NE_2'

ec_referenced <- ec_referenced %>% 
  mutate(better_field_id = 
           # if the field_id is in neither individuals or transects
           ifelse(in_individuals == F & in_transects == F, 
                  # replace any hyphens with underscores
                  gsub('-', '_', field_id),
                  # else, return field_id unedited
                  field_id)
         )
str(ec_individuals)
str(ec_transects)
str(ec_lots)

for_individuals <- ec_individuals %>%
  left_join(ec_lots, by = c("Lot" = "Lot ID")) %>%
  left_join(ec_transects, by = c("Transect" = "Name")) %>%
  select(Lot, `Unit ID`, Transect, Latitude.y, Longitude.y,
         `Date Time Start`, `Date Time End`, Direction, Locality, Type)

for_transects <- ec_lots %>%
  rename(Lot = `Lot ID`) %>%
  left_join(ec_transects, by = c("Transect" = "Name")) %>%
  select(Lot, Transect, Latitude.y, Longitude.y,
         `Date Time Start`, `Date Time End`, Direction, Locality, Type)
