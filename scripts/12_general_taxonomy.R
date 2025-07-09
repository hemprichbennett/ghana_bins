# script creating general taxonomic summaries, and comparing our dataset with
# data on known pest species

# Setup -------------------------------------------------------------------


library(tidyverse)
library(here)


# read in data
big_in_tib <- read_csv(, 
         file = here('data', 'processed_data', 
                     'bold_and_earthcape_combined.csv'))

malaise_trap_metadata <- read_csv(file = here('data', 'processed_data',
                                              'malaise_trap_metadata.csv'))


# make the most basic summary taxa x trap abundance tables ----------------



species_trap_counts <- big_in_tib %>%
  filter(!is.na(species), !is.na(type)) %>%
  group_by(order, family, species, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  # sort the rows by order, then family, then species
  arrange(order, family, species) %>%
  select(order, family, species, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)
  

genus_trap_counts <- big_in_tib %>%
  filter(!is.na(genus), !is.na(type)) %>%
  group_by(order, family, genus, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(order, family, genus) %>%
  select(order, family, genus, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

family_trap_counts <- big_in_tib %>%
  filter(!is.na(family), !is.na(type)) %>%
  group_by(order, family, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(order, family) %>%
  select(order, family, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)


# Make malaise trap temporal abundance table -----------------------------------



# Pest species ------------------------------------------------------------

# compare with a dataset downloaded from DEFRA, 2025-07-09
# https://planthealthportal.defra.gov.uk/pests-and-diseases/uk-plant-health-risk-register/downloadEntireRiskRegister.cfm
defra_pests <- read_csv(here('data', 'raw_data', 'Risk Register 09_July_2025 16_43_13.csv')) %>%
  janitor::clean_names()

# combine it with our species occurrences
pest_detections <- inner_join(species_trap_counts, defra_pests, by = c('species' = 'pest_name'))


# write file
write_csv(pest_detections, 
          file = here('results', 'pest_detections.csv'))

