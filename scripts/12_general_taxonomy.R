# script creating general taxonomic summaries, and comparing our dataset with
# data on known pest species

# Setup -------------------------------------------------------------------


library(tidyverse)
library(here)


# read in data
big_in_tib <- read_csv(, 
         file = here('data', 'processed_data', 
                     'bold_and_earthcape_combined.csv')) %>%
  rename(Order = order,
         Family = family,
         Genus = genus,
         Species = species)

malaise_trap_metadata <- read_csv(file = here('data', 'processed_data',
                                              'malaise_trap_metadata.csv'))


# make the most basic summary taxa x trap abundance tables ----------------



species_trap_counts <- big_in_tib %>%
  filter(!is.na(Species), !is.na(type)) %>%
  group_by(Order, Family, Species, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  # sort the rows by Order, then Family, then Species
  arrange(Order, Family, Species) %>%
  select(Order, Family, Species, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(species_trap_counts, 
          file = here('results', 'taxonomic_summaries', 'species_trap_counts.csv'))  

genus_trap_counts <- big_in_tib %>%
  filter(!is.na(Genus), !is.na(type)) %>%
  group_by(Order, Family, Genus, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order, Family, Genus) %>%
  select(Order, Family, Genus, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(genus_trap_counts, 
          file = here('results', 'taxonomic_summaries', 'genus_trap_counts.csv'))  

family_trap_counts <- big_in_tib %>%
  filter(!is.na(Family), !is.na(type)) %>%
  group_by(Order, Family, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order, Family) %>%
  select(Order, Family, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(family_trap_counts, 
          file = here('results', 'taxonomic_summaries', 'family_trap_counts.csv'))  

# Make malaise trap temporal abundance table -----------------------------------

# some malaise trap lots have no associated temporal data. Make a 
# summary of them
missing_temporal_information <- big_in_tib %>%
  filter(type == 'Malaise', !is.na(Family)) %>%
  left_join(malaise_trap_metadata) %>%
  relocate(lot, coarse_timing) %>%
  filter(is.na(coarse_timing))

unique(missing_temporal_information$lot)


# summarise the temporal data that we can
malaise_trap_arthropods <- big_in_tib %>%
  filter(type == 'Malaise', !is.na(Family)) %>%
  left_join(malaise_trap_metadata) %>%
  filter(!is.na(coarse_timing)) %>%
  group_by(Order, Family, coarse_timing) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = coarse_timing, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order, Family) %>%
  select(Order, Family, Day, Night)
  
write_csv(malaise_trap_arthropods, 
          file = here('results', 'taxonomic_summaries', 'malaise_trap_arthropods.csv'))

# Pest species ------------------------------------------------------------

# compare with a dataset downloaded from DEFRA, 2025-07-09
# https://planthealthportal.defra.gov.uk/pests-and-diseases/uk-plant-health-risk-register/downloadEntireRiskRegister.cfm
defra_pests <- read_csv(here('data', 'raw_data', 'Risk Register 09_July_2025 16_43_13.csv')) %>%
  janitor::clean_names()

# combine it with our species occurrences
pest_detections <- inner_join(species_trap_counts, defra_pests, by = c('Species' = 'pest_name'))


# write file
write_csv(pest_detections, 
          file = here('results', 'taxonomic_summaries', 'pest_detections.csv'))

