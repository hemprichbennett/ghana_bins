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


# make a sheet with just the time-identified malaise trap captures, and the heath
# trap captures
night_time_families <- full_join(malaise_trap_arthropods, 
                                 family_trap_counts) %>%
  # select only the columns we care about
  select(Order, Family, Day, Night, Heath) %>%
  # remove any rows containing zero families from the two remaining trapping 
  # methods
  filter(sum(Day, Night, Heath)> 0)

write_csv(night_time_families,
          file = here('results', 'taxonomic_summaries', 'malaise_and_heath.csv')
          )

# Pest species ------------------------------------------------------------

# compare with a dataset downloaded from DEFRA, 2025-07-09
# https://planthealthportal.defra.gov.uk/pests-and-diseases/uk-plant-health-risk-register/downloadEntireRiskRegister.cfm
defra_pests <- read_csv(here('data', 'raw_data', 'Risk Register 09_July_2025 16_43_13.csv')) %>%
  janitor::clean_names() %>%
  rename(common_name = common_name_or_abbreviation,
         binomial_name = pest_name)

# add data from https://cipotato.org/riskatlasforafrica/
potato_pests <- read_csv(here('data', 'raw_data', 'cipotato_pests.csv')) %>%
  janitor::clean_names() %>%
  rename(major_hosts = group)

pest_sp <- bind_rows(defra_pests, potato_pests)

# combine it with our species occurrences
pest_detections <- inner_join(species_trap_counts, pest_sp, by = c('Species' = 'binomial_name'))

# add pest list from https://cipotato.org/riskatlasforafrica/table-of-contents/

# write file
write_csv(pest_detections, 
          file = here('results', 'taxonomic_summaries', 'pest_detections.csv'))


# make a tibble of only pest detections
pest_occurrence_tib <- big_in_tib %>%
  filter(Species %in% pest_sp$binomial_name)

t_leucotreta_detections <- big_in_tib %>%
  filter(Species == 'Thaumatotibia leucotreta') %>%
  group_by(date, lot) %>%
  count()
