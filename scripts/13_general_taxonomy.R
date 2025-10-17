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
# this table doesn't make it into the paper, but values of it are used in-text
write_csv(species_trap_counts, 
          file = here('results', 'species_trap_counts.csv'))  



family_trap_counts <- big_in_tib %>%
  filter(!is.na(Family), !is.na(type)) %>%
  group_by(Order, Family, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order, Family) %>%
  select(Order, Family, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(family_trap_counts, 
          file = here('results', 'si_tbl_2_family_trap_counts.csv')) 


order_trap_counts <- big_in_tib %>%
  filter(!is.na(Order), !is.na(type)) %>%
  group_by(Order, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order) %>%
  select(Order, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(order_trap_counts, 
          file = here('results', 'si_tbl_1_order_trap_counts.csv')) 

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
          file = here('results', 'si_tbl_6_malaise_trap_arthropods.csv'))


# Pest species ------------------------------------------------------------

# compare with a dataset downloaded from DEFRA, 2025-07-09
# https://planthealthportal.defra.gov.uk/pests-and-diseases/uk-plant-health-risk-register/downloadEntireRiskRegister.cfm
defra_pests <- read_csv(here('data', 'raw_data', 'pest_taxa', 'Risk Register 09_July_2025 16_43_13.csv')) %>%
  janitor::clean_names() %>%
  rename(common_name = common_name_or_abbreviation,
         binomial_name = pest_name)

# add data from https://cipotato.org/riskatlasforafrica/ and a few other sources
potato_pests <- read_csv(here('data', 'raw_data', 'pest_taxa', 'misc_pests.csv')) %>%
  janitor::clean_names() %>%
  rename(major_hosts = group)

pest_sp <- bind_rows(defra_pests, potato_pests)

# combine it with our species occurrences
pest_detections <- inner_join(species_trap_counts, pest_sp, by = c('Species' = 'binomial_name'))

# add pest list from https://cipotato.org/riskatlasforafrica/table-of-contents/

# write file
write_csv(pest_detections, 
          file = here('results', 'si_tbl_3_pest_detections.csv'))





# make a tibble of only pest detections
pest_occurrence_tib <- big_in_tib %>%
  filter(Species %in% pest_sp$binomial_name)

t_leucotreta_detections <- big_in_tib %>%
  filter(Species == 'Thaumatotibia leucotreta') %>%
  group_by(date, lot) %>%
  count()

pest_occurrence_tib

percent_samples_crop_pests <- nrow(pest_occurrence_tib) / nrow(big_in_tib) * 100

percent_bins_crop_pests <- length(unique(pest_occurrence_tib$bin)) / 
  length(unique(big_in_tib$bin)) * 100

# possible_pest taxa ------------------------------------------------------





possible_bloodfeeding_families <- c('Ceratopogonidae',
                                    'Culicidae', 
                                    'Rhagionidae',
                                    'Simuliidae', 
                                     'Tabanidae'
                                    )

possible_bloodfeeders <- big_in_tib %>%
  filter(Family %in% possible_bloodfeeding_families) %>%
  group_by(Order, Family) %>%
  summarise(n_samples = n(),
            nbins = length(unique(bin))) 

percent_poss_bloodfeeders_bins <- sum(possible_bloodfeeders$nbins / 
                                   length(unique(big_in_tib$bin)) * 100)

percent_poss_bloodfeeders_abundance <- sum(possible_bloodfeeders$n_samples /
                                        nrow(big_in_tib) * 100)


poss_bloodfeeder_table <- big_in_tib %>%
  filter(Family %in% possible_bloodfeeding_families) %>%
  group_by(Order, Family, type) %>%
  summarise(n_samples = n()) %>%
  pivot_wider(names_from = type, values_from = n_samples, 
              values_fill = list(n_samples = 0)) %>%
  arrange(Order, Family) %>%
  select(Order, Family, Cdc, Heath, Malaise, Pitfall, `Yellow Pan`)

write_csv(poss_bloodfeeder_table, 
          file = here('results', 'si_tbl_4_bloodfeeder_detections.csv'))

prob_bloodfeeding_families <- c('Culicidae', 'Simuliidae', 'Tabanidae')

prob_bloodfeeders <- possible_bloodfeeders %>% 
  filter(Family %in% prob_bloodfeeding_families)

percent_prob_bloodfeeders_bins <- sum(prob_bloodfeeders$nbins / 
                                        length(unique(big_in_tib$bin)) * 100)

percent_prob_bloodfeeders_abundance <- sum(prob_bloodfeeders$n_samples /
                                             nrow(big_in_tib) * 100)



# How many samples were assigned binomials --------------------------------

species_status <- big_in_tib %>%
  mutate(has_binomial = !is.na(Species)) %>%
  select(bin, has_binomial)

n_bins_with_sp <- species_status %>%
  filter(has_binomial == T) %>%
  pull(bin) %>%
  unique() %>%
  length()


percent_samples_with_sp <- nrow(filter(species_status, has_binomial == T)) / nrow(big_in_tib) * 100
