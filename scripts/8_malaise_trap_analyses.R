library(tidyverse)
library(here)

# read in data
malaise_trap_deployments <- read_csv(here('data', 'processed_data', 
                                          'temp_malaise_traps_edited_times.csv'))

all_arthropod_data <- read_csv(here('data', 'processed_data', 
                                    'bold_and_earthcape_combined.csv'))

# ids to check and perhaps modify the times of
bad_ids <- c(1000, 1011, 1099, 567, 577, 586, 604, 835)
bad_id_pattern <- paste(bad_ids, collapse = '\\.|^')
bad_id_pattern <- paste0('^', bad_id_pattern)
bad_id_pattern <- paste0(bad_id_pattern, '\\.')

# get traps which match the bad id pattern
traps_of_interest <- all_arthropod_data %>% 
  filter(grepl(bad_id_pattern, lot)) %>%
  select(class, order, family, genus, species, bin, lot)


malaise_trap_sample_counts <- all_arthropod_data %>%
  filter(type == 'Malaise') %>%
  group_by(lot) %>%
  summarise(nsamples_sequenced = n())


malaise_trap_bottle_counts <- malaise_trap_sample_counts %>%
  mutate(overall_lot = gsub('\\..+', '', lot)) %>%
  group_by(overall_lot) %>%
  summarise(n_bottles = n()) %>%
  group_by(n_bottles) %>%
  summarise(n = n())

ggplot(malaise_trap_bottle_counts, aes(x = n_bottles)) +
  geom_histogram()
