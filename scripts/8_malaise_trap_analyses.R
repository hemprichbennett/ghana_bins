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


# get the number of different bottles per lot
lot_bottle_counts <- all_arthropod_data %>%
  filter(type == 'Malaise') %>%
  select(lot) %>%
  # make a column with just the overall lot number (e.g. '95' for 95.0 and 95.1)
  mutate(overall_lot = gsub('\\..+', '', lot)) %>%
  group_by(overall_lot) %>%
  summarise(n_sublots = length(unique(lot)))


# plot the number of sublots per lot
ggplot(lot_bottle_counts, aes(x = n_sublots)) + 
  geom_histogram() +
  theme_bw()+
  xlab('Number of bottles per lot')+
  ylab('Count')

# get the ids of lots with 4 or more bottles
lots_to_use <- lot_bottle_counts %>%
  filter(n_sublots >= 4)


# filter all malaise trap data to retain only bottles from those lots
malaise_trap_data_touse <- all_arthropod_data %>%
  filter(type == 'Malaise') %>%
  mutate(overall_lot = gsub('\\..+', '', lot)) %>%
  filter(overall_lot %in% lots_to_use$overall_lot)


# we now need a column giving a factor for if the bottle was deployed midnight-6AM,
# 6AM - 12PM, 12PM - 6PM, 6PM - Midnight. We'll also need to collapse some of the
# bottles together, as there are some that were deployed late morning-midday, with another bottle being
# deployed 6AM - late morning the next day
