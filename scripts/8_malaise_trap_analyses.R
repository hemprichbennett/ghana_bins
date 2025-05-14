library(tidyverse)
library(here)
library(hms)

# read in data
malaise_trap_deployments <- read_csv(here('data', 'processed_data', 
                                          'temp_malaise_traps_edited_times.csv')) %>%
  janitor::clean_names() %>%
  select(lot_id, bottle_start_time, bottle_end_time) %>%
  rename(lot = lot_id,
         bottle_start_datetime = bottle_start_time,
         bottle_end_datetime = bottle_end_time) %>%
  mutate(bottle_start_time = as_hms(bottle_start_datetime),
         bottle_end_time = as_hms(bottle_end_datetime),
         overall_lot = gsub('\\..+', '', lot),
         # make a column giving the temporal classification of a bottle. This
         # is necessary as lots of bottles were started at e.g. 10AM, and only
         # ran until midday. They are to be pooled with samples from the 
         # following day which ran from 6AM until e.g. 10AM, to give a full
         # 6-hour window. So those initial 10-12 bottles are honorary parts of the
         # 6AM bottles
         start_classification = ifelse(bottle_start_time < as_hms('06:00:00'), 
                                      "00:00:00", as.character(bottle_start_time)),
         start_classification = ifelse(bottle_start_time < as_hms('12:00:00') & bottle_start_time > as_hms("06:00:00"), 
                                       "06:00:00", start_classification),
         start_classification = ifelse(bottle_start_time < as_hms('18:00:00') & bottle_start_time > as_hms("12:00:00"), 
                                       "12:00:00", start_classification),
         start_classification = ifelse(bottle_start_time > as_hms("18:00:00"), 
                                       "18:00:00", start_classification),
         start_classification = ifelse(bottle_start_time < as_hms('06:00:00'), 
                                       "00:00:00", start_classification)
         )

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
malaise_trap_data_to_use <- all_arthropod_data %>%
  filter(type == 'Malaise') %>%
  mutate(overall_lot = gsub('\\..+', '', lot)) %>%
  filter(overall_lot %in% lots_to_use$overall_lot) %>%
  # add the temporally wrangled data from above
  left_join(malaise_trap_deployments)


# we now need a column giving a factor for if the bottle was deployed midnight-6AM,
# 6AM - 12PM, 12PM - 6PM, 6PM - Midnight. We'll also need to collapse some of the
# bottles together, as there are some that were deployed late morning-midday, with another bottle being
# deployed 6AM - late morning the next day


malaise_trap_data_to_use %>%
  count(start_classification)

missing_values <- malaise_trap_data_to_use %>%
  filter(is.na(start_classification)) %>%
  select(lot, overall_lot, start_classification, bottle_start_time, bottle_start_datetime, bottle_end_time, bottle_end_datetime)



trap_abundance <- malaise_trap_data_to_use %>%
  # remove any bottles with no time found (the ones in missing_values)
  filter(!is.na(start_classification)) %>%
  group_by(overall_lot, start_classification) %>%
  summarise(`Number of insects captured` = n(),
            `Number of unique BINs` = length(unique(bin))) %>%
  pivot_longer(cols = c(`Number of insects captured`, `Number of unique BINs`), names_to = 'variable_type', 
               values_to = 'variable_result')


# Basic boxplot of results ------------------------------------------------


ggplot(trap_abundance, aes(x = start_classification, y = variable_result))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(. ~variable_type, scales = 'free_y', 
             # put the facet labels on the left of the subplots
             switch = 'left')+
  # put the facet labels outside of the axis markers, to serve as axis labels
  theme(strip.placement = "outside",
        # make the facet labels have a white background
        strip.background =element_rect(fill="white"),
        # remove redundant y-axis label
        axis.title.y = element_blank()
        )+
  xlab('Bottle start time')

library(nlme)

test_model <- nlme::lme(variable_result ~ start_classification, 
          random = ~ start_classification | overall_lot,
          data = filter(trap_abundance, 
                        variable_type == 'Number of insects captured'))

summary(test_model)

