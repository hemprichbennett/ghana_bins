library(tidyverse)
library(here)
library(hms)
library(nlme)
library(emmeans)
library(ggsignif)
library(gridExtra)

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
         ) %>%
  # class bottles as either 'day' or 'night', rather than as finely as possible
  mutate(coarse_timing = ifelse(start_classification %in% c('00:00:00', "18:00:00"),
                                'Night', 'Day'))


# save malaise trap metadata, as we'll want to use it again in the next script
write_csv(malaise_trap_deployments, file = here('data', 'processed_data',
                                                'malaise_trap_metadata.csv'))

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


#the number of fully-sequenced bottles
lot_bottle_counts %>% filter(n_sublots >=4) %>% nrow()

# the number of malaise lots
nrow(lot_bottle_counts)

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



trap_insect_numbers <- malaise_trap_data_to_use %>%
  # remove any bottles with no time found (the ones in missing_values)
  filter(!is.na(start_classification)) %>%
  group_by(overall_lot, coarse_timing,start_classification) %>%
  summarise(`Number of insects captured` = n(),
            `Number of unique BINs` = length(unique(bin))) %>%
  pivot_longer(cols = c(`Number of insects captured`, `Number of unique BINs`), names_to = 'variable_type', 
               values_to = 'variable_result')



# Models ------------------------------------------------------------------

trap_insect_numbers %>%
  filter(variable_type == 'Number of insects captured') %>%
  group_by(coarse_timing) %>%
  summarise(avg_abundance = mean(variable_result),
            sd_abundance = sd(variable_result))

trap_insect_numbers %>%
  filter(variable_type == 'Number of unique BINs') %>%
  group_by(coarse_timing) %>%
  summarise(avg_bin_richness = mean(variable_result),
            sd_bin_richness = sd(variable_result))

## Abundance model ---------------------------------------------------------



precise_abundance_model <- nlme::lme(variable_result ~ start_classification, 
          random = ~ 1 | overall_lot,
          data = filter(trap_insect_numbers, 
                        variable_type == 'Number of insects captured'))
          

summary(precise_abundance_model)
intervals(precise_abundance_model)
anova(precise_abundance_model)

# the significance of each pairwise comparison
emmeans(precise_abundance_model, pairwise ~ start_classification)
# the only significant pairs for precise_abundance_model were 
# 00:00:00 - 12:00:00  p=0.0097
# 12:00:00 - 18:00:00  p=0.0064

# now model it using just day/night
coarse_abundance_model <- nlme::lme(variable_result ~ coarse_timing, 
                                     random = ~ 1 | overall_lot,
                                     data = filter(trap_insect_numbers, 
                                                   variable_type == 'Number of insects captured'))


summary(coarse_abundance_model)
intervals(coarse_abundance_model)
anova(coarse_abundance_model)

abundance_boxplot <- ggplot(filter(trap_insect_numbers, variable_type == 'Number of insects captured'), 
                            aes(x = coarse_timing, y = variable_result))+
  geom_boxplot()+
  theme_bw()+
  scale_y_log10()+
  ylab('Number of insects captured')+
  xlab('Bottle deployment time')+
  theme(text = element_text(size = 15))+ 
  labs(tag = 'A')
abundance_boxplot

ggsave(filename = here('figures', 'fig_6_abundance_boxplot.png'),
       abundance_boxplot,
       dpi = 600)

## BIN model ---------------------------------------------------------------

precise_bin_model <- nlme::lme(variable_result ~ start_classification, 
                             random = ~ 1 | overall_lot,
                             data = filter(trap_insect_numbers, 
                                           variable_type == 'Number of unique BINs'))


summary(precise_bin_model)
anova(precise_bin_model)


emmeans(precise_bin_model, pairwise ~ start_classification)
# the only significant pairs for precise_bin_model were 
# 00:00:00 - 12:00:00  p=0.0145
# 12:00:00 - 18:00:00  p=0.0056

# now model it for day/night only
coarse_bin_model <- nlme::lme(variable_result ~ coarse_timing, 
                               random = ~ 1 | overall_lot,
                               data = filter(trap_insect_numbers, 
                                             variable_type == 'Number of unique BINs'))


summary(coarse_bin_model)
anova(coarse_bin_model)
emmeans(coarse_bin_model, pairwise ~ coarse_timing)


bin_boxplot <- ggplot(filter(trap_insect_numbers, variable_type == 'Number of unique BINs'), 
                      aes(x = coarse_timing, y = variable_result))+
  geom_boxplot()+
  theme_bw()+
  ylab('Number of unique BINs')+
  xlab('Bottle deployment time')+
  scale_y_log10()+
  theme(text = element_text(size = 15))+
  labs(tag = 'B')
bin_boxplot

ggsave(filename = here('figures', 'fig_7_bin_boxplot.png'),
       bin_boxplot,
       dpi = 600)


multipanel_boxplot <- grid.arrange(abundance_boxplot, bin_boxplot, ncol = 2)
multipanel_boxplot
ggsave(multipanel_boxplot, filename = here('figures', 'fig_x_mutlipanel_boxplot.png'),
       width = 7)



# Brown-Forsythe tests ----------------------------------------------------

library(onewaytests)

# test effect of day/night time on number of insects captured
bf.test(variable_result ~ coarse_timing, data = filter(trap_insect_numbers, 
                                                       variable_type == 'Number of insects captured'))

# test effect of day/night time on number of BINs
bf.test(variable_result ~ coarse_timing, data = filter(trap_insect_numbers, 
                                                       variable_type == 'Number of unique BINs'))

