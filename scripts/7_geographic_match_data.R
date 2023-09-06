# broader geographic analysis of BINs sequenced

library(tidyverse)
library(here)
library(countrycode)

# a tibble where every row is a sample we've collected
bold_and_earthcape_combined <- read_csv(
  file = here('data', 'processed_data', 
              'bold_and_earthcape_combined.csv')
) %>%
  janitor::clean_names() %>%
  # remove columns that aren't needed, and can confuse the merge later
  select(-sex, - starts_with('country')) %>%
  # rename a column that has a stupidly vague name (past-Dave is an ass)
  rename(trap_type = type) %>%
  # remove any NA values that have snuck through
  #~~~~~~~~~~~~~~~~
  # also perhaps work out why they snuck through, shouldn't they have been
  # filtered out in previous scripts
  filter(!is.na(trap_type), !is.na(bin))

# a tibble where every BIN is a public sequence that matches a BIN in our
# own samples collected
public_matches <- read_csv(here('data', 'processed_data', 'bold_public_bin_matches.csv'))%>%
  mutate(geographic_region = countrycode(sourcevar = country, 
                                         origin = "country.name",
                                         destination = "region")
  ) %>%
  select(-sex) %>%
  janitor::clean_names() %>%
  filter(!is.na(geographic_region), 
         !is.na(order_name)) %>%
  rename(bin = bin_uri) %>%
  # many bins appear twice despite referring to the same country, as that country
  # has multiple records per-bin. This can confuse things later, so remove 
  # most metadata and then retain only the distinct rows
  select(bin, country, geographic_region, phylum_name, class_name, order_name,
        genus_name, species_name) %>%
  distinct()


# combine datasets

# make a tibble showing what trap type captured each bin detected, and how many
our_bin_captures <- bold_and_earthcape_combined %>%
  select(bin, trap_type) %>%
  mutate(trap_type = paste0(trap_type, '_trap')) %>%
  group_by(bin, trap_type) %>%
  summarise(n_bins = n()) %>%
  pivot_wider(names_from = trap_type, values_from = n_bins,
              values_fill = 0)
  

# combine it with the public data, remove any rows with no matches
combined_tib <- public_matches %>%
  inner_join(our_bin_captures, by = 'bin') %>%
  select(bin, country, geographic_region, 
         ends_with('_trap'))

# make a tibble of the number of BINs per geographic region that were found 
# by a given trapping method
region_bin_traps <- combined_tib %>%
  pivot_longer(cols = ends_with('_trap'),
               names_to = 'trap_type', values_to = 'n_matches') %>%
  filter(n_matches > 0) %>%
  group_by(geographic_region, trap_type) %>%
  summarise(n_bins_found = n()) %>%
  # make trap_type a nicer string to use in the plot legend
  mutate(trap_type = gsub('_', ' ', trap_type),
         trap_type = str_to_sentence(trap_type),
         trap_type = gsub('Cdc', 'CDC', trap_type))

# make a barplot of it
ggplot(region_bin_traps, aes(fill = trap_type, x = geographic_region,
                             y = n_bins_found))+
  geom_bar(position='dodge', stat='identity')+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(title = 'Trap type:'))+
  labs(x = 'Geographic region', 
       y = 'Number of publicly available BINS matching ones in our dataset')



# Analyse trap-composition of BINs with no public matches -----------------

unmatched <- bold_and_earthcape_combined %>%
  filter(!bin %in% public_matches$bin,
         !is.na(order))


unmatched_summary <- unmatched %>%
  group_by(order, trap_type) %>%
  summarise(nsamples = n(), nbins = length(unique(bin)))

ggplot(unmatched_summary, aes(x = trap_type,
                              y = nbins,
                              fill = trap_type))+
  geom_bar(position='dodge', stat='identity')+
  facet_wrap(.~ order, scales = 'free_y')+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(title = 'Trap type:'))+
  labs(x = 'Trap type', 
       y = 'Number of BINs that were not publicly available on BOLD')
