# In this script we explore the geographic origin of the non-'unique' 
# BINs in our dataset


# Setup -------------------------------------------------------------------

library(tidyverse)
library(countrycode)


public_bins_df <- read_csv('data/processed_data/bold_public_bin_matches.csv') %>%
  mutate(geographic_region = countrycode(sourcevar = country, 
                                         origin = "country.name",
                                         destination = "region")
  )



# Data summarising --------------------------------------------------------

basic_public_data <- public_bins_df %>%
  select(bin_uri, phylum_name, class_name, order_name, family_name, genus_name,
         species_name, institution_storing, collection_event_id, site_code,
         lat, lon, elev, country, region, geographic_region)
  


# make an object thats just the number of BINs we have for each 
# taxonomic order
distinct_bins <- public_bins_df %>%
  filter(!is.na(order_name), 
         !is.na(geographic_region)) %>%
  select(order_name, bin_uri) %>%
  distinct() %>%
  group_by(order_name) %>%
  summarise(global_distinct_bins = n())
  
region_percentage_BINs_present <- public_bins_df %>%
  filter(!is.na(order_name), 
         !is.na(geographic_region)) %>%
  select(order_name, bin_uri, geographic_region) %>%
  distinct() %>%
  group_by(order_name, geographic_region) %>%
  summarise(local_n_bins = n()) %>%
  ungroup() %>%
  left_join(distinct_bins) %>%
  mutate(
    # calculate the percentage of all public BINs which we've
    # collected that are known from a given geographic region
    percentage = local_n_bins / global_distinct_bins * 100,
    plotting_label = paste0(order_name, ', ', global_distinct_bins, ' public BINs')) 


# make a table of the top countries for number of BIN matches

top_countries <- public_bins_df %>%
  filter(!is.na(country), !is.na(order_name)) %>%
  select(country, bin_uri) %>%
  distinct() %>%
  group_by(country) %>%
  summarise(nbins = n()) %>%
  # reorder by n, with highest values at the top
  arrange(desc(nbins)) %>%
  slice(1:20) %>%
  pull(country)



geographic_region_bin_summary <- region_percentage_BINs_present %>%
  select(geographic_region, order_name, local_n_bins) %>%
  pivot_wider(names_from = geographic_region, 
              values_from = local_n_bins,
              values_fill = 0)



# add information on the number of BINs per-taxa that are not publicly available
# or haven't been sequenced, from the previous script
overall_uniqueness <- read_csv('data/processed_data/overall_bin_uniqueness.csv') %>%
  select(-english_common_name, -`Already publicly available`)

geographic_region_bin_summary <- geographic_region_bin_summary %>%
  left_join(overall_uniqueness, by = c('order_name' = 'order'))

write_csv(geographic_region_bin_summary, 'results/table_3_orders_by_region.csv')
