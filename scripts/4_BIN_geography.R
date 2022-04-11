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
  

cat('Samples came from ', length(unique(basic_public_data$country)), ' countries\n')

# crude geographic summary by abundance
public_bins_df %>%
  group_by(country) %>%
  summarise(nsamples = n())

public_bins_df %>%
  group_by(geographic_region) %>%
  summarise(nsamples = n(),
            nbins = length(unique(bin_uri)))
# Matches that are NA for country or region are because they were mined from 
# Genbank and have no geographic data attached

public_bins_df %>%
  group_by(bin_uri, geographic_region) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = nsamples))+
  geom_histogram() +
  facet_wrap(. ~ geographic_region) + 
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() +
  labs(x = 'Number of samples per BIN',
       y = 'Number of BINs')

# this plot is a bit confusing, but to summarise: the value on the left hand
# side is the number of BINs which only had ONE public sample in a given geographic
# region. As we progress to the right we have BINs which had tens, hundreds and thousands
# of samples in that geographic region



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

# now use that to make a table of the taxonomic
# composition of the BINs in common with those countries

public_bins_df %>%
  filter(!is.na(country), !is.na(order_name)) %>%
  filter(country %in% top_countries) %>%
  select(country, order_name, bin_uri) %>%
  distinct() %>%
  group_by(country, order_name) %>%
  summarise(nbins = n()) %>%
  arrange(order_name) %>%
  pivot_wider(names_from = order_name, values_from = nbins,
              values_fill = 0) %>%
  write_csv('results/public_data/orders_top_countries.csv')

ggplot(region_percentage_BINs_present, 
       aes(x = geographic_region,
           y = fct_rev(plotting_label),
           fill = percentage)) +
  geom_tile() + 
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(x = 'Geographic region', y = 'Taxonomic order')


geographic_region_bin_summary <- region_percentage_BINs_present %>%
  select(geographic_region, plotting_label, local_n_bins) %>%
  pivot_wider(names_from = plotting_label, 
              values_from = local_n_bins,
              values_fill = 0) %>%
  arrange(desc(geographic_region))
write_csv(geographic_region_bin_summary, 'results/public_data/orders_by_region.csv')


# Now for countries within Africa and the Middle East


country_percentage_BINs_present <- public_bins_df %>%
  filter(!is.na(order_name), 
         !is.na(geographic_region)) %>%
  # filter for regions which include the word 'Africa'
  filter(grepl('Africa', geographic_region)) %>%
  select(order_name, bin_uri, country) %>%
  distinct() %>%
  group_by(order_name, country) %>%
  summarise(local_n_bins = n()) %>%
  ungroup() %>%
  left_join(distinct_bins) %>%
  mutate(
    # calculate the percentage of all public BINs which we've
    # collected that are known from a given geographic region
    percentage = local_n_bins / global_distinct_bins * 100,
    plotting_label = paste0(order_name, ', ', global_distinct_bins, ' public BINs')) 


ggplot(country_percentage_BINs_present, 
       aes(x = country,
           y = fct_rev(plotting_label),
           fill = percentage)) +
  geom_tile() + 
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(x = 'Country', y = 'Taxonomic order')


country_bin_summary <- country_percentage_BINs_present %>%
  select(country, order_name, local_n_bins) %>%
  pivot_wider(names_from = order_name, 
              values_from = local_n_bins,
              values_fill = 0) %>%
  arrange(country)
write_csv(country_bin_summary, 'results/public_data/orders_by_country.csv')


# lets make a heatmap of only countries that have over n bins in common with
# us
bin_threshold <- 10
country_percentage_BINs_present %>% 
  group_by(country) %>% 
  summarise(n = sum(local_n_bins)) %>%
  filter(n >= bin_threshold) %>%
  left_join(country_percentage_BINs_present) %>%
  select(country, order_name, local_n_bins) %>%
  pivot_wider(names_from = country, 
              values_from = local_n_bins,
              values_fill = 0) %>%
  write_csv('results/public_data/orders_in_commonest_countries.csv')

