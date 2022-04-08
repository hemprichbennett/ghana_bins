# In this script we explore the geographic origin of the non-'unique' 
# BINs in our dataset


# Setup -------------------------------------------------------------------

library(tidyverse)
library(countrycode)


our_samples_df <- read_csv('data/processed_data/bold_data_with_availability.csv')

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




public_bins_df %>%
  filter(!is.na(geographic_region)) %>%
  group_by(bin_uri, geographic_region) %>%
  summarise(nsamples = n()) %>%
  group_by(geographic_region, nsamples) %>%
  summarise(nmatches = n()) %>%
  ggplot(., aes(x = nsamples, y = nmatches))+
  geom_point()+ 
  facet_wrap(. ~ geographic_region) +
  theme_bw()+
  scale_y_log10() +
  scale_x_log10() 
# I don't even really know what this is showing rn

public_bins_df %>%
  filter(geographic_region %in% c("Sub-Saharan Africa",
                                  "Middle East & North Africa")) %>%
  group_by(bin_uri, geographic_region, order_name) %>%
  summarise(nsamples = n()) %>%
  group_by(geographic_region, nsamples, order_name) %>%
  summarise(nmatches = n()) #%>%
  ggplot(., aes(x = nsamples, y = nmatches))+
  geom_point()+ 
  facet_grid( geographic_region ~ order_name) +
  theme_bw()+
  scale_y_log10() +
  scale_x_log10() 
