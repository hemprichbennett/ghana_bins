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
  summarise(nsamples = n())
# Matches that are NA for country or region are because they were mined from 
# Genbank and have no geographic data attached


