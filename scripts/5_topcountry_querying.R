library(tidyverse)
library(here)
library(countrycode)
library(bold)

our_big_df <- read_csv(here('data', 'processed_data', 'bold_public_bin_matches.csv'))%>%
  mutate(geographic_region = countrycode(sourcevar = country, 
                                         origin = "country.name",
                                         destination = "region")
  ) %>%
  filter(!is.na(geographic_region), 
         !is.na(order_name))


country_rankings <- our_big_df %>%
  select(country, bin_uri, geographic_region) %>%
  distinct() %>%
  filter(grepl('Africa', geographic_region)) %>%
  group_by(country) %>%
  summarise(`Number of shared BINs` = n()) %>%
  arrange(desc(`Number of shared BINs`)) %>%
  slice(1:20)



country_list <- list()
for(chosen_country in country_rankings$country){
  print(chosen_country)
  country_list[[chosen_country]] <- bold_seqspec(geo = chosen_country)
}
