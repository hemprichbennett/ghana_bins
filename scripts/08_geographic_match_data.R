# broader geographic analysis of BINs sequenced

library(tidyverse)
library(here)
library(countrycode)
library(geosphere)
library(broom)

# data of the centroids of every country, sourced from
# https://github.com/gavinr/world-countries-centroids/blob/master/dist/countries.csv
country_centroids <- read_csv(here('data', 'raw_data', 'country_centroids.csv'))

# a tibble where every row is a sample we've collected
bold_and_earthcape_combined <- read_csv(
  file = here('data', 'processed_data', 
              'bold_and_earthcape_combined.csv')
) %>%
  janitor::clean_names() %>%
  # remove columns that aren't needed, and can confuse the merge later
  select(-sex, - starts_with('country')) %>%
  # rename a column that has a stupidly vague name
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

per_country_public_summary <- read_csv(here('data', 'processed_data', 'country_nsamples_public.csv'))

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

# Make a table of the top 20 countries -------------------------------------

# calculate the number of shared BINs
country_nshared_tib <- combined_tib %>%
  group_by(country, geographic_region) %>%
  summarise(n_shared_bins = n()) %>%
  ungroup() 

# find the top 20
top_20_nshared_tib <- country_nshared_tib %>%
  slice_max(n_shared_bins, n = 20) %>%
  mutate(country = fct(country))

top_20_nshared_tib %>%
  mutate(Rank = seq(1,20)) %>%
  relocate(Rank) %>%
  rename(Country = country,
         `Geographic region` = geographic_region,
         `Number of shared BINs` = n_shared_bins) %>%
  write_csv(here('results', 'tbl_4_top_20_shared_bins.csv'))

# Organise data --------------------

# make a matrix of all countries locations, to use as a distance matrix
coord_mat <- country_centroids %>%
  select(longitude, latitude) %>%
  as.matrix(dimnames = country_centroids$COUNTRY)

# calculate distances
distances_matrix <- distm(coord_mat)
rownames(distances_matrix) <- country_centroids$COUNTRY
colnames(distances_matrix) <- country_centroids$COUNTRY

# tidy the distance matrix
tidier_dist <- as_tibble(distances_matrix, rownames = 'country_a') %>%
  pivot_longer(cols = -'country_a', names_to = 'country_b', values_to = 'distance_m') %>%
  mutate(distance_km = distance_m / 1000)

# filter it for just our desired data
distances_and_nbins_tib <- tidier_dist %>%
  filter(country_a == 'Ghana',
         country_b %in% country_nshared_tib$country) %>%
# join it with the country_nshared_tib
  left_join(country_nshared_tib, join_by(country_b == country)) %>%
  left_join(per_country_public_summary, join_by(country_b == country)) %>%
  filter(country_b != 'Ghana')


# Model -------------------------------------------------------------------


shared_bins_mod <- lm(n_shared_bins ~ distance_km + nsamples, data = distances_and_nbins_tib)
plot(shared_bins_mod,2)
broom::glance(shared_bins_mod)
broom::tidy(shared_bins_mod)


# Plots -------------------------------------------------------------------

# quick function to kill scientific notation from axes etc, from https://r-charts.com/ggplot2/axis/#scales
marks_no_sci <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)  
  
scatterplot <- ggplot(distances_and_nbins_tib, aes(y = n_shared_bins, x = nsamples))  +
  geom_point()+
  facet_wrap(.~ geographic_region, ncol =3)+
  theme_bw()+
  scale_x_log10(labels = marks_no_sci)+
  scale_y_log10()+
  ylab('Number of publicly available BINs shared with our dataset')+
  xlab('Number of publicly available sequences')
scatterplot  

ggsave(plot = scatterplot,
       filename = here('figures', 'fig_4_npublic_and_shared_bins.pdf'),
       dpi = 600)



distance_plot <- ggplot(distances_and_nbins_tib, aes(y = n_shared_bins, x = distance_km
                                    ))+
  geom_point()+ 
  geom_smooth(method = 'lm')+
  theme_bw()+
  ylab('Number of publicly available BINs shared with our dataset')+
  xlab('Distance from Ghana (km)')+
  labs(colour = 'Geographic area')

distance_plot
ggsave(plot = distance_plot,
       filename = here('figures', 'fig_5_dist_and_shared_bins.pdf'),
       dpi = 600)
