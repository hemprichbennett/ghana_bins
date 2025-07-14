# broader geographic analysis of BINs sequenced

library(tidyverse)
library(here)
library(countrycode)
library(geosphere)

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



# Make a plot of the top 20 countries -------------------------------------

# calculate the number of shared BINs
country_nshared_tib <- combined_tib %>%
  group_by(country, geographic_region) %>%
  summarise(nbins = n()) %>%
  ungroup() 

# find the top 20
top_20_nshared_tib <- country_nshared_tib %>%
  slice_max(nbins, n = 20) %>%
  mutate(country = fct(country))

top20_plot <- ggplot(top_20_nshared_tib, aes(x = nbins, y = fct_rev(country), fill = geographic_region)) +
  geom_bar(stat = 'identity')+
  theme_bw()+
  scale_fill_viridis_d()+
  xlab('Number of publicly available BINs shared with our dataset')+
  ylab('Country')+
  theme(legend.position = 'bottom')+ 
  labs(fill = 'Geographic area')

top20_plot
ggsave(plot = top20_plot,
       filename = here('figures', 'fig_5_topcountries_plot.png'),
       width = 8)

top_20_nshared_tib %>%
  mutate(Rank = seq(1,20)) %>%
  relocate(Rank) %>%
  rename(Country = country,
         `Geographic region` = geographic_region,
         `Number of shared BINs` = nbins) %>%
  write_csv(here('results', 'top_20_shared_bins.csv'))

# Subplot for distance between Ghana and top20 country --------------------

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
  left_join(country_nshared_tib, join_by(country_b == country))

distance_plot <- ggplot(distances_and_nbins_tib, aes(x = nbins, y = distance_km
                                    ))+
  geom_point()+ 
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  #scale_colour_viridis_d()+
  xlab('Number of publicly available BINs shared with our dataset')+
  ylab('Distance from Ghana (km)')+
  labs(colour = 'Geographic area')+
  facet_wrap(.~ geographic_region, ncol =3)

distance_plot
ggsave(plot = distance_plot,
       filename = here('figures', 'fig_5_dist_and_sharded_bins.png'))

# Analyse trap-composition of BINs with no public matches -----------------

unmatched <- bold_and_earthcape_combined %>%
  filter(!bin %in% public_matches$bin,
         !is.na(order))


unmatched_summary <- unmatched %>%
  group_by(order, trap_type) %>%
  summarise(nsamples = n(), nbins = length(unique(bin)))

unique_sample_trapping <- ggplot(unmatched_summary, aes(x = trap_type,
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



trap_and_status <- bold_and_earthcape_combined %>%
  filter(!is.na(order)) %>%
  # make a boolean variable, for if the BIN was/was not publicly available
  # prior to our dataset being made open
  mutate(publicly_available = 
           ifelse(bin %in% public_matches$bin, 'Already publicly available',
                  'Not publicly available')) %>%
  group_by(order, trap_type, publicly_available) %>%
  summarise(nsamples = n(), nbins = length(unique(bin)))


# A function to ensure that the y axis labels' breaks are always integers, taken
# from https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

trap_bin_availability_plot <- ggplot(trap_and_status, aes(x = trap_type,
                              y = nbins,
                              #colour = trap_type,
                            fill = publicly_available))+
  geom_bar(position='stack', stat='identity')+
  
  scale_fill_manual(values = c('steelblue', 'red'))+
  theme_bw()+
  # ensure that the y-axis breaks are always integers, using the function above
  scale_y_continuous(breaks = integer_breaks())+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(size=20))+
  #guides(fill = guide_legend(title = 'Trap type:'))+
  labs(x = 'Trap type', 
       y = 'Number of BINs that were/weren\'t publicly available on BOLD',
       fill = 'BIN availability')

trap_bin_availability_tall <- trap_bin_availability_plot +
  facet_wrap(.~ order, scales = 'free_y')
ggsave(filename = here('figures', 'trap_bin_availability.jpeg'),
       dpi = 600,
       plot = trap_bin_availability_tall)


trap_bin_availability_wide <- trap_bin_availability_plot +
  facet_wrap(.~ order, scales = 'free_y', ncol = 7)
ggsave(filename = here('figures', 'trap_bin_availability_wide.jpeg'),
       dpi = 600,
       plot = trap_bin_availability_wide, height = 10, width = 20)

ggsave(filename = here('figures', 'fig_1_trap_bin_availability_wide.jpeg'),
       dpi = 600,
       plot = trap_bin_availability_wide, height = 10, width = 20)