# quick script making a summary number of the number of BINs and 
# sequences in the queried data from BOLD from Africa, versus the total
# number on BOLD

library(tidyverse)
library(here)

# MANUALLY CODED VARIABLES: check before submission as these will go out of date
total_n_sequences <- 16500000
total_n_bins <- 1326000

our_data <- read_csv(file = here('data', 'processed_data', 
                                 'bold_and_earthcape_combined.csv'))

our_n_bins <- length(unique(our_data$bin))
our_n_samples <- nrow(our_data)

our_bin_contribution_percent <- our_n_bins / total_n_bins * 100

african_countries <- readr::read_lines('data/raw_data/african_countries.txt')

queried_files <- list.files(here('data', 'processed_data', 'bold_queries'),
                            full.names = T) %>%
  purrr::map_dfr( ~ read_csv(file = ., 
                             col_select = c('bin_uri', 'country'))) %>%
  mutate(african = country %in% african_countries)

africas_n_public_bins <- queried_files %>%
  filter(african == T & !is.na(bin_uri)) %>%
  select(bin_uri) %>%
  distinct() %>%
  nrow()

africas_bin_contribution_percent <- africas_n_public_bins / total_n_bins * 100


# all west african countries as defined by the UN 
# https://en.wikipedia.org/wiki/West_Africa , excluding Ghana as our data
# will inflate the total dramatically
west_african_countries <- c("Benin", "Burkina Faso", "Cape Verde", "Gambia", 
                            "Guinea", "Guinea-Bissau", "Cote d'Ivoire", 
                            "Liberia", "Mali", "Mauritania", "Niger", "Nigeria",
                            "Senegal", "Sierra Leone", "Togo")

total_westafrican_nonghanain_public_bins <- queried_files %>%
  filter(!is.na(bin_uri)) %>%
  filter(country %in% west_african_countries) %>%
  select(bin_uri) %>%
  distinct() %>%
  nrow()
