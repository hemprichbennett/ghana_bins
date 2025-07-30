library(tidyverse)
library(here)
library(vroom)


# read in the huge (23GB) file, specifying that we solely want the columns for
# the bin and country. File downloaded from
# https://v4.boldsystems.org/index.php/datapackages
all_public_bins <- vroom(here('~/data/manual_bold_queries/BOLD_Public.11-Jul-2025/BOLD_Public.11-Jul-2025.tsv'), delim ='\t',
                         col_select = c('bin_uri', 'country/ocean')) %>%
  rename(country = `country/ocean`) %>%
  # calculate some basic stats for use in plotting in script 8
  group_by(country) %>%
  summarise(nsamples = length(bin_uri),
         nbins = length(unique(bin_uri)))

write_csv(all_public_bins, here('data', 'processed_data', 'country_nsamples_public.csv')
          )  
