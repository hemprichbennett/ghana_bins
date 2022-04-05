library(tidyverse)
library(bold)

in_df <- read_csv('data/processed_data/tidied_bold_data.csv')

unique_bins <- in_df %>%
  filter(!is.na(bin)) %>%
  pull(bin)

bold_sequence_list <- list()

for(chosen_bin in unique_bins){
  bold_sequence_list[[chosen_bin]] <- bold_seq(bin = chosen_bin)
}
