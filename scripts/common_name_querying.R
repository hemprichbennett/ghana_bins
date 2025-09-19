library(tidyverse)
library(taxize)
bold_data <- read_csv('data/processed_data/our_organised_bold_data.csv')

# a subset of field to use in later analyses: we only care about samples
# from our field sites, so exclude other 'test' samples
bold_field_data <- bold_data %>%
  filter(exact_site %in% c('Abutia Amegame', 'Mafi Agorve')) %>%
  # if it doesn't have a BIN we don't want it
  filter(!is.na(bin))


unique_taxonomy <- bold_field_data %>%
  filter(!is.na(order) & !is.na(family)) %>%
  select(order, family, genus, species) %>%
  distinct()


order_common_names <- unique_taxonomy %>%
  select(order) %>%
  distinct() %>%
  mutate(common_name = sci2comm(order))
  


order_common_names_tib <- unique_taxonomy %>%
  pull(order) %>%
  unique() %>%
  sci2comm() %>%
  unlist %>% 
  tibble(order = names(.), order_common_name = .)

family_common_names_tib <- unique_taxonomy %>%
  pull(family) %>%
  unique() %>%
  sci2comm() %>%
  unlist %>% 
  tibble(family = names(.), family_common_name = .)

genus_common_names_tib <- unique_taxonomy %>%
  pull(genus) %>%
  unique() %>%
  sci2comm() %>%
  unlist %>% 
  tibble(genus = names(.), genus_common_name = .)

species_common_names_tib <- unique_taxonomy %>%
  pull(species) %>%
  unique() %>%
  sci2comm() %>%
  unlist %>% 
  tibble(species = names(.), species_common_name = .)

manual_order_common_names <- read_csv('data/order_common_names.csv')

automated_names_df <- unique_taxonomy %>%
  left_join(order_common_names_tib) %>%
  left_join(family_common_names_tib) %>%
  left_join(genus_common_names_tib) %>%
  left_join(species_common_names_tib)

# fill in a few weird order-level blanks with my manually-created csv
all_names_df <- automated_names_df %>%
  left_join(manual_order_common_names) %>%
  mutate(order_common_name = ifelse(is.na(order_common_name), 
                                    english_common_name, order_common_name)) %>%
  select(-english_common_name) %>%
  arrange(across(everything()))


write_csv(all_names_df, file = 'results/taxonomic_and_common_names.csv')

n_common_names <- nrow(order_common_names_tib) + nrow(family_common_names_tib) + 
  nrow(genus_common_names_tib) + nrow(species_common_names_tib)
cat('Number of common names found was', n_common_names, '\n')


taxonomy_counts <- unique_taxonomy %>%
  summarise(across(everything(), n_distinct))
n_taxa <- sum(taxonomy_counts)
cat('Number of unique taxonomic values was', n_taxa, '\n')
