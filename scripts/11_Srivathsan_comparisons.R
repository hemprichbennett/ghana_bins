# Quick script to check our taxonomic counts relative to those found by 
# Srivathsan et al. 2023:
# 'Convergence of dominance and neglect in flying insect diversity'

library(tidyverse)
library(here)

our_data <- read_csv(file = here('data', 'processed_data', 
                                 'bold_and_earthcape_combined.csv')) %>%
  rename(trap_type = type) %>%
  filter(!is.na(trap_type),
         !is.na(family))


sri_values <- tibble(taxa_strings = c("Lepidoptera:Crambidae", 
                                      "Diptera:Chloropidae", 
                                      "Hymenoptera:Bethylidae", 
                                      "Hymenoptera:Eulophidae", 
                                      "Hemiptera:Aleyrodidae", 
                                      "Diptera:Sphaeroceridae", 
                                      "Lepidoptera:Erebidae", 
                                      "Diptera:Muscidae", 
                                      "Diptera:Dolichopodidae", 
                                      "Hymenoptera:Braconidae", 
                                      "Hymenoptera:Ichneumonidae", 
                                      "Hemiptera:Cicadellidae", 
                                      "Hymenoptera:Formicidae", 
                                      "Diptera:Phoridae", 
                                      "Diptera:Sciaridae", 
                                      "Diptera:Psychodidae", 
                                      "Hymenoptera:Platygastridae", 
                                      "Diptera:Chironomidae", 
                                      "Diptera:Ceratopogonidae", 
                                      "Diptera:Cecidomyiidae")) %>%
  separate(taxa_strings, into = c('Order', 'Family'), sep = ':')

# get the taxonomy of orders and families, for use later
orders_and_families <- our_data %>%
  select(order, family) %>%
  distinct()

# in a loop, calculate the abundance rankings of each family for each trap type
rankings_list <- list()
for(trap in unique(our_data$trap_type)){
  print(trap)
  rankings_list[[trap]] <- our_data %>%
    filter(trap_type == trap) %>%
    # calculate n_samples
    group_by(family) %>%
    summarise(n_samples = n()) %>%
    # order the tibble by n_samples
    arrange(desc(n_samples)) %>%
    # calculate rankings
    mutate(rank = rank(-n_samples),
           trap_type = trap) %>%
    # retain only the top 20
    filter(rank <= 20)
}

# combine the outputs
rankings_tib <- rankings_list %>%
  bind_rows() %>%
  # remove redundant column
  select(-n_samples) %>%
  # make it a 'wide' format, with a column for each trap type
  pivot_wider(names_from = trap_type, values_from = rank) %>%
  mutate(in_Srivathsan = family %in% sri_values$Family) %>%
  left_join(orders_and_families)

# find which families that were common in malaise traps were NOT in the top 
# 20 of Srivathsan's paper
rankings_tib %>%
  filter(!is.na(Malaise)) %>%
  filter(in_Srivathsan == F)


write_csv(rankings_tib, file = here('results', 'trap_family_rankings.csv'))
