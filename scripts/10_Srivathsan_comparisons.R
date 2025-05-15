# Quick script to check our taxonomic counts relative to those found by 
# Srivathsan et al. 2023:
# 'Convergence of dominance and neglect in flying insect diversity'

library(tidyverse)
library(here)

our_data <- read_csv(file = here('data', 'processed_data', 
                                 'bold_and_earthcape_combined.csv')) %>%
  rename(trap_type = type)


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


our_top20 <- our_data %>% 
  group_by(trap_type, family) %>%
  count()
