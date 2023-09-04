# script has no defined purpose yet, I want to poke the
# latest iteration of our data
library(tidyverse)
library(vegan)

fortified_data <- read_csv('data/processed_data/fortified.csv')
our_big_df <- read_csv('data/processed_data/our_organised_bold_data.csv')


unique(our_big_df$sampling_protocol)

bin_taxa <- our_big_df %>%
  select(bin, phylum, class, order, family, genus, species)

# function to filter and reorganise our data, then plot
# stacked barplots (I know...) of them
tileplot <- function(data, filter_by){
  data %>%
    filter(!is.na(order)) %>%
    filter(sampling_protocol == filter_by) %>%
    select(bin, order, family, genus, exact_site) %>%
    group_by(order, exact_site) %>%
    summarise(n_bins = n()) %>%
  ggplot(., aes(x = exact_site, y = order)) +
    geom_tile(aes(fill = n_bins), colour = "grey50")+
    theme_classic() +
    theme(legend.position = 'bottom')+
    xlab('Site')+
    ylab('Number of BINs sequenced and identified to Order level')+
    ggtitle(paste0('Tileplot of the BINs collected by ', filter_by, 's'))+
    # make the plot in the sensible, alphabetical order, rather than the weird
    # default, which has 'A' at the bottom
    scale_y_discrete(limits=rev)
}

tileplot(our_big_df, filter_by = 'Sweep Net')    
tileplot(our_big_df, filter_by = 'Heath Trap')  
  

# for some reason there are no sweep net samples listed from Abutia

# look at beta-diversity and turnover over time and space?

for_betadiv <- our_big_df %>%
  # sadly if a sample has yet to be given a BIN it can't really
  # be analysed here
  filter(!is.na(bin)) %>%
  filter(!is.na(order)) %>%
  filter(!is.na(exact_site)) %>%
  select(bin, order, family, genus, lat, lon, sampling_protocol,
         exact_site, collection_date, collection_date_accuracy)



sitewise_alpha_diversity <- for_betadiv %>%
  group_by(exact_site, bin) %>%
  summarise(n = n()) %>%
  group_by(exact_site) %>%
  summarise(alpha_div = diversity(n))

library(vegan)

for_vegdist <- for_betadiv %>%
  filter(!is.na(sampling_protocol)) %>%
  select(bin, sampling_protocol) %>%
  group_by(bin, sampling_protocol) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = bin,
              values_from = n,
              values_fill = 0) #%>%
  column_to_rownames(var = 'sampling_protocol')

# look at dissimilarity of communities
for_vegdist %>%
  column_to_rownames(var = 'sampling_protocol') %>%
  vegdist()

# make a tibble of taxa frequencies per-BIN and 'trap' type
trap_captures <- for_betadiv %>%
  filter(!is.na(sampling_protocol)) %>%
  group_by(bin, sampling_protocol) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = sampling_protocol, values_from = n,
              values_fill = 0) %>%
  mutate(in_both = ifelse(`Heath Trap` > 0 && `Sweep Net` >0, TRUE, FALSE)) %>%
  left_join(bin_taxa)

trap_captures %>%
  group_by(in_both, order) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = in_both, values_from = n)
