# script has no defined purpose yet, I want to poke the
# latest iteration of our data
library(tidyverse)

fortified_data <- read_csv('data/processed_data/fortified.csv')
our_big_df <- read_csv('data/processed_data/our_organised_bold_data.csv')


unique(our_big_df$sampling_protocol)

# function to filter and reorganise our data, then plot
# stacked barplots (I know...) of them
tileplot <- function(data, filter_by){
  data %>%
    filter(sampling_protocol == filter_by) %>%
    select(bin, sampling_protocol, order, family, genus, exact_site) %>%
    group_by(order, exact_site) %>%
    summarise(n_bins = n()) %>%
  ggplot(., aes(x = exact_site, y = order)) +
    geom_tile(aes(fill = n_bins), colour = "grey50")+
    theme_classic() +
    theme(legend.position = 'bottom')+
    xlab('Site')+
    ylab('Proportion of BINs identified to Order level')+
    ggtitle(paste0('Stacked barplot of the BINs collected by ', filter_by, 's'))
}

tileplot(our_big_df, filter_by = 'Sweep Net')    
tileplot(our_big_df, filter_by = 'Heath Trap')  
  

# for some reason there are no sweep net samples listed from Abutia

