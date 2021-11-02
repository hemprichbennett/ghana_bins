# this will need to be updated a lot, its currently using a file which was 
# exported from mBRAVE and sent via email. The plotting also sucks
library(tidyverse)
library(iNEXT)
library(gridExtra)
mbrave_tib <- read_csv('data/mbrave_output.csv')

mbrave_tib <- mbrave_tib %>%
  mutate(seq_length = nchar(Sequence)) %>%
  filter(Phylum == 'Arthropoda')

# nsamples
mbrave_tib %>%
  group_by(Order) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = Order, y = nsamples)) +
  geom_bar(stat = 'identity')+
  theme_bw()+
  scale_y_continuous(trans = 'log10') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))+
  labs(x = 'Taxonomic Order', y = 'Number of samples')
  ggsave('figures/mbrave_samples.jpeg', width = 12)

#seqlength hists
ggplot(mbrave_tib, aes(x = seq_length)) +
         geom_histogram()+
         geom_vline(aes(xintercept=mean(seq_length)))+
  facet_wrap(. ~ Order)+ 
  scale_y_continuous(trans = 'log10') +
  theme_bw()

bin_accumulation <-   mbrave_tib %>%
  group_by(BIN_OTU) %>%
  summarise(n = n()) %>%
  pull(n) %>%
  iNEXT(.) 

all_bin_plot <- bin_accumulation %>%
  ggiNEXT() +
  labs(y = 'BIN richness') +
  theme_bw() +
  ggtitle('BIN accumulation (all arthropod taxa)') +
  theme(legend.position = 'bottom',
        text = element_text(size = 20))
all_bin_plot
ggsave('figures/all_bin_plot.jpeg')

mbrave_tib <- arrange(mbrave_tib, Order)

plot_list <- list()
for(i in 1:length(unique(mbrave_tib$Order))){
  chosen_order <- unique(mbrave_tib$Order)[i]
  
  order_bins <-   mbrave_tib %>%
    filter(Order == chosen_order) %>%
    group_by(BIN_OTU) %>%
    summarise(n = n()) %>%
    pull(n) %>%
    iNEXT(.) 
  
  plot_list[[i]] <- order_bins %>%
    ggiNEXT() +
    labs(y = 'BIN richness') +
    theme_bw() +
    ggtitle(paste(chosen_order)) +
    theme(legend.position = 'none',
          text = element_text(size = 10))
  
}


big_plot <- do.call("grid.arrange", c(plot_list, ncol = 3))

ggsave('figures/all_iNEXT.jpeg', big_plot, width = 20, height = 20)
