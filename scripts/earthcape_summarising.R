# Script to produce files summarising our data from
# earthcape after it's manually exported.
# Currently has no integration with our BIN data
# from BOLD. That's a to-do


library(tidyverse)
file_path <- list.files('data/raw_data/earthcape_exports/', full.names = T) %>%
  max(.) 

in_df <- read_csv(file_path) %>%
  janitor::clean_names()

date_of_export <- gsub('.+/|_Individuals.+', '', file_path) %>%
  lubridate::ymd()

processed_df <- in_df %>%
  # Various string substitutions to convert temporary_name
  # to a uniform, pleasant, format
  mutate(Order = gsub('_.+', '', temporary_name)) %>%
  mutate(Order = gsub('_', '', Order)) %>%
  mutate(Order = str_to_title(Order)) %>%
  # Remove 'Larvae' as that isn't a taxonomic order
  filter(Order != 'Larvae') %>%
  group_by(Order) %>%
  summarise(n = n())

ggplot(processed_df, aes(x = Order, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))+
  scale_y_continuous(trans = 'log10') +
  labs(x = 'Taxonomic Order', y = 'Number of samples')

ggsave(filename = 'figures/nsamples.jpeg', width = 12)
