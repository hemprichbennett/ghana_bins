# Script to produce files summarising our data from
# earthcape after it's manually exported.
# Currently has no integration with our BIN data
# from BOLD. That's a to-do


library(tidyverse)
in_df <- read_csv('2021_08_09_Individuals.csv')

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

in_df <- in_df %>%
  # the cases are mixed
  mutate(Order = str_to_lower(Order)) %>%
  mutate(Order = firstup(Order)) %>%
  group_by(Order) %>%
  summarise(n = n())

ggplot(in_df, aes(x = Order, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))+
  labs(x = 'Taxonomic Order', y = 'Number of samples')

ggsave(filename = 'crag_stuff/plots/nsamples.jpeg', width = 12)
