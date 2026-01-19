# This script incorporates data from the previous two, plus
# data that BOLD's portal provides us on our BINs and their
# uniqueness. Sadly these files have had to have minor 
# manual modifications prior to import


# Read in and explore bin reports ------------------------------------------

library(tidyverse)
library(here)
our_bin_reports <- list.files(path = here('data', 'processed_data'),
                              pattern = 'bin_report',
                              recursive = T,
                              full.names = T) %>%
  map_dfr(read_tsv, id = 'project') %>%
  mutate(project = gsub('.+data/|/bin.+|_bin_report.tsv', '', project)) 

# a few of the our BINs have multiple rows within a single project,
# because they have multiple taxonomic identifiers associated with them,
# including multiple taxonomic levels (e.g. BOLD:AEK3140 is assigned to 
# both Coleoptera and Staphylinidae)

# check to see if a BIN is actually unique to us, just not one of our subprojects
duplicated_nonunique_bins <- our_bin_reports %>% 
  filter(Unique == F) %>% 
  group_by(BIN) %>%
  filter(n()>1)

duplicate_bin_list <- list()
for(i in 1:length(unique(duplicated_nonunique_bins$BIN))){
  BIN <- unique(duplicated_nonunique_bins$BIN[i])
  
  bin_matches <- duplicated_nonunique_bins %>% 
    filter(BIN == BIN)
  
  actually_unique <- sum(bin_matches$`Count in Project`) == max(bin_matches$TotalMembers)
  duplicate_bin_list[[i]] <- data.frame(BIN, actually_unique)
}
# are any of our duplicate bins actually unique? If we get any rows for 'TRUE'
# then its yes, but its currently no
bind_rows(duplicate_bin_list) %>% group_by(actually_unique) %>% summarise(n = n())


# phew...


# Read in and check previous csvs -----------------------------------------

our_big_df <- read_csv(here('data', 'processed_data',
                            'bold_and_earthcape_combined.csv'))
bold_public_info_df <- read_csv(here('data', 'processed_data',' 
                                     bold_public_bin_matches.csv'))

common_names <- read_csv(here('data', 'order_common_names.csv'))

our_bin_reports %>%
  filter(Unique == T) %>%
  filter(BIN %in% bold_public_info_df$bin_uri)
# none of our BINs which BOLD tells us are unique were available 
# via a public search. Thats good: it shows us that their
# labelling works



# Data-processing ---------------------------------------------------------

# now to make something usable

# get all our 'non-unique' BINs and identify if they 
# do or don't have public sequences
non_unique_BINS <- our_bin_reports %>%
  filter(Unique == F) %>% 
  mutate(available = ifelse(BIN %in% bold_public_info_df$bin_uri, 
                            'Already publicly available', 'Already sequenced, no public sequences'))

overall_availability <- our_bin_reports %>%
  filter(Unique == T) %>%
  # make a column showing that they're all unique
  mutate(available = 'Unique to our project') %>%
  # combine with the above df
  bind_rows(non_unique_BINS) %>%
  mutate(available = as.factor(available))


# now combine it with our field data
main_df <- overall_availability %>%
  select(BIN, available) %>%
  # as bold has been a dick and given us some BINs twice (where the 
  # same BIN was identified as 2 or more taxa before sequencing)
  # we want to remove those duplicate values now
  distinct() %>%
  left_join(our_big_df, 
            by = c('BIN' = 'bin')) %>%
  left_join(common_names)


# now for number of BINs
overall_uniqueness <- main_df %>%
  select(order, available, english_common_name, BIN) %>%
  distinct() %>%
  group_by(order, available, english_common_name) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = available, values_from = n, 
              values_fill = 0,
              names_sort = T)

write_csv(overall_uniqueness, here('data', 'processed_data', 
                                   'overall_bin_uniqueness.csv'))

# summary information for the 200 most common BINs
retention_threshold <- 200
abundant_bin_summary <- main_df %>%
  group_by(BIN, available, order, family, species) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(retention_threshold) %>%
  mutate(bold_url = paste0('https://portal.boldsystems.org/bin/',BIN))

# table is not used in manuscript text or SI, but referred to in-text (lines 284 onwards in Sep/Oct 2025 submission)
write_csv(abundant_bin_summary, 'results/abundant_bins.csv')

abundant_bin_summary %>%
  group_by(order) %>%
  summarise(nbins = n()) %>%
  arrange(desc(nbins))

# quick summary values for the manuscript text
main_df %>% group_by(type, available) %>% summarise(n = length(unique(BIN)))

abundant_bin_summary %>% arrange(species) %>% pull(species) %>% unique()

# the numbers of those 200 in the different availability categories
abundant_bin_summary %>% group_by(available) %>% summarise(ntaxa = n())

# total number publicly available
number_publicly_available <- main_df %>% filter(available == 'Already publicly available') %>%
  pull(BIN) %>% unique() %>% length()

total_n_bins <- main_df %>% pull(BIN) %>% unique() %>% length()

# percent available
number_publicly_available / total_n_bins * 100


# number privately held
number_privately_held <- main_df %>% filter(available == 'Already sequenced, no public sequences') %>%
  pull(BIN) %>% unique() %>% length()

# percent hidden
number_privately_held / total_n_bins * 100



# Make a plot of the sequence lengths -------------------------------------

#Some failed sequences have length zero, some have length NA. These both need 
# accounting for

for_seqlength_plot <- our_big_df %>%
  # format the column better for analysis, as it's currently in a weird 
  # character format
  mutate(seqlength = gsub('\\[.+', '', coi_5p_seq_length),
         seqlength = as.numeric(seqlength),
         seqlength = na_if(seqlength, 0))

ggplot(filter(for_seqlength_plot, !is.na(seqlength)), aes(x = seqlength))+
  geom_histogram(binwidth = 10)+
  theme_bw()+
  xlab('Sequence length')+
  ylab('Number of sequences')

ggsave(filename = here('figures', 'fig_si_x_seqlengths.png'), width = 12, height = 7)
  
# summary stats for plot
mean(for_seqlength_plot$seqlength, na.rm = T)
sd(for_seqlength_plot$seqlength, na.rm = T)

# the number of samples that DID NOT generate a sequence
length(which(is.na(for_seqlength_plot$seqlength)))

# the number of samples that DID generate a sequence
length(which(!is.na(for_seqlength_plot$seqlength)))
