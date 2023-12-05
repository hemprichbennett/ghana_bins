# Code for reading in and formatting basic data from BOLD
# Setup -------------------------------------------------------------------

# load packages
library(readxl)
library(iNEXT)
library(purrr)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)


# Read in the data

# this assumes that the input data has had the redundant first two rows
# (eyeroll) of the raw data on each spreadsheet removed
xl_paths <- list.files(path = 'data/processed_data', pattern = '*tidied*', full.names = T, recursive = T)

download_date <- file.info('data/raw_data/GCEP/bold.xlsx')$ctime %>% 
  ymd_hms() %>%
  format(., "%d %B %Y")


# function for taking the horrible excel files and (assuming that the redundant first
# two rows of the raw data on each spreadsheet are removed) loads it in in
# a useable format

data_import <- function(xl_path){
  xl_path %>%
    excel_sheets() %>% 
    set_names() %>% 
    # read all the sheets in as individual list items
    map(read_excel, path = xl_path) %>%
    # join  them all into a master sheet
    reduce(., left_join) %>%
    clean_names() %>%
    # make the dates usable, rather than a string
    mutate(collection_date = dmy(collection_date))
}


# now read in the data, with each spreadsheet being a different list item
bold_list <- lapply(xl_paths, data_import)  

# combine the list items into a single dataframe
bold_data <- bind_rows(bold_list)

write_csv(bold_data, 'data/processed_data/our_organised_bold_data.csv')

# (for plotting later) make a string giving the number of samples which we have site information
# for. 
nsamples <- filter(bold_data, !is.na(exact_site)) %>% 
  nrow(.) %>% 
  # Format it with commas, to make it more human-readable
  format(., big.mark = ',')

# a subset of field to use in later analyses: we only care about samples
# from our field sites, so exclude other 'test' samples
bold_field_data <- bold_data %>%
  filter(exact_site %in% c('Abutia Amegame', 'Mafi Agorve')) %>%
    # if it doesn't have a BIN we don't want it
    filter(!is.na(bin))

# Summary tables ----------------------------------------------------------

# read in some common names for each taxonomic order, for the benefit
# of any readers who aren't yet familiar with the Ixodida or Strepsiptera...

common_names <- read_csv('data/order_common_names.csv')

summary_table <- bold_field_data %>%
  filter(!is.na(order)) %>%
  group_by(exact_site, order) %>%
  summarise(`Number of samples sequenced` = n(), 
            `Number of unique BINs found` = length(unique(bin))) %>%
  ungroup() %>%
  left_join(common_names) %>%
  rename(`Taxonomic order` = 'order',
         `English common name` = 'english_common_name') %>%
  relocate(`English common name`, .after = `Taxonomic order`)


summary_table

# split the summary table into a list of two items, by sample site
site_tables <- split(summary_table, f = summary_table$exact_site)

# save the tables as csvs
for(i in 1:length(site_tables)){
  filename <- paste0('data/processed_data/', names(site_tables)[i], '_summary.csv')

  site_tables[[i]] %>%
    select(-exact_site) %>%
    write_csv(., file = filename)
}

# make a table that's just the taxonomy of samples we collected



orders_and_families_sequenced <- bold_field_data %>%
  filter(!is.na(order) & !is.na(family)) %>%
  select(order, family) %>%
  distinct() %>%
  left_join(common_names) %>%
  select(order, english_common_name, family) %>%
  arrange(across(everything())) %>%
  rename(`Taxonomic Order` = order,
         `Taxonomic Order's english common name` = english_common_name, 
         `Taxonomic Family` = family)

family_common_names <- taxize::sci2comm(orders_and_families_sequenced$`Taxonomic Family`)

write_csv(orders_and_families_sequenced, file = 'results/orders_and_families_sequenced.csv')

# Basic plotting ----------------------------------------------------------

# function happily borrowed from https://stackoverflow.com/a/66583089
# to make the histogram bins exactly one month wide. Otherwise they default to 
# being ~30 days wide, which can get misleading
by_month <- function(x,n=1){
  seq(min(x,na.rm=T),max(x,na.rm=T),by=paste0(n," months"))
}

# for plotting, we'll want to have the x-axis going up in 6 month increments. So we want the 
# first and last axis labels to be either January or July, depending on which is appropriate

# get the first and last days in our dataset so far
first_collection_day <- bold_field_data %>% pull(collection_date) %>% min(.)
last_collection_day <- bold_field_data %>% pull(collection_date) %>% max(.)

# make some values for happier plotting later, by giving some thresholds 
# for the x-axis depending on our first and last collection day
if(month(first_collection_day) <7){
  date_1 <- paste0('01-01-', year(first_collection_day))
  }else{
  date_1 <- paste0('01-07-',year(first_collection_day))
  }

if(month(last_collection_day) <7){
  date_2 <- paste0('01-07-', year(last_collection_day))
}else{
  date_2 <- paste0('01-01-',year(last_collection_day)+1)
}

# make a simple plot of the number of SAMPLES over time

collection_date_histogram <- filter(bold_field_data, !is.na(exact_site)) %>%
  ggplot(., aes(x = collection_date)) + 
    # make the histogram breaks by month, using the function from above
  geom_histogram(breaks = by_month(bold_field_data$collection_date)) +
  # make subgraphs for each site
  facet_wrap(.~ exact_site) +
  # cosmetic improvements
  theme_bw() +
  theme(text = element_text(size = 20),
        # rotate the x-axis labels
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste('Collection dates of the', nsamples, 'samples with metadata sequenced by', download_date))+
  # manually set the x-axis scale
    scale_x_date(breaks = seq(dmy(date_1), 
                            dmy(date_2), 
                            by="4 months"), 
               date_labels = "%b\n%Y")+
  xlab('Collection date') + 
  ylab('Number of samples sequenced')
# show it
collection_date_histogram

# save it
ggsave('figures/collection_date_histogram.jpeg', collection_date_histogram,
       width = 14)


# make a plot of the number of samples of each taxonomic order sequenced
# so far
bold_field_data %>%
  filter(!is.na(order)) %>%
  group_by(order) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = order, y = nsamples)) +
  geom_bar(stat = 'identity')+
  theme_bw()+
  scale_y_continuous(trans = 'log10') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))+
  ggtitle(paste('Taxonomic orders of the', nsamples, 'samples sequenced by', download_date))+
  labs(x = 'Taxonomic Order', y = 'Number of samples')
ggsave('figures/sample_taxonomy.jpeg', width = 14)


# now the same plot, but split between sites

bold_field_data %>%
  filter(!is.na(order)) %>%
  group_by(order, exact_site) %>%
  filter(exact_site %in% c('Abutia Amegame', 'Mafi Agorve')) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = order, y = nsamples)) +
  geom_bar(stat = 'identity')+
  facet_wrap(. ~ exact_site) +
  theme_bw()+
  scale_y_continuous(trans = 'log10') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20))+
  ggtitle(paste('Taxonomic orders of the field samples sequenced by', download_date))+
  labs(x = 'Taxonomic Order', y = 'Number of samples')
ggsave('figures/site_taxonomy.jpeg', width = 14)


# iNEXT data prep ---------------------------------------------------------

# now we can work on the BINs and their known taxonomic information.


# basic checking of how many samples actually have a BIN assigned
# so far
n_unassigned <- bold_data %>%
  filter(is.na(bin)) %>%
  nrow(.)

cat("The number of samples which don't yet have a bin is", n_unassigned, '.\n',
    'This is', round(n_unassigned / nrow(bold_data) * 100, 2), 
    '% of the', nrow(bold_data), ' samples which have been sequenced')

overall_bin_frequencies <- bold_data %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  summarise(n = n())

order_bin_frequencies <- bold_data %>%
  filter(!is.na(bin)) %>%
  group_by(bin, order) %>%
  summarise(n = n()) %>%
  filter(n > 0) 

order_nsamples <- order_bin_frequencies %>% 
  group_by(order) %>% 
  summarise(nsamples = sum(n))

# get the names of the orders with 40 or more
# samples
to_inext <- order_nsamples %>%
  filter(nsamples > 50) %>%
  filter(!is.na(order)) %>%
  pull(order)

# for the orders, make a list of their frequencies
abundance_list <- list()
for(chosen_order in to_inext){
  
  abundance_vec <- order_bin_frequencies %>%
    filter(order == chosen_order) %>%
    pull(n)
  cat(chosen_order, 'contains', length(abundance_vec), 'different BINs\n')
  # if the Order only contains less than 10 BINs, abandon it
  if(length(abundance_vec) >= 40){
    abundance_list[[chosen_order]] <-abundance_vec 
  }
  
}


# make the iNEXT objects --------------------------------------------------


# a basic iNEXT object with all items on a single plot
abun_iNEXT <- iNEXT(abundance_list, datatype = 'abundance')
basic_gginext <- ggiNEXT(abun_iNEXT) + theme_classic()+
  theme(legend.position = 'bottom') +
  ggtitle(paste('BIN accumulation-rate of the', nsamples, 'samples sequenced by', download_date))+
  xlab(paste('Number of samples sequenced')) +
  ylab("BIN richness") # CHECK THAT THIS IS DEFINITELY WHAT IT SHOWS

basic_gginext
ggsave('figures/basic_gginext.jpeg', basic_gginext,
       width = 12)


# now, we throw the kitchen sink at the dataset
big_iNEXT <- iNEXT(abundance_list, datatype = 'abundance',
                   q = c(1,2,3))

# using the fortify command we can turn the iNEXT object into a happy friendly 
# dataframe, for easier analysis and plotting
fortified_iNEXT <- fortify(big_iNEXT)
# save the fortified object, as when we have lots of BINs the last few 
# commands will take a long time
write_csv(fortified_iNEXT, 'data/processed_data/fortified.csv')


# I've not yet written it, but analysis and plotting of fortified_iNEXT
# should happen in a different script, rather than having to repeatedly
# rerun the above code.