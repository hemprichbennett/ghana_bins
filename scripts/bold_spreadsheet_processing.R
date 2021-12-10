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


# Read in the data

# this assumes that the input data has had the redundant first two rows
# (eyeroll) of the raw data on each spreadsheet removed
xl_paths <- list.files(path = 'data/processed_data/', pattern = '*tidied*', full.names = T, recursive = T)

download_date <- file.info('data/raw_data/')$ctime %>% 
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

# (for plotting later) make a string giving the number of samples which we have site information
# for. 
nsamples <- filter(bold_data, !is.na(exact_site)) %>% 
  nrow(.) %>% 
  # Format it with commas, to make it more human-readable
  format(., big.mark = ',')

# Basic plotting ----------------------------------------------------------



# make a simple plot of the number of SAMPLES over time
collection_date_histogram <- ggplot(filter(bold_data, !is.na(exact_site)), aes(x = collection_date)) + 
  geom_histogram() +
  facet_wrap(.~ exact_site) +
  theme_bw() +
  theme(text = element_text(size = 20))+
  ggtitle(paste('Collection dates of the', nsamples, 'samples with metadata sequenced by', download_date))+
  xlab('Collection date') + 
  ylab('Number of samples sequenced')
# show it
collection_date_histogram

# save it
ggsave('figures/collection_date_histogram.jpeg', collection_date_histogram,
       width = 10)


# make a plot of the number of samples of each taxonomic order sequenced
# so far
bold_data %>%
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

# for the orders, make a list of their frequencies
abundance_list <- list()
for(chosen_order in unique(order_bin_frequencies$order)){
  
  abundance_vec <- order_bin_frequencies %>%
    filter(order == chosen_order) %>%
    pull(n)
  cat(chosen_order, 'contains', length(abundance_vec), 'different BINs\n')
  # if the Order only contains less than 10 BINs, abandon it
  if(length(abundance_vec) >= 10){
    abundance_list[[chosen_order]] <-abundance_vec 
  }
  
}


# make the iNEXT objects --------------------------------------------------


# a basic iNEXT object with all items on a single plot
abun_iNEXT <- iNEXT(abundance_list, datatype = 'abundance')
basic_gginext <- ggiNEXT(abun_iNEXT) + theme_classic()+
  theme(legend.position = 'bottom') + 
  ggtitle(paste('Samples with sequencing data available on', download_date))+
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