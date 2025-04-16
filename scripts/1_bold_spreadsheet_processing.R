# Code for reading in and formatting basic data from BOLD
# Setup -------------------------------------------------------------------

# load packages
library(readxl)
library(iNEXT)
library(janitor)
library(taxize)
library(here)
library(tidyverse)



# Import and format BOLD data ---------------------------------------------


xl_paths <- list.files(path = 'data/raw_data', pattern = 'bold.xlsx', full.names = T, recursive = T)

download_date <- file.info('data/raw_data/GCEP/bold.xlsx')$ctime %>% 
  ymd_hms() %>%
  format(., "%d %B %Y")


# function for taking the horrible excel files and loads it in in
# a useable format

data_import <- function(xl_path){
  xl_path %>%
    excel_sheets() %>% 
    set_names() %>% 
    # read all the sheets in as individual list items
    map(read_excel, path = xl_path, skip = 2) %>%
    # join  them all into a master sheet
    reduce(., left_join) %>%
    clean_names() %>%
    # make the dates usable, rather than a string
    mutate(collection_date = dmy(collection_date))
}


# now read in the data, with each spreadsheet being a different list item
bold_list <- lapply(xl_paths, data_import)

# combine the list items into a single dataframe
bold_data <- bind_rows(bold_list) %>%
  # replace all hyphens with underscores in field IDs
  mutate(field_id = gsub('-', '_', field_id))



# get the formats of the field IDs that will need pairing to earthcape

bold_data <- bold_data %>%
  mutate(sample_naming_convention = 'unknown')

# trial samples, which aren't to be used for any actual analyses
bold_data %>%
  filter(grepl(' ', field_id)) %>%
  pull(field_id)

bold_data <- bold_data %>%
  mutate(sample_naming_convention = ifelse(
    grepl(' ', field_id), 'trial_sample', sample_naming_convention)
    )

# the patterns of individual IDs on earthcape. E.g. TI_RL_35515_G09
ec_individual_pattern_1 <- '[A-Z]{2}_[A-Z]{2}_[0-9]{5}_[A-Z][0-9]{2}'

bold_data %>%
  filter(grepl(ec_individual_pattern_1, field_id)) %>%
  pull(field_id)


bold_data <- bold_data %>%
  mutate(sample_naming_convention = ifelse(
    grepl(ec_individual_pattern_1, field_id), 'ec_individual_type1', sample_naming_convention)
  )

# the second type of pattern on earthcape individuals. E.g. CRAG_TI_1375
ec_individual_pattern_2 <- 'CRAG_TI_[0-9]{1,4}'

bold_data %>%
  filter(grepl(ec_individual_pattern_2, field_id)) %>%
  pull(field_id)


bold_data <- bold_data %>%
  mutate(sample_naming_convention = ifelse(
    grepl(ec_individual_pattern_2, field_id), 'ec_individual_type2', sample_naming_convention)
  )

# the main pattern for earthcape transects. E.g. 2_AA_NW_8
ec_transect_pattern1 <- '[0-9]_[A-Z]{2}_[A-Z]{2}_[0-9]'

bold_data %>%
  filter(grepl(ec_transect_pattern1, field_id)) %>%
  pull(field_id)


bold_data <- bold_data %>%
  mutate(sample_naming_convention = ifelse(
    grepl(ec_transect_pattern1, field_id), 'ec_transect1', sample_naming_convention)
  )

# the secondary pattern for earthcape transects. E.g. MA02
ec_transect_pattern2 <- '[A-Z]{2}[0-9]{2}'

bold_data %>%
  filter(grepl(ec_transect_pattern2, field_id)) %>%
  pull(field_id)


bold_data <- bold_data %>%
  mutate(sample_naming_convention = ifelse(
    grepl(ec_transect_pattern2, field_id), 'ec_transect2', sample_naming_convention)
  )


# unmatched field IDs
bold_data %>%
  filter(sample_naming_convention == 'unknown') %>%
  pull(field_id)

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



# Read in and format earthcape data ---------------------------------------


ec_individuals <- read_csv(
  here('data', 'earthcape_app_query', 'Individuals.csv')) %>%
  janitor::clean_names()

ec_lots <- read_csv(
  here('data', 'earthcape_app_query', 'Lots.csv')) %>%
  mutate(Type = tolower(Type)) %>%
  janitor::clean_names()

ec_transects <- read_csv(
  here('data', 'earthcape_app_query', 'Transects.csv'))%>%
  janitor::clean_names()



# Combine BOLD and earthcape data -----------------------------------------


bold_data <- bold_data %>%
  mutate(sampling_protocol = gsub('Heath Trap', 'heath', sampling_protocol),
         # convert all hyphens in 'field_id' to underscores
         field_id = gsub('-', '_', field_id),
         # for internal reasons, BOLD gave some of the field IDs a 'B' suffix.
         # this never exists in our data, and messes up the matching of those 
         # samples to our lot ID or transect ID. Remove them
         field_id = gsub('B$', '', field_id)
  ) 




str(bold_data)
unique(bold_data$field_id)
bold_data$field_id %in% ec_individuals$unit_id

# make a dataframe with a column saying if the field_id value was found in
# individuals or transects
ec_referenced <- bold_data %>%
  mutate(in_individuals = field_id %in% ec_individuals$unit_id,
         in_transects = field_id %in% ec_transects$name)


# now a dataframe containing samples that matched both earthcape export-types
duplicate_matches <- ec_referenced %>%
  filter(in_individuals == T & in_transects == T)

# Annoyingly in the data there seems to be a mix of hyphens and underscores
# used as delimiters for transect names. They're mostly paired fine, but BOLD
# has samples with field_id '2-MA-NE-2' but earthcape calls it '2_MA_NE_2'
# 
# ec_referenced <- ec_referenced %>% 
#   mutate(better_field_id = 
#            # if the field_id is in neither individuals or transects
#            ifelse(in_individuals == F & in_transects == F, 
#                   # replace any hyphens with underscores
#                   gsub('-', '_', field_id),
#                   # else, return field_id unedited
#                   field_id)
#          )
str(ec_individuals)
str(ec_transects)
str(ec_lots)

cat(ec_individuals %>% filter(is.na(lot)) %>% nrow(), 
    'rows of ec_individuals have NA values!')

# make a dataframe just containing samples that were missing
unmatched <- ec_referenced %>%
  filter(in_individuals == F & in_transects == F)

unmatched_field_ids <- unmatched %>%
  group_by(field_id, project_code) %>%
  summarise(nsamples = n())

write_csv(unmatched_field_ids, file = here('data', 'processed_data', 
                                           'unmatched_field_ids.csv'))



duplicate_transects <- ec_transects %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  filter(n >1 ) %>% 
  pull(name)

cat("Transects", duplicate_transects, 'all have multiple rows!')

ec_transects <- ec_transects %>%
  filter(!name %in% duplicate_transects) %>%
  select(-starts_with('date_'))

ec_individuals <- ec_individuals %>%
  filter(!is.na(lot))

# The transects AND lots have date stamps and GPS coordinates, as there are 
# multiple 'lots' per-transect. A transect is a set point from A to B, but 
# there were multiple 'lots' (traps) set up along each. 


for_individuals <- ec_individuals %>%
  left_join(ec_lots, by = c("lot" = "lot_id")) %>%
  left_join(ec_transects, by = c("transect" = "name")) %>%
  select(lot, unit_id, transect, latitude.x, longitude.x,
         date, 
         direction,
         locality.x, 
         type)

for_transects <- ec_lots %>%
  rename(lot = lot_id) %>%
  left_join(ec_transects, by = c("transect" = "name")) %>%
  select(lot, transect, latitude.y, longitude.y,
         date, direction, locality.x, type)


individual_referenced <- ec_referenced %>%
  filter(in_individuals == T) %>%
  left_join(for_individuals, by = c("field_id" = "unit_id"))

transect_referenced <- ec_referenced %>%
  filter(in_transects == T) %>%
  left_join(for_transects, by = c("field_id" = "transect", 
                                  "sampling_protocol" = "type"))


trap_transect_counts <- for_transects %>% 
  group_by(type, transect) %>% 
  summarise(n = n())

# we had a problem that bold seemed to have named the field id by our transects,
# but each transect has multiple individual sampling events on it. These
# samples were all from heath traps, however


n_heaths <- for_transects %>% 
  filter(type == 'heath') %>% 
  group_by(transect) %>% 
  summarise(n = n())

# get recorded instances of multiple heath traps per-transect

duplicate_heaths <- for_transects %>%
  filter(type == 'heath') %>%
  group_by(transect) %>%
  summarise(n_heaths = n()) %>%
  filter(n_heaths >1) %>%
  pull(transect)

# if there are any duplicate heath traps, remove them

for_transects <- for_transects %>%
  # remove those for now, they can't be trusted
  filter(!transect %in% duplicate_heaths)

# Combine bold data with the ec data ------------------------------------------------



individual_referenced <- ec_referenced %>%
  filter(in_individuals == T) %>%
  left_join(for_individuals, by = c("field_id" = "unit_id"))

transect_referenced <- ec_referenced %>%
  filter(in_transects == T) %>%
  left_join(for_transects, by = c("field_id" = "transect", 
                                  "sampling_protocol" = "type")) %>%
  rename('type'= 'sampling_protocol')





# if we ignore the issue of duplicate heaths for now, 
# we can make a big df of all the earthcape-matched data
too_many_cols <- bind_rows(individual_referenced, transect_referenced)

# make a tibble of only rows from this that contain no trap type (so insects
# which were not paired with a trap on earthcape)
unpaired_insects <- too_many_cols %>%
  filter(is.na(type))


# as we know that some heath samples from BOLD weren't paired with a lot but 
# with a transect, we know that they'll have an NA for the 'Lot' column,
# but the transect will only have a single Heath trap used, so we can use 
# the transect as an identifier instead

too_many_cols <- too_many_cols %>%
  mutate(sampling_event = ifelse(
    # if there is no value for 'Lot' and it's a heath sample
    is.na(lot) & type == "heath",
    # use the Transect name from the field_id instead)
    field_id,
    # else use the lot, as its fine
    lot)) %>%
  # same for Type
  mutate(type = ifelse(
    is.na(type) & sampling_protocol == "heath",
    sampling_protocol,
    type
  ),
  type = str_to_title(type))

# Save the huge df for reuse in further scripts
write_csv(too_many_cols, 
          file = here('data', 'processed_data', 
                      'bold_and_earthcape_combined.csv'))


# Malaise trap debugging --------------------------------------------------

malaise_trap_data <- too_many_cols %>% 
  filter(type == 'Malaise') %>%
  mutate(overall_lot = gsub('\\..+', '', lot))

malaise_trap_sample_counts <- malaise_trap_data %>% 
  group_by(overall_lot) %>%
  summarise(n_arthropods = n())

ggplot(malaise_trap_sample_counts, aes(x = n_arthropods))+
  geom_histogram()

dubious_malaise_trap_lots <- c(1000,1011,1099,567,577,586,604,835)

trap_1 <- too_many_cols %>% filter(grepl(1000, lot)) %>%
  select(lot, date, order, family, genus, species, field_id, transect) %>%
  arrange(lot)

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
ggsave('figures/sample_taxonomy.png', width = 14)


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
ggsave('figures/site_taxonomy.png', width = 14)


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
  filter(nsamples > 500) %>%
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
ggsave('figures/basic_gginext.png', basic_gginext,
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