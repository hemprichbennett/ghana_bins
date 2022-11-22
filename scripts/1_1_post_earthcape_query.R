library(tidyverse)

csvs <- list.files('data/earthcape_query', pattern = '.csv',
                   full.names = T)

# read in data manually (for-now)
extracts <- read_csv('data/earthcape_query/DNAextract.csv')
units <- read_csv('data/earthcape_query/Unit.csv')
localities <- read_csv('data/earthcape_query/LocalityVisit.csv')

str(units)


bold_tidied <- read_csv('data/processed_data/our_organised_bold_data.csv')

# it looks like the data we have from earthcape relating
# to BOLD is metadata for sampling EVENTS, not samples.
units$Dataset
