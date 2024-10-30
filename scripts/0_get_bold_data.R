library(BOLDconnectR)
library(dplyr)
library(readr)

# read in your private BOLD api key, here saved as a string named 'bold_key' in
# the file ./bold_key.R
# (see https://github.com/boldsystems-central/BOLDconnectR for instructions on 
# getting one)
source('bold_key.R')
bold.apikey(bold_key)


# download data from each project
projects <- c('GCEP', 'TMGHA', 'TGMHB')

bold_list <- list()
for(p in projects){
  bold_list[[p]] <- bold.fetch(get_by = "project_codes",
                               identifiers = p)
}

# combine into one tibble
bold_df <- bind_rows(bold_list)

write_csv(bold_df, 'data/raw_data/bold_query.csv')
