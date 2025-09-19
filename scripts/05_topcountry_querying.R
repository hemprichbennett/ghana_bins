library(tidyverse)
library(here)
library(countrycode)
library(bold)

our_big_df <- read_csv(here('data', 'processed_data', 'bold_public_bin_matches.csv'))%>%
  mutate(geographic_region = countrycode(sourcevar = country, 
                                         origin = "country.name",
                                         destination = "region")
  ) %>%
  filter(!is.na(geographic_region), 
         !is.na(order_name))


country_rankings <- our_big_df %>%
  select(country, bin_uri, geographic_region) %>%
  distinct() %>%
  filter(grepl('Africa', geographic_region)) %>%
  group_by(country) %>%
  summarise(`Number of shared BINs` = n()) %>%
  arrange(desc(`Number of shared BINs`)) %>%
  slice(1:20)


outdir <- 'data/processed_data/bold_queries'
if(dir.exists(outdir)){
  queried <- list.files(outdir, full.names = T) %>%
    file.info(extra_cols = F) %>%
    mutate(country = gsub('.+\\/|\\..+', '', rownames(.))) %>%
    # make a column stating if a file was created in the last
    # 10 days
    mutate(recently_queried = 
             difftime(Sys.time(), mtime, units = "days") < 10)
  exclude_from_query <- queried %>%
    filter(recently_queried == T) %>%
    pull(country)
}
if(!dir.exists(outdir)){
  dir.create(outdir)
}

# make a vector of our top countries AND all african countries
african_countries <- readr::read_lines('data/raw_data/african_countries.txt')

countries_to_query <- c(country_rankings$country, african_countries) %>%
  # remove duplicates
  unique()

country_list <- list()
for(chosen_country in countries_to_query){
  print(chosen_country)
  if(chosen_country %in% exclude_from_query){
    cat(chosen_country, ' has already been queried recently. Skipping\n')
    next()
  }
  cat('--------------------------\nTime is ', as.character(lubridate::now()), '\n', 'querying ', chosen_country, sep = '')
  country_list[[chosen_country]] <- bold_seqspec(geo = chosen_country)
  # some countries do not exist on BOLD, skip them if so
  if(length(country_list[[chosen_country]])< 2){
    if(is.na(country_list[[chosen_country]])){
      cat('\n', chosen_country, 'had no results\n')
      next()
    }
  
  }
  write_csv(country_list[[chosen_country]], paste0(outdir, '/', chosen_country, '.csv'))
  cat('\n', chosen_country, 'queried and saved\n')
}
