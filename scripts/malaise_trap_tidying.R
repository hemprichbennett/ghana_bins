
# fix earthcape malaise trap data

library(tidyverse)
library(here)

ec_lots <- read_csv(
  here('data', 'earthcape_app_query', 'Lots.csv')) 

malaise_trap_data <- ec_lots %>%
  filter(Type == 'Malaise') %>%
  mutate(
    # get the time increment from the lot ID. for example, for 989.0 it should 
    # be 0, for 989.1 it should be 1, etc, 989.2 it should be 2, etc
    time_increment = gsub('.+\\.', '', `Lot ID`),
    time_increment = as.numeric(time_increment),
    # due to the underlying formatting, for e.g. 989.0 we actually received 989,
    # not 0, as those were integers imported within a column of doubles. Make 
    # any value greater than 4 into a 0
    time_increment = ifelse(time_increment > 4, 0, time_increment),
    # format the 'date' column as a datetime object, for use later
    Date = dmy_hm(Date),
    # get the overall lot ID (e.g. 989, for the above)
    overall_lot = gsub('\\..+', '', `Lot ID`)
  
    ) 

# not all overall lots have multiple sub-lots, and we only want to modify those
# that do. Get a vector of all the lots TO modify

overall_lots_to_modify <- malaise_trap_data %>%
  group_by(overall_lot) %>%
  summarise(n_sublots = n()) %>%
  filter(n_sublots >1) %>%
  pull(overall_lot)

malaise_trap_data_for_modification <- malaise_trap_data %>%
  # make a list of each of the lots that we want to modify, with each overall lot
  # being a list item
  filter(overall_lot %in% overall_lots_to_modify) %>%
  group_split(overall_lot)

out_list <- list()
bad_vec <- c()
a <- 1
#for(df in malaise_trap_data_for_modification){
for(i in 1:length(malaise_trap_data_for_modification)){

  df <- malaise_trap_data_for_modification[[i]] %>%
    arrange(`Lot ID`)
  out_df <- df
  
  # if there was no time attached to Date, the hour will show as 'zero'. 
  # We want to skip these ones as there's no ability to tell when in the day
  # any of the bottles were deployed. They'll also give us an error if we try 
  # to run the code for calculating the exact times of each bottle
  if(unique(hour(df$Date)) == 0){
    out_df$bottle_start_time <- NA
    out_df$bottle_end_time <- NA
    
    cat('issues with', unique(df$overall_lot), '\n')
    bad_vec[a] <- unique(df$overall_lot)
    a <- a+1
  }else{
    
    # make a series of date-time objects, starting with the 'date' field for all
    # of the rows, then going forwards in 6-hour increments (e.g. midday, 6pm, midnight...)
    out_df$bottle_start_time <- c(out_df$Date[1], 
                                  seq(ceiling_date(out_df$Date[1],
                                                   '6 hours'),
                                      out_df$Date[1]+ days(1), by = '6 hours'))
    
    # make a series of date-time objects starting at the first 6-hour increment 
    # after the trap was deployed (e.g. midday, 6pm, midnight), finishing 24 hours
    # after the first bottle's deployment time
    out_df$bottle_end_time <- c(seq(ceiling_date(out_df$Date[1],
                                                 '6 hours'),
                                    out_df$Date[1]+ days(1), by = '6 hours'),
                                out_df$Date[1]+ days(1))
  }
  
  
  out_list[[i]] <- out_df
}

final_malaise_trap_df <- bind_rows(out_list)

write_csv(final_malaise_trap_df, 
          file = 'data/processed_data/temp_malaise_traps_edited_times.csv')

write_csv(as_tibble(bad_vec),
          file = 'data/processed_data/missing_malaise_trap_times.csv')
