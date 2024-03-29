library(tidyverse)
library(bold)

# read in the data from the previous script
in_df <- read_csv('data/processed_data/tidied_bold_data.csv')

# make a vector of the unique BINs
unique_bins <- in_df %>%
  filter(!is.na(bin)) %>%
  pull(bin) %>%
  unique(.)

# bold doesn't like being hit with thousands of queries at once, so we have to specify
# how many bins to query at once
n_simultaneous_queries <- 50

# we can then query this many items at once in a loop, saving
# the resulting dataframes into objects within a list, which we then
# combine later

# for ease, save the number of unique bins
n_total <- length(unique_bins)

# calculate the number of loop iterations needed
iterations <- ceiling(n_total / n_simultaneous_queries)

bold_list <- list()
for(i in 1:iterations){
  
  # calculate which positions in the unique_bins we
  # want to query in this iteration
  lower_bound <- ((i-1) * n_simultaneous_queries) + 1
  upper_bound <- i * n_simultaneous_queries
  
  # as the above maths can lead to us looking for 
  # bins at positions outside of the unique_bins
  # vector in the final loop iteration, check to see
  # if this is being done here, and rectify it if so
  if(upper_bound > n_total){upper_bound <- n_total}
  cat(i, lower_bound, upper_bound, '\n')
  
  # query bold
  bold_list[[i]] <- bold_seqspec(
    bin = unique_bins[lower_bound : upper_bound]
    )
}
