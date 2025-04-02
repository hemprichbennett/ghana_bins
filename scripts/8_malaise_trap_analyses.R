library(tidyverse)
library(here)

# read in data
malaise_trap_deployments <- read_csv(here('data', 'processed_data', 
                                          'temp_malaise_traps_edited_times.csv'))

all_arthropod_data <- read_csv(here('data', 'processed_data', 
                                    'bold_and_earthcape_combined.csv'))