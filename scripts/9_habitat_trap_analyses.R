# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
all_arthropod_data <- read_csv(here('data', 'processed_data', 
                                    'bold_and_earthcape_combined.csv'))

habitat_data <- read_csv('data/raw_data/lot_habitat_classifications.csv')
