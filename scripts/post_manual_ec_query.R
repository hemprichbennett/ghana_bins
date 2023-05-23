library(tidyverse)
library(iNEXT)

ec_individuals <- read_csv('data/earthcape_app_query/Individuals.csv')
ec_lots <- read_csv('data/earthcape_app_query/Lots.csv') %>%
  mutate(Type = tolower(Type))
ec_transects <- read_csv('data/earthcape_app_query/Transects.csv')
ec_units <- read_csv('data/earthcape_app_query/Units.csv')

bold_organised <- read_csv('data/processed_data/our_organised_bold_data.csv') %>%
  mutate(sampling_protocol = gsub('Heath Trap', 'heath', sampling_protocol))

str(bold_organised)
unique(bold_organised$field_id)
bold_organised$field_id %in% ec_individuals$`Unit ID`

ec_referenced <- bold_organised %>%
  mutate(in_individuals = field_id %in% ec_individuals$`Unit ID`,
         in_transects = field_id %in% ec_transects$Name)

unmatched <- ec_referenced %>%
  filter(in_individuals == F & in_transects == F)

duplicate_matches <- ec_referenced %>%
  filter(in_individuals == T & in_transects == T)

# Annoyingly in the data there seems to be a mix of hyphens and underscores
# used as delimiters for transect names. They're mostly paired fine, but BOLD
# has samples with field_id '2-MA-NE-2' but earthcape calls it '2_MA_NE_2'

ec_referenced <- ec_referenced %>% 
  mutate(better_field_id = 
           # if the field_id is in neither individuals or transects
           ifelse(in_individuals == F & in_transects == F, 
                  # replace any hyphens with underscores
                  gsub('-', '_', field_id),
                  # else, return field_id unedited
                  field_id)
         )
str(ec_individuals)
str(ec_transects)
str(ec_lots)

for_individuals <- ec_individuals %>%
  left_join(ec_lots, by = c("Lot" = "Lot ID")) %>%
  left_join(ec_transects, by = c("Transect" = "Name")) %>%
  select(Lot, `Unit ID`, Transect, Latitude.y, Longitude.y,
         `Date Time Start`, `Date Time End`, Direction, Locality, Type)

for_transects <- ec_lots %>%
  rename(Lot = `Lot ID`) %>%
  left_join(ec_transects, by = c("Transect" = "Name")) %>%
  select(Lot, Transect, Latitude.y, Longitude.y,
         `Date Time Start`, `Date Time End`, Direction, Locality, Type)


individual_referenced <- ec_referenced %>%
  filter(in_individuals == T) %>%
  left_join(for_individuals, by = c("field_id" = "Unit ID"))

transect_referenced <- ec_referenced %>%
  filter(in_transects == T) %>%
  left_join(for_transects, by = c("field_id" = "Transect", 
                                  "sampling_protocol" = "Type"))




# we have a problem: bold seem to have named the field id by our transects,
# but each transect has multiple individual sampling events on it. These
# samples all seem to be from heath traps, however


n_heaths <- for_transects %>% 
  filter(Type == 'heath') %>% 
  group_by(Transect) %>% 
  summarise(n = n())
# we can work around this though, as there was only two recorded instances of
# a transect having two heath traps

duplicate_heaths <- for_transects %>%
  filter(Type == 'heath') %>%
  group_by(Transect) %>%
  summarise(n_heaths = n()) %>%
  filter(n_heaths >1) %>%
  pull(Transect)


for_transects <- for_transects %>%
  # remove those for now, they can't be trusted
  filter(!Transect %in% duplicate_heaths)

# Combine with the ec data ------------------------------------------------



individual_referenced <- ec_referenced %>%
  filter(in_individuals == T) %>%
  left_join(for_individuals, by = c("field_id" = "Unit ID"))

transect_referenced <- ec_referenced %>%
  filter(in_transects == T) %>%
  left_join(for_transects, by = c("field_id" = "Transect", 
                                  "sampling_protocol" = "Type")) %>%
  rename('Type'= 'sampling_protocol')





# if we ignore the issue of duplicate heaths for now, 
# we can make a big df of all the earthcape-matched data
too_many_cols <- bind_rows(individual_referenced, transect_referenced)

# as we know that some heath samples from BOLD weren't paired with a lot but 
# with a transect, we know that they'll have an NA for the 'Lot' column,
# but the transect will only have a single Heath trap used, so we can use 
# the transect as an identifier instead

too_many_cols <- too_many_cols %>%
  mutate(sampling_event = ifelse(
    # if there is no value for 'Lot' and it's a heath sample
    is.na(Lot) & Type == "heath",
    # use the Transect name from the field_id instead)
    field_id,
    # else use the lot, as its fine
    Lot)) %>%
    # same for Type
  mutate(Type = ifelse(
    is.na(Type) & sampling_protocol == "heath",
    sampling_protocol,
    Type
  ))



# make a very basic summary plot
too_many_cols %>%
  filter(!is.na(order)) %>%
  group_by(order, sampling_event, Type) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = order, y = nsamples)) +
  geom_boxplot() +
  facet_wrap(.~ Type)


tib_for_inext <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  select(bin, order, sampling_event, Type) %>%
  group_by_all() %>%
  summarise(nsamples = n())

# only work on orders with at least the below number of bins
nbin_threshold <- 20

desired_orders <- tib_for_inext %>%
  filter(!is.na(order)) %>%
  group_by(order) %>%
  summarise(bin_richness = length(unique(bin))) %>%
  filter(bin_richness >= nbin_threshold) %>%
  pull(order)

traptypes <- tib_for_inext %>%
  filter(!is.na(Type)) %>%
  filter(Type != 'cdc') %>%
  pull(Type) %>%
  unique()
for_inext_list <- list()
inext_objs <- list()
inext_plots <- list()
inext_plots[['completeness']] <- list()
inext_plots[['extrapolation']] <- list()
for(trap_type in traptypes){
  #cat(o, '\n')
  for_inext_list[[trap_type]] <- list()
  n_events <- tib_for_inext %>%
    filter(Type == trap_type) %>%
    pull(sampling_event) %>%
    unique() %>%
    length()
  for(o in desired_orders){
    cat('trap type is ', trap_type, '\n')
    
    cat('order is ', o, 'trap type is ', trap_type, ' number of sampling events was', n_events, '\n')
    incidence_freq <- tib_for_inext %>%
      filter(Type == trap_type) %>%
      filter(order == o) %>%
      group_by(bin) %>%
      summarise(freq = n())%>%
      pull(freq) %>%
      sort(decreasing = T)
    # if there are fewer than ten unique BINs in this
    # trap type , discard the order, otherwise save it for analysis
    if(length(incidence_freq) >= 15){
      for_inext_list[[trap_type]][[o]] <- c(n_events, incidence_freq)
    }
    
  }
  inext_objs[[trap_type]] <- iNEXT(for_inext_list[[trap_type]], 
                                  #q = c(0, 1, 2),
                                  q = 0, # get 'species' richness 
                      datatype = 'incidence_freq',
                      size = round(seq(1,n_events*4, by = n_events/10)),
                      se=FALSE)
  
  
  # Plot completeness
  inext_plots[['completeness']][[trap_type]] <- ggiNEXT(inext_objs[[trap_type]], type=2, 
                                      color.var="Assemblage",
                                      se = F) +
    theme_bw(base_size = 18) +
    theme(legend.position="bottom",
          legend.box = "vertical") + 
    ggtitle(trap_type) 
  ggsave(paste0('figures/inext_plots/completeness_', trap_type, '.pdf'), 
         inext_plots[['completeness']][[trap_type]],
         width = 8)
  
  
  # plot interpolation/extrapolation
  inext_plots[['extrapolation']][[trap_type]] <- ggiNEXT(inext_objs[[trap_type]], type=3, 
                                                         color.var="Assemblage",
                                                        se = F) +
    theme_bw(base_size = 18) +
    theme(legend.position="bottom",
          legend.box = "vertical") + 
    ylab("BIN diversity")+ 
    ggtitle(trap_type) 
  ggsave(paste0('figures/inext_plots/extrapolation_', trap_type, '.pdf'), 
         inext_plots[['extrapolation']][[trap_type]],
         width = 8)
}
# todo: make inext plots for number of sampling visits

# grouping by the date-time start. Is this correct?
visit_inext_tib <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  rename(date =`Date Time End`) %>%
  filter(!is.na(date)) %>%
  select(bin, order, date) %>%
  group_by_all() %>%
  summarise(nsamples = n())

n_dates <- visit_inext_tib %>%
  pull(date) %>%
  unique(.) %>%
  length()

visit_inext_list <- list()
for(o in unique(visit_inext_tib$order)){
  incidence_freq <- visit_inext_tib %>%
    filter(order == o) %>%
    group_by(bin) %>%
    summarise(freq = n())%>%
    pull(freq) %>%
    sort(decreasing = T)
  # if there are fewer than 15 unique BINs in this
  # trap type , discard the order, otherwise save it for analysis
  if(length(incidence_freq) >= 15){
    visit_inext_list[[o]] <- c(n_dates, incidence_freq)
  }
  
}

visit_inext <- iNEXT(visit_inext_list, 
      #q = c(0, 1, 2),
      q = 0, # get 'species' richness 
      datatype = 'incidence_freq',
      size = round(seq(1,n_dates*4, by = n_dates/10)),
      se=FALSE)

visit_inext_plot <- ggiNEXT(visit_inext, type=2, 
                            color.var="Assemblage",
                            se = F) +
  xlab('Number of visits') +
  theme_bw(base_size = 18) +
  theme(legend.position="bottom",
        legend.box = "vertical")
visit_inext_plot
ggsave('figures/inext_plots/overall_visits.pdf', visit_inext_plot)
# estimate of 'species' richness
ChaoRichness(visit_inext_list, datatype = 'incidence_freq')



# the number of samples classed to BIN/ not classed to BIN

samples_with_bins <- bold_organised %>% 
  mutate(has_bin = !is.na(bin)) %>%
  group_by(order, has_bin) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = has_bin, values_from = n, values_fill = 0)

write_csv(samples_with_bins, 'results/samples_with_bins.csv')
