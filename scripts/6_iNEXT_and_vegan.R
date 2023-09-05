library(tidyverse)
library(iNEXT)
library(here)
library(vegan)

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

cat(ec_individuals %>% filter(is.na(Lot)) %>% nrow(), 
    'rows of ec_individuals have NA values!')

duplicate_transects <- ec_transects %>% 
  group_by(Name) %>% 
  summarise(n = n()) %>% 
  filter(n >1 ) %>% 
  pull(Name)
  
cat("Transects", duplicate_transects, 'all have multiple rows!')

ec_transects <- ec_transects %>%
  filter(!Name %in% duplicate_transects)

ec_individuals <- ec_individuals %>%
  filter(!is.na(Lot))

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


trap_transect_counts <- for_transects %>% 
  group_by(Type, Transect) %>% 
  summarise(n = n())

# we had a problem that bold seemed to have named the field id by our transects,
# but each transect has multiple individual sampling events on it. These
# samples were all from heath traps, however


n_heaths <- for_transects %>% 
  filter(Type == 'heath') %>% 
  group_by(Transect) %>% 
  summarise(n = n())

# get recorded instances of multiple heath traps per-transect

duplicate_heaths <- for_transects %>%
  filter(Type == 'heath') %>%
  group_by(Transect) %>%
  summarise(n_heaths = n()) %>%
  filter(n_heaths >1) %>%
  pull(Transect)

# if there are any duplicate heath traps, remove them

for_transects <- for_transects %>%
  # remove those for now, they can't be trusted
  filter(!Transect %in% duplicate_heaths)

# Combine bold data with the ec data ------------------------------------------------



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
  ),
  Type = str_to_title(Type))



# make a very basic summary plot
too_many_cols %>%
  filter(!is.na(order) & !is.na(Type)) %>%
  group_by(order, sampling_event, Type) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = order, y = nsamples)) +
  geom_boxplot() +
  facet_wrap(.~ Type, scales = 'free')


tib_for_inext <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  select(bin, order, sampling_event, Type) %>%
  group_by_all() %>%
  summarise(nsamples = n())

# only work on orders with at least the below number of bins
nbin_threshold <- 5

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
    
    nbin_threshold <- 30
    # if there are fewer than nbin_threshold unique BINs in this
    # trap type , discard the order, otherwise save it for analysis
    
    if(length(incidence_freq) >= nbin_threshold){
      for_inext_list[[trap_type]][[o]] <- c(n_events, incidence_freq)
    }
    
  }
  inext_objs[[trap_type]] <- iNEXT(for_inext_list[[trap_type]], 
                                  #q = c(0, 1, 2),
                                  q = 0, # get 'species' richness 
                                  # this is incidence_freq: we're analysing a dataset 
                                  # of how often a given BIN is detected at least once in 
                                  # a sample, not the number of occurrences of that BIN
                      datatype = 'incidence_freq',
                      size = round(seq(1,n_events*4, by = n_events/10)),
                      se=T)
  
  
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




# Big iNEXT plots ---------------------------------------------------------


big_inext_plotting <- function(input_list, inext_type){
  inext_tib <- map(input_list, function(x) fortify(x,type=inext_type)) %>% 
    bind_rows(.id = 'trap_type') %>%
    # reverse the factors in the 'Method' column, as their default alphabetical
    # order makes the plot legend confusing
    mutate(Method = fct(Method, 
                        levels = c('Observed', 'Rarefaction', 'Extrapolation')),
           # capitalise the trap types
           trap_type = str_to_title(trap_type))
  
  if(inext_type == 1){
    yaxis_text <- 'Number of BINs'
  }else if(
    inext_type ==2
  ){
    yaxis_text <- 'Sample coverage'
  }
  
  
  inext_plot <- ggplot(inext_tib, aes(x = x, y = y
  ))+
    geom_line(data = filter(inext_tib, Method %in% c('Rarefaction', 'Extrapolation')),
              mapping = aes(linetype=Method))+
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr), alpha=0.2)+
    geom_point(data = filter(inext_tib, Method == 'Observed'),
               mapping = aes(x = x, y = y))+
    facet_grid(Assemblage ~trap_type, 
               scales = 'free')+
    theme_bw()+
    theme(legend.position = 'bottom')+
    labs(x = 'Number of traps', y = yaxis_text)
  
  
  return(inext_plot)
  
}
## make a function of this, there's a silly amount of redundancy atm

type1_inext_plot <- big_inext_plotting(input_list = inext_objs,
                                       inext_type = 1)

ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.pdf'),
       type1_inext_plot,
       height = 15)

type1_inext_plot <- big_inext_plotting(input_list = inext_objs,
                                       inext_type = 1)

ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.pdf'),
       type1_inext_plot,
       height = 15)

type2_inext_plot <- big_inext_plotting(input_list = inext_objs,
                                       inext_type = 2)
ggsave(filename = here('figures', 'inext_plots', 'type2_inext_plot.pdf'),
       type2_inext_plot,
       height = 15)





# Further stuff -----------------------------------------------------------



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
  scale_colour_viridis_d()+
  facet_wrap(.~ Assemblage)+
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


# NMDS time ---------------------------------------------------------------

nmds_input_generator <- function(taxa_grouping, min_threshold = NA){
  acceptable_groupings <- c('order', 'family', 'genus', 'bin') 
  if(!taxa_grouping %in% acceptable_groupings){
    stop(cat('Error, ', taxa_grouping, 'is not in the list of accepted grouping levels (',
             acceptable_groupings,')\n'))
  }
  if(!is.numeric(min_threshold) & !is.na(min_threshold)){
    stop('min_threshold should either be blank, NA, or numeric')
  }
  # generate the output matrix
  out_mat <- too_many_cols %>%
    select(taxa_grouping, bin, Type) %>%
    filter_all(all_vars(!is.na(.))) %>%
    select(taxa_grouping, Type) %>%
    group_by_all() %>%
    summarise(abundance = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = taxa_grouping, values_from = abundance,
                # fill missing values with 0, not the default of NA
                values_fill = 0) %>%
    column_to_rownames(var="Type") %>%
    as.matrix(.)
  

  # if a minimum threshold of abundance for taxa_grouping was specified, 
  # remove columns whose sums are less than it
  if(is.numeric(min_threshold)){
    badcols <- which(colSums(out_mat) < min_threshold)
    out_mat <- out_mat[,-badcols]
  }
      
  return(out_mat)
}


family_nmds_dataset <- nmds_input_generator('family', min_threshold = 10)


big_nmds <- metaMDS(family_nmds_dataset, # Our community-by-species matrix
                                 k=2) # The number of reduced dimensions. Increase if high stress is problem. 
