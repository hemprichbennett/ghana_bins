# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(vegan)
library(gridExtra)

habitat_data <- read_csv('data/raw_data/lot_habitat_classifications.csv') %>%
  janitor::clean_names() %>%
  rename(lot = name, habitat_type = name_2) %>%
  select(lot, habitat_type) %>%
  mutate(habitat_type = gsub('^Natural$', 'Semi-natural', habitat_type))

source(here('parameters.R'))

malaise_trap_metadata <- read_csv(here('data', 'processed_data',
                                       'malaise_trap_metadata.csv'))

too_many_cols <- read_csv(, 
                          file = here('data', 'processed_data', 
                                      'bold_and_earthcape_combined.csv')) %>%
  rename(trap_type = type)

too_many_cols <- left_join(too_many_cols, habitat_data)

# check for lots which were not matched
unmatched_rows <- anti_join(too_many_cols, habitat_data)

unmatched_rows$lot

# Functions ---------------------------------------------------------------

nmds_input_generator <- function(taxa_grouping, min_taxa_threshold = NA,
                                 min_trap_threshold = NA){
  acceptable_groupings <- c('order', 'family', 'genus', 'bin') 
  if(!taxa_grouping %in% acceptable_groupings){
    stop(cat('Error, ', taxa_grouping, 'is not in the list of accepted grouping levels (',
             acceptable_groupings,')\n'))
  }
  if(!is.numeric(min_taxa_threshold) & !is.na(min_taxa_threshold)){
    stop('min_taxa_threshold should either be blank, NA, or numeric')
  }
  # generate the output matrix
  out_mat <- too_many_cols %>%
    select(taxa_grouping, bin,  sampling_event) %>%
    filter_all(all_vars(!is.na(.))) %>%
    select(taxa_grouping, sampling_event) %>%
    group_by_all() %>%
    summarise(abundance = n()) %>%
    ungroup() %>%
    mutate(abundance = as.numeric(abundance)) %>%
    pivot_wider(names_from = taxa_grouping, values_from = abundance,
                # fill missing values with 0, not the default of NA
                values_fill = 0) %>%
    column_to_rownames(var="sampling_event") %>%
    as.matrix(.)
  
  trap_types <- too_many_cols %>%
    select(sampling_event, trap_type, habitat_type) %>%
    distinct() %>%
    slice(order(factor(sampling_event, levels = rownames(out_mat))))
  
  # if a minimum threshold of abundance for taxa_grouping was specified, 
  # remove columns whose sums are less than it
  if(is.numeric(min_taxa_threshold)){
    badcols <- which(colSums(out_mat) < min_taxa_threshold)
    out_mat <- out_mat[,-badcols]
  }
  
  # if any trap is empty (it has a row sum of 0), remove it
  if(0 %in% rowSums(out_mat)){
    out_mat <- out_mat[- which(rowSums(out_mat) == 0),]
  }
  
  if(is.numeric(min_trap_threshold)){
    badrows <- which(rowSums(out_mat) < min_trap_threshold)
    out_mat <- out_mat[-badrows,]
  }
  
  # remove from the trap_types object any rows which no longer have a 
  # corresponding value in the output matrix, they mess up analyses later
  trap_types <- trap_types %>%
    filter(sampling_event %in% rownames(out_mat))
  
  return(list(trap_matrix = out_mat, 
              trap_types = trap_types,
              taxa_grouping = taxa_grouping))
}



# function to do an NMDS and ggplot on a dataset made with the above function
nmds_analysis <- function(input_list, k = 2, min_tries = 20, max_tries = 20,
                          malaise_analysis = F){
  
  dist_mat <- vegdist(input_list$trap_matrix)
  
  big_nmds <- metaMDS(dist_mat, # Our community-by-species matrix
                      k=k,# The number of reduced dimensions. Increase if high stress is problem. 
                      try = min_tries, 
                      trymax = max_tries) 
  
  
  # make dataframes for plotting
  site.scores <- as.data.frame(scores(big_nmds, "site"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  site.scores$sampling_event <- as.numeric(rownames(site.scores))  # create a column of site names, from the rownames of data.scores
  site.scores <- site.scores %>%
    left_join(input_list$trap_types) %>%
    mutate(taxa_grouping = input_list$taxa_grouping)
  
  
  species.scores <- as.data.frame(scores(big_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
  
  trap_centroid <- site.scores %>%
    group_by(trap_type) %>%
    summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))%>%
    mutate(taxa_grouping = input_list$taxa_grouping)
  
  habitat_centroid <- site.scores %>%
    group_by(habitat_type) %>%
    summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))%>%
    mutate(taxa_grouping = input_list$taxa_grouping)
  
  # if this is an analysis of malaise-trap data ONLY, we have temporal data
  # that will also need analysing and returning
  if(malaise_analysis == T){
    time_centroid <- site.scores %>%
      group_by(coarse_timing) %>%
      summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))
    
    return(list(scores = site.scores, 
                nmds_analysis = big_nmds,
                trap_centroid = trap_centroid,
                habitat_centroid = habitat_centroid,
                time_centroid = time_centroid,
                dist_mat = dist_mat))
  }else{
    # if it isn't malaise-trap data, return all of the standard required objects
    return(list(scores = site.scores, 
                nmds_analysis = big_nmds,
                trap_centroid = trap_centroid,
                habitat_centroid = habitat_centroid,
                dist_mat = dist_mat))
  }

  
}

nmds_plot <- function(input_list, title_str = NA, viridis_option = "D",
                      plot_by, all_text_size = 30){
  if(!plot_by %in% c('trap_type', 'habitat_type', 'coarse_timing')){
    stop('Acceptable plot_by variables are trap, habitat and malaise timing')
  }
  
  plot_by <- sym(plot_by)
  
  if(plot_by == 'trap_type'){
    centroids_to_use <- input_list$trap_centroid
    legend_str = "Trap type"
  }else if(plot_by == 'habitat_type'){
    centroids_to_use <- input_list$habitat_centroid
    legend_str = "Habitat type"
  }else if(plot_by == 'coarse_timing'){
    centroids_to_use <- input_list$time_centroid
    legend_str = 'Time'
  }
  
  out_plot <- ggplot(data=input_list$scores,
                     aes(
    x=NMDS1,
    y=NMDS2,
    group=!!plot_by,
    shape=!!plot_by)) + 
    stat_ellipse(show.legend=FALSE) +
    geom_point(aes(colour=!!plot_by)) + # add the point markers
    # add the centroid data
    geom_point(data=centroids_to_use, size=5,# color="black",
               aes(colour=!!plot_by, fill = !!plot_by, shape=!!plot_by), show.legend=FALSE)+
    scale_colour_viridis_d(option = viridis_option)+
    scale_fill_viridis_d(option = viridis_option)+
    #geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=sampling_event),size=6,vjust=0) +  # add the site labels
    #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
    #coord_equal() +
    theme_bw()+
    theme(legend.position = 'bottom',
          text=element_text(size=all_text_size))+
    labs(colour = legend_str, shape = legend_str)+
    # increase point size in legend
    guides(colour = guide_legend(override.aes = list(size=10)))
  
  if(!is.na(title_str)){
    out_plot <- out_plot + 
      ggtitle(title_str)
  }
  return(out_plot)
}

# A quick plot of the orders found in each trap type ----------------------

trap_sample_counts <- too_many_cols %>%
  group_by(trap_type) %>%
  # the number of samples in a given trap type, independent of taxonomy
  summarise(trap_total_nsamples = n())

taxa_sample_counts <- too_many_cols %>%
  select(order, trap_type) %>%
  group_by(order, trap_type) %>%
  summarise(n_samples_per_taxa = n())

trap_taxonomy_for_plotting <- taxa_sample_counts %>%
  left_join(trap_sample_counts) %>%
  mutate(percent_of_trap_abundance = n_samples_per_taxa / trap_total_nsamples * 100) %>%
  filter(!is.na(trap_type),
         !is.na(order))

ggplot(trap_taxonomy_for_plotting, 
       aes(y = fct_rev(order), x = trap_type, fill = percent_of_trap_abundance))+
  theme_bw()+
  geom_tile(colour = 'white')+
  scale_fill_gradient2(low = "white", mid = "blue",
                       high = "black", midpoint = 0.5, 
                       name ='Percent of\nsamples containing')

# make a table of the same values
trap_taxonomy_table <- trap_taxonomy_for_plotting %>%
  select(order, trap_type, percent_of_trap_abundance) %>%
  mutate(percent_of_trap_abundance = round(percent_of_trap_abundance,
                                           digits = 2)) %>%
  pivot_wider(names_from = trap_type, values_from = percent_of_trap_abundance)

write_csv(trap_taxonomy_table, file = here('results', 'manuscript_tables',
                                           'trap_taxonomy_percentage.csv'))

# NMDS analyses ---------------------------------------------------------------
nmds_inputs <- list()
nmds_outputs <- list()
nmds_trap_plots <- list()
nmds_habitat_plots <- list()
taxonomic_levels <- c('order', 'family', 'genus', 'bin')
for(current_taxa in taxonomic_levels){
  print(current_taxa)
  nmds_inputs[[current_taxa]] <- nmds_input_generator(current_taxa, 
                                                      min_taxa_threshold = nmds_inclusion_threshold,
                                                      min_trap_threshold = nmds_inclusion_threshold)
  
  nmds_outputs[[current_taxa]] <- nmds_analysis(nmds_inputs[[current_taxa]], 
                                                min_tries = 20,
                                                max_tries = 100)
  
  nmds_trap_plots[[current_taxa]] <- nmds_plot(input_list = nmds_outputs[[current_taxa]],
                                          title_str = paste0(str_to_title(current_taxa), '-level NMDS'),
                                          viridis_option = 'B',
                                          plot_by = 'trap_type')
  
  ggsave(here('figures', 'nmds', paste0(current_taxa, '_trap_nmds.png')), nmds_trap_plots[[current_taxa]], height = 12, width = 10)
  
  nmds_habitat_plots[[current_taxa]] <- nmds_plot(input_list = nmds_outputs[[current_taxa]],
                                               title_str = paste0(str_to_title(current_taxa), '-level NMDS'),
                                               viridis_option = 'B',
                                               plot_by = 'habitat_type')+
    theme(text=element_text(size=10))
  
  ggsave(here('figures', 'nmds', paste0(current_taxa, '_habitat_nmds.png')), nmds_habitat_plots[[current_taxa]], height = 12, width = 10)
  
}



# Combine 4 levels of plots for one big plot ------------------------------

nmds_scores <- bind_rows(nmds_outputs[['order']]$scores, 
                         nmds_outputs[['family']]$scores,
                         nmds_outputs[['genus']]$scores,
                         nmds_outputs[['bin']]$scores
                         ) %>%
  # capitalise the taxonomic ranks for plotting
  mutate(taxa_grouping = str_to_title(taxa_grouping),
         # convert from 'Bin' to 'BIN'
         taxa_grouping = gsub('Bin', 'BIN', taxa_grouping),
         # turn it into a factor, so we can specify a non-alphabetical plotting
         # order
         taxa_grouping = as.factor(taxa_grouping),
         # specify the factor order
         taxa_grouping = fct_relevel(taxa_grouping, c('Order', 'Family', 
                                                      'Genus', 'BIN')))

nmds_centroids <- bind_rows(nmds_outputs[['order']]$trap_centroid, 
                            nmds_outputs[['family']]$trap_centroid,
                            nmds_outputs[['genus']]$trap_centroid,
                            nmds_outputs[['bin']]$trap_centroid) %>%
  # capitalise the taxonomic ranks for plotting
  mutate(taxa_grouping = str_to_title(taxa_grouping),
         # convert from 'Bin' to 'BIN'
         taxa_grouping = gsub('Bin', 'BIN', taxa_grouping),
         # turn it into a factor, so we can specify a non-alphabetical plotting
         # order
         taxa_grouping = as.factor(taxa_grouping),
         # specify the factor order
         taxa_grouping = fct_relevel(taxa_grouping, c('Order', 'Family', 
                                                      'Genus', 'BIN')))
  
# plot containing all but the centroids
big_nmds_plot <- ggplot(data=nmds_scores,
       aes(
         x=NMDS1,
         y=NMDS2,
         group=trap_type,
         shape=trap_type)) + 
  geom_point(data=nmds_centroids, size=5,# color="black",
             aes(colour=trap_type, fill = trap_type, shape=trap_type), show.legend=FALSE)+
  stat_ellipse(show.legend=FALSE) +
  geom_point(aes(colour=trap_type)) + # add the point markers
  scale_colour_viridis_d(option = 'D')+
  scale_fill_viridis_d(option = 'D')+
  facet_wrap(.~ taxa_grouping, scales = 'free')+
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  #coord_equal() +
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(size=10))+
  labs(colour = 'Trap type', shape = 'Trap type')+
  # increase point size in legend
  guides(colour = guide_legend(override.aes = list(size=10)))

ggsave(here('figures', 'nmds', 'fig_2_big_nmds_plot.pdf'), big_nmds_plot,
       dpi = 600)

# save the habitat NMDS plot as a basic gridextra format one
multipanel_nmds <- grid.arrange(nmds_habitat_plots$order, 
             nmds_habitat_plots$family,
             nmds_habitat_plots$genus,
             nmds_habitat_plots$bin,
             ncol = 2)
ggsave(multipanel_nmds, filename = here('figures', 'fig_si4_nmds.png'),
       width = 10, height = 7)
# Analyses ----------------------------------------------------------------



# Pat Schloss's tutorial at https://www.youtube.com/watch?v=oLf0EpMJ4yA is good


## order-level analyses
order_centroid <- nmds_outputs[['order']]$scores %>%
  group_by(trap_type) %>%
  summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))


tests_list <- list()
for(taxa in taxonomic_levels){
  tests_list[[taxa]] <- adonis2(nmds_outputs[[taxa]]$dist_mat~ nmds_outputs[[taxa]]$scores$trap_type + nmds_outputs[[taxa]]$scores$habitat_type, 
                                permutations = 1e3, by = 'terms')
  
  tests_list[[taxa]] %>%
    broom::tidy() %>%
    # save summary values used in-manuscript text
    write_csv(here('results', 'adonis', paste0(taxa, '_summary.csv')))
}

tests_list[['genus']]


# Malaise trap time analyses ----------------------------------------------

# family level

malaise_inputs <- list()
malaise_nmds_list <- list()
malaise_adonis_outputs <- list()
for(taxa in  taxonomic_levels){
  print(taxa)
  
  # filter just for the desired values
  malaise_inputs[[taxa]] <- nmds_inputs[[taxa]]
  
  malaise_inputs[[taxa]]$trap_types <- malaise_inputs[[taxa]]$trap_types %>% 
    filter(trap_type == 'Malaise') %>%
    # add the temporal metadata
    left_join(malaise_trap_metadata, by = c('sampling_event' = 'lot')) %>%
    select(sampling_event, trap_type, habitat_type, coarse_timing) %>%
    filter(!is.na(coarse_timing))
  
  # filter the matrix so that it only contains rows present in the trap_types
  # tibble
  malaise_inputs[[taxa]]$trap_matrix <- malaise_inputs[[taxa]]$trap_matrix[rownames(malaise_inputs[[taxa]]$trap_matrix) %in% malaise_inputs[[taxa]]$trap_types$sampling_event,]
  
  malaise_nmds_list[[taxa]] <- nmds_analysis(malaise_inputs[[taxa]], 
                                       min_tries = 20,
                                       max_tries = 100,
                                       malaise_analysis = T)
 
  
  # write the taxonomic ranking to the time_centroid, for use later
  malaise_nmds_list[[taxa]]$time_centroid$taxa_grouping <- taxa
  
  # run the analysis
  malaise_adonis_outputs[[taxa]] <- adonis2(malaise_nmds_list[[taxa]]$dist_mat~malaise_nmds_list[[taxa]]$scores$coarse_timing + malaise_nmds_list[[taxa]]$scores$habitat_type, 
                                            permutations = 1e3, by = 'terms')
   
}




# Big MALAISE plot --------------------------------------------------------


malaise_nmds_scores <- bind_rows(malaise_nmds_list[['order']]$scores, 
                         malaise_nmds_list[['family']]$scores,
                         malaise_nmds_list[['genus']]$scores,
                         malaise_nmds_list[['bin']]$scores
) %>%
  # capitalise the taxonomic ranks for plotting
  mutate(taxa_grouping = str_to_title(taxa_grouping),
         # convert from 'Bin' to 'BIN'
         taxa_grouping = gsub('Bin', 'BIN', taxa_grouping),
         # turn it into a factor, so we can specify a non-alphabetical plotting
         # order
         taxa_grouping = as.factor(taxa_grouping),
         # specify the factor order
         taxa_grouping = fct_relevel(taxa_grouping, c('Order', 'Family', 
                                                      'Genus', 'BIN')))


malaise_nmds_centroids <- bind_rows(malaise_nmds_list[['order']]$time_centroid, 
                            malaise_nmds_list[['family']]$time_centroid,
                            malaise_nmds_list[['genus']]$time_centroid,
                            malaise_nmds_list[['bin']]$time_centroid) %>%
  # capitalise the taxonomic ranks for plotting
  mutate(taxa_grouping = str_to_title(taxa_grouping),
         # convert from 'Bin' to 'BIN'
         taxa_grouping = gsub('Bin', 'BIN', taxa_grouping),
         # turn it into a factor, so we can specify a non-alphabetical plotting
         # order
         taxa_grouping = as.factor(taxa_grouping),
         # specify the factor order
         taxa_grouping = fct_relevel(taxa_grouping, c('Order', 'Family', 
                                                      'Genus', 'BIN')))

# plot containing all but the centroids
big_malaise_nmds_plot <- ggplot(data=malaise_nmds_scores,
                        aes(
                          x=NMDS1,
                          y=NMDS2,
                          group=coarse_timing,
                          shape=coarse_timing)) + 
  geom_point(data=malaise_nmds_centroids, size=5,# color="black",
             aes(colour=coarse_timing, fill = coarse_timing, shape=coarse_timing), show.legend=FALSE)+
  stat_ellipse(show.legend=FALSE) +
  geom_point(aes(colour=coarse_timing)) + # add the point markers
  # specify colours manually, as the viridis options default to a dark tone for
  # day, light for night
  scale_colour_manual(values = c('Day' ='#FDE725FF', 'Night' = '#440154FF'))+
  scale_fill_manual(values = c('Day' = '#FDE725FF', 'Night' = '#440154FF'))+
  facet_wrap(.~ taxa_grouping, scales = 'free')+
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(size=10))+
  labs(colour = 'Trap deployment time', shape = 'Trap deployment time')+
  # increase point size in legend
  guides(colour = guide_legend(override.aes = list(size=10)))

big_malaise_nmds_plot
ggsave(here('figures', 'nmds', 'si_fig_7_malaise_nmds_plot.png'), big_malaise_nmds_plot)




# Basic night-day taxonomic summaries -------------------------------------

# get the order and family pairs from our big input data
taxonomy_tib <- too_many_cols %>%
  select(order, family) %>%
  filter(!is.na(family)) %>%
  distinct()

# now summarise our datasets by day/night for order
order_temporal_counts <- malaise_inputs[['order']]$trap_matrix %>%
  as_tibble(rownames = 'sampling_event') %>%
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  left_join(malaise_inputs[['order']]$trap_types) %>%
  select(-c(habitat_type, trap_type, sampling_event)) %>%
  pivot_longer(where(is.numeric), names_to = 'taxa', values_to = 'count') %>%
  group_by(coarse_timing, taxa) %>%
  summarise(overall_abundance = sum(count)) %>%
  pivot_wider(names_from = 'coarse_timing', values_from = 'overall_abundance')

# now summarise our datasets by day/night for family
family_temporal_counts <- malaise_inputs[['family']]$trap_matrix %>%
  as_tibble(rownames = 'sampling_event') %>%
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  left_join(malaise_inputs[['family']]$trap_types) %>%
  select(-c(habitat_type, trap_type, sampling_event)) %>%
  pivot_longer(where(is.numeric), names_to = 'taxa', values_to = 'count') %>%
  group_by(coarse_timing, taxa) %>%
  summarise(overall_abundance = sum(count)) 


family_summary <- family_temporal_counts %>%
  pivot_wider(names_from = 'coarse_timing', values_from = 'overall_abundance')

# add the order-level information to the family

family_temporal_counts %>%
  pivot_wider(names_from = 'coarse_timing', values_from = 'overall_abundance') %>%
  left_join(taxonomy_tib, by = c('taxa' = 'family')) %>%
  arrange(order) %>%
  rename(Family = taxa, Order = order) %>%
  select(Order, Family, Day, Night) %>%
  write_csv(file = here('results', 'supplementary_table_6_diurnal_activity.csv'))

