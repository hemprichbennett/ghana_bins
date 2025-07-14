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


# NMDS analyses ---------------------------------------------------------------

for(current_taxa in c('order', 'family', 'genus', 'bin')){
  print(current_taxa)
}

## Family-level

family_nmds_input <- nmds_input_generator('family', 
                                          min_taxa_threshold = nmds_inclusion_threshold,
                                          min_trap_threshold = nmds_inclusion_threshold)

family_nmds <- nmds_analysis(family_nmds_input, 
                    min_tries = 20,
                    max_tries = 100)

### trapwise plots

family_trap_plot <- nmds_plot(input_list = family_nmds,
          title_str = 'Family-level NMDS',
          viridis_option = 'B',
          plot_by = 'trap_type')


family_trap_plot

ggsave(here('figures', 'nmds', 'family_trap_nmds.png'), family_trap_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_3_family_trap_nmds.png'), family_trap_plot, height = 12, width = 12)


### habitatwise plots

family_habitat_plot <- nmds_plot(input_list = family_nmds,
                              title_str = 'Family-level NMDS',
                              viridis_option = 'D',
                              plot_by = 'habitat_type')


family_habitat_plot

ggsave(here('figures', 'nmds', 'family_habitat_nmds.png'), family_habitat_plot, height = 12, width = 10)
#ggsave(here('figures', 'fig_7_family_habitat_nmds.png'), family_habitat_plot, height = 12, width = 10)


## Order-level

order_nmds_input <- nmds_input_generator('order', min_taxa_threshold = nmds_inclusion_threshold,
                                         min_trap_threshold = nmds_inclusion_threshold)

order_nmds <- nmds_analysis(order_nmds_input,
                   min_tries = 20,
                   max_tries = 100)

## trapwise plots

order_trap_plot <- nmds_plot(input_list = order_nmds,
                         title_str = 'Order-level NMDS',
                        viridis_option = 'B',
                        plot_by = 'trap_type')

order_trap_plot

ggsave(here('figures', 'nmds', 'order_trap_nmds.png'),order_trap_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_2_order_trap_nmds.png'),order_trap_plot, height = 12, width = 12)

## habitatwise plots

order_habitat_plot <- nmds_plot(input_list = order_nmds,
                             title_str = 'Order-level NMDS',
                             viridis_option = 'D',
                             plot_by = 'habitat_type')

order_habitat_plot

ggsave(here('figures', 'nmds', 'order_habitat_nmds.png'),order_habitat_plot, height = 12, width = 10)
#ggsave(here('figures', 'fig_8_order_habitat_nmds.png'),order_habitat_plot, height = 12, width = 10)

## BIN-level

bin_nmds_input <- nmds_input_generator('bin', min_taxa_threshold = nmds_inclusion_threshold,
                                         min_trap_threshold = nmds_inclusion_threshold)

bin_nmds <- nmds_analysis(bin_nmds_input,
                            min_tries = 20,
                            max_tries = 100)

## trapwise plots

bin_trap_plot <- nmds_plot(input_list = bin_nmds,
                             title_str = 'BIN-level NMDS',
                             viridis_option = 'B',
                             plot_by = 'trap_type')

bin_trap_plot

ggsave(here('figures', 'nmds', 'bin_trap_nmds.png'),bin_trap_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_2_bin_trap_nmds.png'),bin_trap_plot, height = 12, width = 12)

## habitatwise plots

bin_habitat_plot <- nmds_plot(input_list = bin_nmds,
                                title_str = 'BIN-level NMDS',
                                viridis_option = 'D',
                                plot_by = 'habitat_type')

bin_habitat_plot

ggsave(here('figures', 'nmds', 'bin_habitat_nmds.png'),bin_habitat_plot, height = 12, width = 10)


# Combine 3 levels of plots for one big plot ------------------------------


nmds_scores <- bind_rows(order_nmds$scores, 
                         family_nmds$scores, 
                         bin_nmds$scores) %>%
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

nmds_centroids <- bind_rows(order_nmds$trap_centroid, 
                            family_nmds$trap_centroid,
                            bin_nmds$trap_centroid) %>%
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
ggplot(data=nmds_scores,
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

# Analyses ----------------------------------------------------------------



# Pat Schloss's tutorial at https://www.youtube.com/watch?v=oLf0EpMJ4yA is good


## order-level analyses
order_centroid <- order_nmds$scores %>%
  group_by(trap_type) %>%
  summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))



# z <- order_nmds$nmds_analysis$dist
# temp_dist <- vegdist(order_nmds_input$trap_matrix)
# 
# metaMDS(temp_dist)

order_test <- adonis2(order_nmds$dist_mat~ order_nmds$scores$trap_type + order_nmds$scores$habitat_type, 
                      permutations = 1e3, by = 'terms')

order_test

order_test %>%
  broom::tidy()

order_test %>%
  broom::tidy() %>%
  write_csv(here('results', 'adonis', 'order_summary.csv'))


## family-level analyses

family_test <- adonis2(family_nmds$dist_mat~family_nmds$scores$trap_type + family_nmds$scores$habitat_type, 
                       permutations = 1e3, by = 'terms')

family_test

family_test %>%
  broom::tidy()

family_test %>%
  broom::tidy() %>%
  write_csv(here('results', 'adonis', 'family_summary.csv'))

# the strata option in adonis could be used to try and control for trap location
p_value <- order_test$aov.tab$`Pr(>F)`[1]
order_test$aov.tab

# test for betadispersion
library(broom)
order_bd <- betadisper(dist(order_nmds_input$trap_matrix), order_nmds$scores$trap_type)
# is the data betadispersed? ("Is there difference in within-group variation 
# between groups." I think.)
anova(order_bd) %>%
  tidy() %>%
  write_csv(here('results', 'adonis', 'order_betadispersion.csv'))


family_bd <- betadisper(dist(family_nmds_input$trap_matrix), family_nmds$scores$trap_type)
# is the data betadispersed? ("Is there difference in within-group variation 
# between groups." I think.)
anova(family_bd) %>%
  tidy() %>%
  write_csv(here('results', 'adonis', 'family_betadispersion.csv'))





# Malaise trap time analyses ----------------------------------------------

# family level

# filter just for the desired values
family_malaise_input <- family_nmds_input

family_malaise_input$trap_types <- family_malaise_input$trap_types %>% 
  filter(trap_type == 'Malaise') %>%
  # add the temporal metadata
  left_join(malaise_trap_metadata, by = c('sampling_event' = 'lot')) %>%
  select(sampling_event, trap_type, habitat_type, coarse_timing) %>%
  filter(!is.na(coarse_timing))

# filter the matrix so that it only contains rows present in the trap_types
# tibble
family_malaise_input$trap_matrix <- family_malaise_input$trap_matrix[rownames(family_malaise_input$trap_matrix) %in% family_malaise_input$trap_types$sampling_event,]
  
family_malaise_nmds <- nmds_analysis(family_malaise_input, 
                             min_tries = 20,
                             max_tries = 100,
                             malaise_analysis = T)

family_malaise_plot <- nmds_plot(input_list = family_malaise_nmds,
          title_str = 'Family-level NMDS',
          viridis_option = 'D',
          plot_by = 'coarse_timing',
          all_text_size = 10)+
  labs(tag = 'B')


# test significance of timing and habitat type on the families detected
adonis2(family_malaise_nmds$dist_mat~family_malaise_nmds$scores$coarse_timing + family_malaise_nmds$scores$habitat_type, 
        permutations = 1e3, by = 'terms') %>%
  broom::tidy()



# order level

# filter just for the desired values
order_malaise_input <- order_nmds_input

order_malaise_input$trap_types <- order_malaise_input$trap_types %>% 
  filter(trap_type == 'Malaise') %>%
  # add the temporal metadata
  left_join(malaise_trap_metadata, by = c('sampling_event' = 'lot')) %>%
  select(sampling_event, trap_type, habitat_type, coarse_timing) %>%
  filter(!is.na(coarse_timing))

# filter the matrix so that it only contains rows present in the trap_types
# tibble
order_malaise_input$trap_matrix <- order_malaise_input$trap_matrix[rownames(order_malaise_input$trap_matrix) %in% order_malaise_input$trap_types$sampling_event,]

order_malaise_nmds <- nmds_analysis(order_malaise_input, 
                                     min_tries = 20,
                                     max_tries = 100,
                                     malaise_analysis = T)

order_malaise_plot <- nmds_plot(input_list = order_malaise_nmds,
          title_str = 'Order-level NMDS',
          viridis_option = 'D',
          plot_by = 'coarse_timing',
          all_text_size = 10)+
  labs(tag = 'A')

# test significance of timing and habitat type on the orders detected
adonis2(order_malaise_nmds$dist_mat~order_malaise_nmds$scores$coarse_timing + order_malaise_nmds$scores$habitat_type, 
        permutations = 1e3, by = 'terms') %>%
  broom::tidy()



# Save malaise plots ------------------------------------------------------



multipanel_nmds <- grid.arrange(order_malaise_plot, family_malaise_plot, ncol = 2)
multipanel_nmds
ggsave(multipanel_nmds, filename = here('figures', 'si_x_mutlipanel_nmds.png'),
       width = 7)

# Basic night-day taxonomic summaries -------------------------------------

# get the order and family pairs from our big input data
taxonomy_tib <- too_many_cols %>%
  select(order, family) %>%
  filter(!is.na(family)) %>%
  distinct()

# now summarise our datasets by day/night for order
order_temporal_counts <- order_malaise_input$trap_matrix %>%
  as_tibble(rownames = 'sampling_event') %>%
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  left_join(order_malaise_input$trap_types) %>%
  select(-c(habitat_type, trap_type, sampling_event)) %>%
  pivot_longer(where(is.numeric), names_to = 'taxa', values_to = 'count') %>%
  group_by(coarse_timing, taxa) %>%
  summarise(overall_abundance = sum(count)) %>%
  pivot_wider(names_from = 'coarse_timing', values_from = 'overall_abundance')

# now summarise our datasets by day/night for family
family_temporal_counts <- family_malaise_input$trap_matrix %>%
  as_tibble(rownames = 'sampling_event') %>%
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  left_join(order_malaise_input$trap_types) %>%
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
  write_csv(file = here('results', 'supplementary_table_diurnal_activity.csv'))

