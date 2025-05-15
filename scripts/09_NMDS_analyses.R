# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(vegan)

habitat_data <- read_csv('data/raw_data/lot_habitat_classifications.csv') %>%
  janitor::clean_names() %>%
  rename(lot = name, habitat_type = name_2) %>%
  select(lot, habitat_type) %>%
  mutate(habitat_type = gsub('^Natural$', 'Semi-natural', habitat_type))

source(here('parameters.R'))


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
              trap_types = trap_types))
}



# function to do an NMDS and ggplot on a dataset made with the above function
nmds_analysis <- function(input_list, k = 2, min_tries = 20, max_tries = 20){
  
  dist_mat <- vegdist(input_list$trap_matrix)
  
  big_nmds <- metaMDS(dist_mat, # Our community-by-species matrix
                      k=k,# The number of reduced dimensions. Increase if high stress is problem. 
                      try = min_tries, 
                      trymax = max_tries) 
  
  
  # make dataframes for plotting
  site.scores <- as.data.frame(scores(big_nmds, "site"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  site.scores$sampling_event <- as.numeric(rownames(site.scores))  # create a column of site names, from the rownames of data.scores
  site.scores <- site.scores %>%
    left_join(input_list$trap_types)
  
  
  species.scores <- as.data.frame(scores(big_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
  
  trap_centroid <- site.scores %>%
    group_by(trap_type) %>%
    summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))
  
  habitat_centroid <- site.scores %>%
    group_by(habitat_type) %>%
    summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))
  

  return(list(scores = site.scores, 
              nmds_analysis = big_nmds,
              trap_centroid = trap_centroid,
              habitat_centroid = habitat_centroid,
              dist_mat = dist_mat))
}

nmds_plot <- function(input_list, title_str = NA, viridis_option = "D",
                      plot_by){
  if(!plot_by %in% c('trap_type', 'habitat_type')){
    stop('Acceptable plot_by variables are trap and habitat')
  }
  
  plot_by <- sym(plot_by)
  
  if(plot_by == 'trap_type'){
    centroids_to_use <- input_list$trap_centroid
    legend_str = "Trap type"
  }else if(plot_by == 'habitat_type'){
    centroids_to_use <- input_list$habitat_centroid
    legend_str = "Habitat type"
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
          text=element_text(size=30))+
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
ggsave(here('figures', 'fig_5_family_trap_nmds.png'), family_trap_plot, height = 12, width = 10)


### habitatwise plots

family_habitat_plot <- nmds_plot(input_list = family_nmds,
                              title_str = 'Family-level NMDS',
                              viridis_option = 'D',
                              plot_by = 'habitat_type')


family_habitat_plot

ggsave(here('figures', 'nmds', 'family_habitat_nmds.png'), family_habitat_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_7_family_habitat_nmds.png'), family_habitat_plot, height = 12, width = 10)


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
ggsave(here('figures', 'fig_4_order_trap_nmds.png'),order_trap_plot, height = 12, width = 10)

## habitatwise plots

order_habitat_plot <- nmds_plot(input_list = order_nmds,
                             title_str = 'Order-level NMDS',
                             viridis_option = 'D',
                             plot_by = 'habitat_type')

order_habitat_plot

ggsave(here('figures', 'nmds', 'order_habitat_nmds.png'),order_habitat_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_8_order_habitat_nmds.png'),order_habitat_plot, height = 12, width = 10)



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



