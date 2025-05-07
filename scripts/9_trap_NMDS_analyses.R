# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(vegan)

habitat_data <- read_csv('data/raw_data/lot_habitat_classifications.csv')

source(here('parameters.R'))


too_many_cols <- read_csv(, 
                          file = here('data', 'processed_data', 
                                      'bold_and_earthcape_combined.csv'))


# NMDS time ---------------------------------------------------------------

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
    select(sampling_event, type) %>%
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
  
  return(list(trap_matrix = out_mat, 
              trap_types = trap_types))
}





# big_nmds <- metaMDS(family_nmds_dataset$trap_matrix, # Our community-by-species matrix
#                                  k=2) # The number of reduced dimensions. Increase if high stress is problem. 
# ### Make a better ordination, using tutorial from https://chrischizinski.github.io/rstats/vegan-ggplot2/
# # base R plot, as a starting point
# plot(big_nmds, type = "t")

# function to do an NMDS and ggplot on a dataset made with the above function
nmds <- function(input_list, k = 2, title_str = NA, min_tries = 20, max_tries = 20){
  big_nmds <- metaMDS(input_list$trap_matrix, # Our community-by-species matrix
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
    group_by(type) %>%
    summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))
  
  
  print(trap_centroid)
  out_plot <- ggplot(data=site.scores,aes(x=NMDS1,y=NMDS2,group=type)) + 
    stat_ellipse(show.legend=FALSE) +
    geom_point(aes(colour=type)) + # add the point markers
    geom_point(data=trap_centroid, size=5, shape=21, color="black",
               aes(fill=type), show.legend=FALSE)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    #geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=sampling_event),size=6,vjust=0) +  # add the site labels
    #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
    #coord_equal() +
    theme_bw()+
    theme(legend.position = 'bottom',
          text=element_text(size=30))+
    labs(colour = 'Trap type')
  
  if(!is.na(title_str)){
    out_plot <- out_plot + 
      ggtitle(title_str)
  }
  
  return(list(nmds_plot = out_plot, scores = site.scores, nmds_analysis = big_nmds))
}

family_nmds_input <- nmds_input_generator('family', 
                                          min_taxa_threshold = nmds_inclusion_threshold,
                                          min_trap_threshold = nmds_inclusion_threshold)

family_nmds <- nmds(family_nmds_input, title_str = 'Family-level NMDS',
                    min_tries = 20,
                    max_tries = 100)

family_nmds$nmds_plot
ggsave(here('figures', 'nmds', 'family_nmds.png'), family_nmds$nmds_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_5_family_nmds.png'), family_nmds$nmds_plot, height = 12, width = 10)

order_nmds_input <- nmds_input_generator('order', min_taxa_threshold = nmds_inclusion_threshold,
                                         min_trap_threshold = nmds_inclusion_threshold)

order_nmds <- nmds(order_nmds_input,title_str = 'Order-level NMDS',
                   min_tries = 20,
                   max_tries = 100)
order_nmds$nmds_plot
ggsave(here('figures', 'nmds', 'order_nmds.png'),order_nmds$nmds_plot, height = 12, width = 10)
ggsave(here('figures', 'fig_4_order_nmds.png'),order_nmds$nmds_plot, height = 12, width = 10)




# Pat Schloss's tutorial at https://www.youtube.com/watch?v=oLf0EpMJ4yA is good

order_centroid <- order_nmds$scores %>%
  group_by(type) %>%
  summarize(NMDS1=mean(NMDS1), NMDS2=mean(NMDS2))


order_nmds$nmds_plot +
  geom_point(data=order_centroid, size=5, shape=21, color="black",
             aes(fill=type), show.legend=FALSE)


order_test <- adonis2(dist(order_nmds_input$trap_matrix)~order_nmds$scores$type, 
                      permutations = 1e4)

order_test %>%
  broom::tidy() %>%
  write_csv(here('results', 'adonis', 'order_summary.csv'))

family_test <- adonis2(dist(family_nmds_input$trap_matrix)~family_nmds$scores$type, 
                       permutations = 1e4)

family_test %>%
  broom::tidy() %>%
  write_csv(here('results', 'adonis', 'family_summary.csv'))

# the strata option in adonis could be used to try and control for trap location
p_value <- order_test$aov.tab$`Pr(>F)`[1]
order_test$aov.tab

# test for betadispersion
library(broom)
order_bd <- betadisper(dist(order_nmds_input$trap_matrix), order_nmds$scores$type)
# is the data betadispersed? ("Is there difference in within-group variation 
# between groups." I think.)
anova(order_bd) %>%
  tidy() %>%
  write_csv(here('results', 'adonis', 'order_betadispersion.csv'))


family_bd <- betadisper(dist(family_nmds_input$trap_matrix), family_nmds$scores$type)
# is the data betadispersed? ("Is there difference in within-group variation 
# between groups." I think.)
anova(family_bd) %>%
  tidy() %>%
  write_csv(here('results', 'adonis', 'family_betadispersion.csv'))

# There is, yes

# 
# order_dist_mat <- vegdist(order_nmds_input$trap_matrix) %>%
#   as.matrix()
# 
# order_traptypes <- order_nmds_input$trap_types %>%
#   filter(sampling_event %in% colnames(order_dist_mat))
# 
# adonis_output <- adonis2(order_dist_mat ~ order_traptypes$type)
# summary(adonis_output)
#   
