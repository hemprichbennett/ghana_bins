# this script, for the first time in the workflow, incorporates data from 
# earthcape as well as from BOLD.

library(tidyverse)
library(iNEXT)
library(here)
library(vegan)

source(here('parameters.R'))


too_many_cols <- read_csv(, 
          file = here('data', 'processed_data', 
                      'bold_and_earthcape_combined.csv'))


# Plotting ----------------------------------------------------------------



# make a very basic summary plot
too_many_cols %>%
  filter(!is.na(order) & !is.na(type)) %>%
  group_by(order, sampling_event, type) %>%
  summarise(nsamples = n()) %>%
  ggplot(., aes(x = order, y = nsamples)) +
  geom_boxplot() +
  facet_wrap(.~ type, scales = 'free')

bin_presence_summary <- too_many_cols %>%
  mutate(has_bin = !is.na(bin)) %>%
  group_by(type, has_bin) %>%
  summarise(nsamples = n())

tib_for_inext <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  select(bin, order, sampling_event, type) %>%
  group_by_all() %>%
  summarise(nsamples = n())

# only work on orders with at least the below number of bins (nbin_threshold
# is set in the external, 'parameters' script, for easy incorporation with
# manuscript)

desired_orders <- tib_for_inext %>%
  filter(!is.na(order)) %>%
  group_by(order) %>%
  summarise(bin_richness = length(unique(bin))) %>%
  filter(bin_richness >= nbin_threshold) %>%
  pull(order)


traptypes <- tib_for_inext %>%
  filter(!is.na(type)) %>%
  filter(type != 'Cdc') %>%
  pull(type) %>%
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
    filter(type == trap_type) %>%
    pull(sampling_event) %>%
    unique() %>%
    length()
  for(o in desired_orders){
    cat('trap type is ', trap_type, '\n')
    
    cat('order is ', o, 'trap type is ', trap_type, ' number of sampling events was', n_events, '\n')
    incidence_freq <- tib_for_inext %>%
      filter(type == trap_type) %>%
      filter(order == o) %>%
      group_by(bin) %>%
      summarise(freq = n())%>%
      pull(freq) %>%
      sort(decreasing = T)
    
    
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
  # inext_plots[['completeness']][[trap_type]] <- ggiNEXT(inext_objs[[trap_type]], type=2, 
  #                                     color.var="Assemblage",
  #                                     se = F) +
  #   theme_bw(base_size = 18) +
  #   theme(legend.position="bottom",
  #         legend.box = "vertical") + 
  #   ggtitle(trap_type) 
  # ggsave(here('figures', 'inext_plots', 
  #             paste0('completeness_', trap_type, '.pdf')),
  #        inext_plots[['completeness']][[trap_type]],
  #        width = 8)
  
  
  # plot interpolation/extrapolation
  # inext_plots[['extrapolation']][[trap_type]] <- ggiNEXT(inext_objs[[trap_type]], type=3, 
  #                                                        color.var="Assemblage",
  #                                                       se = F) +
  #   theme_bw(base_size = 18) +
  #   theme(legend.position="bottom",
  #         legend.box = "vertical") + 
  #   ylab("BIN diversity")+ 
  #   ggtitle(trap_type) 
  # ggsave(here('figures', 'inext_plots', 
  #             paste0('extrapolation_', trap_type, '.pdf')),
  #        inext_plots[['extrapolation']][[trap_type]],
  #        width = 8)
}


# make a plot of ALL taxa

alltaxa_trap_abundances <- too_many_cols %>%
  filter(!is.na(type)) %>%
  select(bin, type) %>%
  group_by_all() %>%
  summarise(nsamples = n())

alltaxa_trap_for_inext <- list()
for(trap in unique(alltaxa_trap_abundances$type)){
  # if(trap == 'Cdc'){
  #   next()
  # }
  print(trap)
  n_individuals <- alltaxa_trap_abundances %>%
    filter(type == trap) %>%
    pull(nsamples) %>%
    sum()
  bin_abundances <- alltaxa_trap_abundances %>%
    filter(type == trap) %>%
    pull(nsamples) %>%
    sort(decreasing = T)
  alltaxa_trap_for_inext[[trap]] <- c(n_individuals, bin_abundances)
}
Sys.time()
# this should run an iNEXT where the samples are the number of samples sequenced,
# NOT the trapping effort
z <- iNEXT(alltaxa_trap_for_inext, q=0, datatype="incidence_freq")
Sys.time()
rds_path <- here('data', 'processed_data', 'big_inext_object.RDS')
saveRDS(object = z, 
        file = rds_path)
z <- readRDS(file = rds_path)
alltaxa_gginext <- ggiNEXT(z, type=1, color.var="Assemblage")+ theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(size=20))+
  ylab('BIN richness')#+
  #xlab('Number of individuals sequenced')


alltaxa_gginext
ggsave(alltaxa_gginext, file = here('figures', 'inext_plots', 'alltaxa_plot.png'))

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
    scale_linetype('Data type')+
    geom_point(data = filter(inext_tib, Method == 'Observed'),
               mapping = aes(x = x, y = y, fill = Method))+
    #scale_fill_discrete(guide = guide_legend(title = NULL))+
    facet_grid(Assemblage ~trap_type, 
               scales = 'free')+
    theme_bw()+
    guides(linetype = guide_legend(order=1),
           fill = guide_legend(order=2, title = NULL))+
    theme(legend.position = 'bottom')+
    labs(x = 'Number of traps', y = yaxis_text)
  
  
  return(inext_plot)
  
}

type1_inext_plot <- big_inext_plotting(input_list = inext_objs,
                                       inext_type = 1)
type1_inext_plot
ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.pdf'),
       type1_inext_plot,
       height = 15,
       dpi = 600)

ggsave(filename = here('figures', 'fig_2_type1_inext_plot.png'),
       type1_inext_plot,
       height = 15,
       dpi = 600)

ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.png'),
       type1_inext_plot,
       height = 15,
       dpi = 600)

type2_inext_plot <- big_inext_plotting(input_list = inext_objs,
                                       inext_type = 2)
ggsave(filename = here('figures', 'inext_plots', 'type2_inext_plot.png'),
       type2_inext_plot,
       height = 15,
       dpi = 600)

ggsave(filename = here('figures', 'fig_3_type2_inext_plot.png'),
       type2_inext_plot,
       height = 15,
       dpi = 600)





# Further stuff -----------------------------------------------------------



# grouping by the date-time start. Is this correct?
visit_inext_tib <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  #rename(date =date_time_start) %>%
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
ggsave(here('figures', 'inext_plots', 'overall_visits.pdf'), visit_inext_plot)
# estimate of 'species' richness
overall_chaorichness <- ChaoRichness(visit_inext_list, 
                                     datatype = 'incidence_freq') %>%
  mutate(percent_completeness = Observed / Estimator * 100) %>%
  rownames_to_column('taxa')

write_csv(overall_chaorichness,
          file = here('results', 'overall_chaorichness.csv'))

# the number of samples classed to BIN/ not classed to BIN

samples_with_bins <- too_many_cols %>% 
  mutate(has_bin = !is.na(bin)) %>%
  group_by(order, has_bin) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = has_bin, values_from = n, values_fill = 0)

write_csv(samples_with_bins, 
          here('results', 'samples_with_bins.csv'))


# remove CDC traps before NMDS etc
too_many_cols <- too_many_cols %>%
  filter(type != 'Cdc')

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



# Malaise trap analyses ---------------------------------------------------

# samples mostly have the date and time of deployment, apart from some samples
# that only contain date. Exclude the samples that only have date data

too_many_cols %>%
  filter(type == 'Malaise') %>%
  pull(date)


# we have three types of analyses we can do: 
# 1) how do the communities of individual traps change over their 24 hour deployment
# 2) how do the traps accumulate BINs over their 5ish deployment intervals 
#   (many iNEXT) analyses
# 3) what overall temporal patterns are there in our species captures. E.g. Are there
#     taxa only caught at night?