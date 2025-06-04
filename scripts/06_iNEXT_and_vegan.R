# this script, for the first time in the workflow, incorporates data from 
# earthcape as well as from BOLD.

library(tidyverse)
library(iNEXT)
library(here)
#library(vegan)

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



# Condensed iNEXT plots ---------------------------------------------------


for(o in desired_orders){
  print(o)
  inext_objs[[o]] <- list()
  for_inext <- list()
  for(t in traptypes){
    inext_vec <- tib_for_inext %>%
      # select only the rows that we need for this order and trap type
      filter(order == o, type == t) %>%
      # calculate the abundance of each BIN
      group_by(bin) %>%
      summarise(total_nsamples = sum(nsamples)) %>%
      # arrange the values from biggest to smallest
      arrange(desc(total_nsamples)) %>%
      # pull just a vector of total abundances
      pull(total_nsamples)
    if(length(inext_vec) >= 5){
      for_inext[[t]] <- inext_vec
    }
  }
  inext_objs[[o]] <- iNEXT(for_inext, datatype = 'abundance')
}


# type1 plot
type1_df <- map(inext_objs, fortify, type = 1) %>%
  bind_rows(.id = 'taxa')


type1_df.point <- type1_df[which(type1_df$Method=="Observed"),]
type1_df.line <- type1_df[which(type1_df$Method!="Observed"),]


type1_plot <- ggplot(type1_df, aes(x=x, y=y, colour=Assemblage)) + 
  geom_point(size=2, data=type1_df.point) +
  #geom_point(aes(shape=Assemblage), size=5, data=type1_df.point) +
  geom_line(aes(linetype=Method), lwd=1.5, data=type1_df.line) +
  facet_wrap(.~ taxa, scales = 'free') +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=Assemblage, colour=NULL), alpha=0.2) +
  labs(x="Number of individual insects from order and trap type sequenced", 
       y="Number of BINs") +
  theme_bw()+
  scale_colour_viridis_d()+
  theme(legend.position = "bottom", 
        legend.title=element_blank(),
        text=element_text(size=18))
type1_plot

ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.png'),
       type1_plot,
       dpi = 600)

# type2 plot
type2_df <- map(inext_objs, fortify, type = 2) %>%
  bind_rows(.id = 'taxa')


type2_df.point <- type2_df[which(type2_df$Method=="Observed"),]
type2_df.line <- type2_df[which(type2_df$Method!="Observed"),]


type2_plot <- ggplot(type2_df, aes(x=x, y=y, colour=Assemblage)) + 
  geom_point(size=2, data=type2_df.point) +
  geom_line(aes(linetype=Method), lwd=1.5, data=type2_df.line) +
  facet_wrap(.~ taxa, scales = 'free') +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=Assemblage, colour=NULL), alpha=0.2) +
  labs(x="Number of individual insects from order and trap type sequenced", 
       y="Sample coverage") +
  theme_bw()+
  scale_colour_viridis_d()+
  theme(legend.position = "bottom", 
        legend.title=element_blank(),
        text=element_text(size=18))
type2_plot

ggsave(filename = here('figures', 'inext_plots', 'type2_inext_plot.png'),
       type2_plot,
       dpi = 600)

# Old code, probably to be deleted ----------------------------------------


for(trap_type in traptypes){
  for_inext_list[[trap_type]] <- list()
  # n_events <- tib_for_inext %>%
  #   filter(type == trap_type) %>%
  #   pull(sampling_event) %>%
  #   unique() %>%
  #   length()
  for(o in desired_orders){
    cat('trap type is ', trap_type, '\n')
    
    #cat('order is ', o, 'trap type is ', trap_type, ' number of sampling events was', n_events, '\n')
    incidence_freq <- tib_for_inext %>%
      filter(type == trap_type) %>%
      filter(order == o) %>%
      group_by(bin) %>%
      summarise(freq = n())%>%
      pull(freq) %>%
      sort(decreasing = T)
    
    n_sequenced <- sum(incidence_freq)
    
    # if there are fewer than nbin_threshold unique BINs in this
    # trap type , discard the order, otherwise save it for analysis
    
    if(length(incidence_freq) >= nbin_threshold){
      for_inext_list[[trap_type]][[o]] <- c(n_sequenced, incidence_freq)
    }
    
  }
  inext_objs[[trap_type]] <- iNEXT(for_inext_list[[trap_type]], 
                                  #q = c(0, 1, 2),
                                  q = 0, # get 'species' richness 
                                  # this is incidence_freq: we're analysing a dataset 
                                  # of how often a given BIN is detected at least once in 
                                  # a sample, not the number of occurrences of that BIN
                      datatype = 'incidence_freq',
                      size = round(seq(1,8000, by = 100)),
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
   inext_plots[['extrapolation']][[trap_type]] <- ggiNEXT(inext_objs[[trap_type]], type=3, 
                                                          color.var="Assemblage",
                                                         se = F) +
     theme_bw(base_size = 18) +
     theme(legend.position="bottom",
           legend.box = "vertical") + 
     ylab("BIN diversity")+ 
     ggtitle(trap_type) 
   ggsave(here('figures', 'inext_plots', 
               paste0('extrapolation_', trap_type, '.pdf')),
          inext_plots[['extrapolation']][[trap_type]],
          width = 8)
}



# make a plot of ALL taxa -------------------------------------------------


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
options(scipen = 999)
alltaxa_gginext <- ggiNEXT(z, type=1, color.var="Assemblage")+ theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(size=20))+
  ylab('BIN richness')+
  xlab('Number of individuals sequenced')+
  # make ggplot use a thousands separator on the axis labels
  scale_x_continuous(labels=function(x) format(x, big.mark = ",",
                                               scientific = FALSE))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",",
                                             scientific = FALSE))


alltaxa_gginext
ggsave(alltaxa_gginext, file = here('figures', 'inext_plots', 'alltaxa_plot.png'),
       height = 8, width = 15, dpi = 600)

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
    labs(x = 'Number of individual insects from order sequenced', y = yaxis_text)
  
  
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

type2_inext_plot
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



