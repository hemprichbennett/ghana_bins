
library(tidyverse)
library(iNEXT)
library(here)

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
        text=element_text(size=10))
type1_plot

ggsave(filename = here('figures', 'inext_plots', 'type1_inext_plot.pdf'),
       type1_plot,
       dpi = 600)

ggsave(filename = here('figures', 'fig_1_type1_inext_plot.pdf'),
       type1_plot,
       dpi = 600,
       width = 11.5)

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
        text=element_text(size=10))
type2_plot

ggsave(filename = here('figures', 'inext_plots', 'type2_inext_plot.png'),
       type2_plot,
       dpi = 600)

ggsave(filename = here('figures', 'supplementary_figure_type2_inext_plot.png'),
       type2_plot,
       dpi = 600, 
       width = 11.5)

# make a plot of ALL taxa -------------------------------------------------


alltaxa_trap_abundances <- too_many_cols %>%
  filter(!is.na(type)) %>%
  select(bin, type) %>%
  group_by_all() %>%
  summarise(nsamples = n())

alltaxa_trap_for_inext <- list()
for(trap in unique(alltaxa_trap_abundances$type)){
  
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


# Calculate the OVERALL estimated sampling completeness, -------------------
# independent of trap type or taxonomy

overall_n_individuals <- nrow(too_many_cols)

overall_bin_abundances <- too_many_cols %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  summarise(nsamples = n()) %>%
  pull(nsamples) %>%
  sort(decreasing = T)



overall_inext <- iNEXT(c(overall_n_individuals, overall_bin_abundances), q=0, datatype="incidence_freq")
overall_rds_path <- here('data', 'processed_data', 'big_overall_inext_object.RDS')
# save the object, so it can be reloaded again without needing several hours
# to regenerate
saveRDS(object = overall_inext, 
        file = overall_rds_path)
overall_inext <- readRDS(file = overall_rds_path)

overall_inext

# calculate percentage completeness
overall_inext$AsyEst %>% 
  as_tibble(rownames = 'Index') %>%
  filter(Index == 'Species Richness') %>% 
  mutate(percent_completeness = Observed / Estimator * 100) %>%
  pull(percent_completeness)



# Calculate overall percent completeness for each order -------------------

results_list <- list()
for(taxa in desired_orders){
  
  taxa_n_individuals <- too_many_cols %>%
    filter(order == taxa) %>%
    nrow(.)
  
  taxa_bin_abundances <- too_many_cols %>%
    filter(order == taxa) %>%
    group_by(bin) %>%
    summarise(nsamples = n()) %>%
    pull(nsamples) %>%
    sort(decreasing = T)
  
  
  
  taxa_inext <- iNEXT(c(taxa_n_individuals, taxa_bin_abundances), q=0, datatype="incidence_freq")
  completeness_est <- taxa_inext$AsyEst %>% 
    as_tibble(rownames = 'Index') %>%
    filter(Index == 'Species Richness') %>% 
    mutate(percent_completeness = Observed / Estimator * 100) %>%
    pull(percent_completeness)
  results_list[[taxa]] <- tibble(taxa = taxa, 
                                 estimated_completeness_percentage = completeness_est)
}

taxa_overall_completeness <- bind_rows(results_list)

write_csv(taxa_overall_completeness, file = here('results', 'taxa_completeness.csv'))

taxa_overall_completeness %>% summarise(m = mean(estimated_completeness_percentage))

taxa_overall_completeness %>% arrange(estimated_completeness_percentage)
