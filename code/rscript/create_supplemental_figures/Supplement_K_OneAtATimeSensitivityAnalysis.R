library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)

# read csv file with aggregated data

if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  out.path <- here("simulations", sim.date,"Supplement_SensitivityAnalysis",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}

# read simulation data
stacked<- read_csv(out.path)

#identify which parameters have been varied in the one-at-a-time approach
stacked$var <- NA

the_cols <- c(1, 30:32, 35:40)

names(stacked)[the_cols] <- stringi::stri_replace_all_regex(names(stacked[,the_cols]), "\\.", "-")
# iterate over columns that contain parameters which may have been changed
for(cols in the_cols){
  # identify columns at which the parameter has been varied
  different_par_row <- which(as.character(as.vector(as.matrix(stacked[,cols]))) != names(sort(table(stacked[,cols]), decreasing = T))[1])
  par_name <- names(stacked)[cols]
  # create the label for the type of variation as ("parameter: value")
  stacked$var[different_par_row] <- apply(cbind(rep(par_name, length(different_par_row)), stacked[different_par_row, par_name]), 1, paste, collapse = ": ")
}

# set parameter sets with no varied parameter to default
stacked$var[(is.na(stacked$var))] <- "default"

#label simulations that stopped prematurely

# # create a subset where fast and slow end of each population is retained
stacked_sub <- stacked %>% 
  group_by(tot_coefvar) %>%
  mutate(pop_group = order(pop_dens)) %>%
  group_by(sim.id) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end"))

stacked_sub$pop_group[is.na(stacked_sub$pop_group)] <- "high"

stacked_sub <- 
stacked_sub %>% 
  mutate(different = ifelse(`maintenance-cost` == 0.08| `resource-growth-rate-linear` == 0.225, T, F))

print(stacked_sub$disturbance.interval)

# generate figure
(Fig_SX5 <- 
    ggplot(stacked_sub, aes(y = medianBT, x = medianLH))+
    geom_point(data = dplyr::select(stacked_sub %>% filter(var == "default" & sim.id == sim.id[1]), -var), color = "gray")+
    geom_line(data = dplyr::select(stacked_sub %>% filter(var == "default") %>% filter(sim.id == sim.id[[1]]), -var), aes(group = tot_coefvar, color = as.factor(disturbance.interval)), color = "gray")+
    geom_line(aes(group = tot_coefvar, color = as.factor(disturbance.interval)),  alpha = 1)+
    geom_point(aes(color = as.factor(disturbance.interval), shape = gt_group),  alpha = 1, size = 2)+
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, color = ifelse(stacked_sub$different, "red", "white"), fill = NA)+
    scale_shape_manual("POL (generation time)", values  =c(17,1))+
    theme_clean()+
    scale_color_manual("Disturbance interval", values = c("coral1", "turquoise"))+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Responsiveness")+
    xlab("Reproductive investment threshold")+
    guides(size = guide_legend(title = "POL (generation time)"))+
    facet_wrap(~var))

#save figure
ggsave(here("Figs", sim.date, "supplemental", "S1_K_OneAtATimeSensitivity.jpeg"), Fig_SX5, width = 17, height = 12, units = "cm")
ggsave(here("Figs", sim.date, "supplemental", "S1_K_OneAtATimeSensitivity.pdf"), Fig_SX5, width = 17, height = 12, units = "cm")
