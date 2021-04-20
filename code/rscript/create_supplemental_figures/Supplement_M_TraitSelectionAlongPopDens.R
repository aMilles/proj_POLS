library(tidyverse)
library(here)
library(ggthemes)

# read csv file with aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"Supplement_altered_movementassumption_POLS",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

# show parameter groups
stacked <- read_csv(out.path) %>% 
  mutate(maintenance.cost.text = paste0("Maintenance cost: ", maintenance.cost)) %>% 
  mutate(growth.rate.linear.text = paste0("Growth rate, linear: ", resource.growth.rate.linear)) %>% 
  mutate(parameter.group = factor(paste0(resource.growth.rate.linear, maintenance.cost), labels = c("less maintenance costs", "default", "higher res. growth\nless maintenance costs", "higher res. growth"))) %>% 
  mutate(parameter.group =  factor(parameter.group, levels =  unique(parameter.group)[c(2,1,4,3)]))


# check whether all groups have equal proportion
table(stacked[, c("disturbance.interval", "disturbance.intensity", "maintenance.cost", "resource.growth.rate.linear")])

# rearrange data in long format
stacked_melt <- 
  stacked %>% 
  dplyr::select(medianBT, medianLH, median_repo_activity, median_movement_activity, pop_dens, parameter.group) %>% 
  reshape2::melt(id.vars = c("pop_dens", "parameter.group")) %>% 
  mutate(variable = factor(as.character(variable), levels = rev(unique(variable))[c(4,3,1,2)], labels = c("Movement rate", "Reproductive investment rate", "Reproductive investment threshold", "Responsiveness")[c(4,3,1,2)]))

# set color scale
colors <- c(RColorBrewer::brewer.pal(n = 3, "Set2"), "gray50")

# create plot
Fig_SX11 <- 
ggplot(stacked_melt, aes(x = pop_dens/2500, y = value, color = parameter.group))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_point()+
  scale_color_manual("", values = colors)+
  facet_wrap(~variable, scales = "free_y", ncol = 2, strip.position = "left")+
  theme_clean()+
  theme(panel.grid.major.y = element_blank(), strip.placement = "outside", legend.position = "bottom", legend.text = element_text(size = 8))+
  ylab("")+
  xlab("Population density [individual/patch]")



# save plot
ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_M_TraitSelectionAlongPopDens_RelaxesAssumption.jpeg")), Fig_SX11, width = 16, height = 12, units = "cm", dpi = 600)


