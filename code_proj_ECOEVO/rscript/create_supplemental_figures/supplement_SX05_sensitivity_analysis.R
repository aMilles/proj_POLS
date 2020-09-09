library(shiny)
library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)

# read csv file with aggregated data

if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  out.path <- here("simulations", sim.date,"sensitivity_analysis",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}

stacked<- read_csv(out.path) %>% 
  mutate(medianBT = 2 - medianBT)

#identify varied parameter

stacked$var <- NA
names(stacked)[35:55]
for(cols in c(41, 43:53)){
  different_par_row <- which(as.character(as.vector(as.matrix(stacked[,cols]))) != names(sort(table(stacked[,cols]), decreasing = T))[1])
  par_name <- names(stacked)[cols]
  stacked$var[different_par_row] <- apply(cbind(rep(par_name, length(different_par_row)), stacked[different_par_row, par_name]), 1, paste, collapse = ": ")
}

# set parameter sets with no varied parameter to default
stacked$var[(is.na(stacked$var))] <- "default"

#label simulations that stopped prematurely
stacked$stopped <- stacked$max_deathtick < 39000

stacked_sub <- stacked %>% 
  group_by(tot_coefvar) %>%
  mutate(pop_group = order(pop_dens)) %>%
  filter(pop_group %in% range(pop_group)) %>% 
  mutate(stopped = max_deathtick < 39000 & !(duplicated(max_deathtick))) %>% 
  mutate(pop_group = factor(pop_group, levels = c("1", "10"), labels  = c("low", "high")))

stacked_sub$pop_group[is.na(stacked_sub$pop_group)] <- "high"

(Fig_SX5 <- 
    ggplot(stacked_sub, aes(x = medianBT, y = medianLH))+
    geom_text(aes(label = ifelse(stopped, paste("stopped"), NA)), color = "gray")+
    geom_point(data = dplyr::select(stacked_sub %>% filter(var == "default"), -var), color = "gray")+
    geom_line(data = dplyr::select(stacked_sub %>% filter(var == "default"), -var), aes(group = tot_coefvar, color = as.factor(disturbancefrequency)), color = "gray")+
    geom_line(aes(group = tot_coefvar, color = as.factor(disturbancefrequency)),  alpha = 1)+
    geom_point(aes(color = as.factor(disturbancefrequency), shape = pop_group),  alpha = 1, size = 2)+
    scale_shape_manual("Population density", values  =c(1,16))+
    theme_clean()+
    scale_color_manual("Disturbance frequency", values = c("coral1", "turquoise"))+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"))+
    ylim(0.1, 0.8)+
    xlab("Responsiveness (2-BT)")+
    ylab("Relative investment\nto reproduction (LH)")+
    guides(size = guide_legend(title = "Population\nDensity"))+
    facet_wrap(~var))

ggsave(here("Figs", sim.date, "supplemental", "Fig_SX05.png"), Fig_SX5, width = 17, height = 12, units = "cm")
