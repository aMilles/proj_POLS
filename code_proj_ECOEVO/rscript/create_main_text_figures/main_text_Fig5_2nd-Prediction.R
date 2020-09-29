library(shiny)
library(tidyverse)
library(here)
library(ggthemes)


# read aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  
  out.path <- here("simulations", sim.date,"Main_Predictions",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

stacked <- read_csv(out.path) 

  
##################
#### FIGURE 5 ####
##################

# calculate scaled interquartile range for traits and phenotypic behaviour (only data with logistic growth model)
stacked$sd
Fig5_data <-
  stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
  filter(growth_type == "logistic") %>% 
  mutate("Movement rate" = scale(sd_movement_activity)) %>% 
  mutate("Rate of investment\nto reproduction" = scale(sd_repo_activity)) %>% 
  mutate("Responsiveness (BT)" = scale(sdBT)) %>% 
  mutate("Relative investment\nto reproduction (LH)" = scale(sdLH)) %>% 
  dplyr::select(generation_time, "Movement rate", "Rate of investment\nto reproduction", "Responsiveness (BT)", "Relative investment\nto reproduction (LH)") %>% 
  reshape2::melt(id.vars = c("generation_time"))

# create figure
(Fig5 <- 
    ggplot(Fig5_data, aes(x = log10(generation_time), y = value))+
    geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
    #geom_smooth(se = F, method = "lm", formula = y ~ poly(x,3), col = "coral1")+
    theme_clean() +
    theme(plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank())+
    facet_wrap(~variable)+
    xlab("Generation time (log10)") +
    ylab("SD of behavioural expression and traits (scaled)")+
  scale_x_continuous(breaks = c(2, 2.5, 3)))


ggsave(here::here("figs", sim.date, "main_text", "Fig5.png"), Fig5, width = 9, height = 9, units = "cm", dpi = 600)
