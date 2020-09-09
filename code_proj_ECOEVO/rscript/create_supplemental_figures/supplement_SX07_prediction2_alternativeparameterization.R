library(tidyverse)
library(here)
library(ggthemes)

# read csv file with aggregated data

if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  out.path <- here("simulations", sim.date,"Additional_test",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}



stacked <- read_csv(out.path) 

SX07_data <-
  stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
  filter(growth_type == "logistic") %>% 
  mutate("Movement rate" = scale(upper_movement_activity - lower_movement_activity)) %>% 
  mutate("Rate of investment\nto reproduction" = scale(upper_repo_activity - lower_repo_activity)) %>% 
  mutate("Responsiveness (BT)" = scale(upperBT - lowerBT)) %>% 
  mutate("Relative investment\nto reproduction (LH)" = scale(upperLH - lowerLH)) %>% 
  dplyr::select(generation_time, "Movement rate", "Rate of investment\nto reproduction", "Responsiveness (BT)", "Relative investment\nto reproduction (LH)") %>% 
  reshape2::melt(id.vars = c("generation_time"))

# create figure
(SX07 <- 
    ggplot(SX07_data, aes(x = log10(generation_time), y = value))+
    geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
    geom_smooth(se = F, method = "lm", formula = y ~ poly(x,3), col = "coral1")+
    theme_clean() +
    theme(plot.background = element_rect(fill = NA, color = NA))+
    facet_wrap(~variable)+
    xlab("Generation time (log10)") +
    ylab("IQR of behavioural expression and traits (scaled)")+
    scale_x_continuous(breaks = c(2, 2.5, 3)))




ggsave(here::here("figs", sim.date, "supplemental", paste0("Fig_SX07", ".png")), SX07, width = 9, height = 9, units = "cm", dpi = 600)

