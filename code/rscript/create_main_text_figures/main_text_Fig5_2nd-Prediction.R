library(tidyverse)
library(here)
library(ggthemes)


# read aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}



stacked <- read_csv(out.path) 

##################
#### FIGURE 5 ####
##################

# calculate scaled interquartile range for traits and phenotypic behaviour (only data with logistic growth model)

Fig5_data <-
  stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
  mutate("Movement rate\n(scaled SD)" = scale(sd_movement_activity)) %>% 
  mutate("Rate of investment\nto reproduction (scaled SD)" = scale(sd_repo_activity)) %>% 
  mutate("Responsiveness\n(scaled SD)" = scale(sdBT)) %>% 
  mutate("Reproductive investment\nthreshold (scaled SD)" = scale(sdLH)) %>% 
  dplyr::select(generation_time, "Movement rate\n(scaled SD)", "Rate of investment\nto reproduction (scaled SD)", "Responsiveness\n(scaled SD)", "Reproductive investment\nthreshold (scaled SD)") %>% 
  reshape2::melt(id.vars = c("generation_time")) %>% 
  mutate(variable = factor(as.character(variable), levels = rev(unique(variable)), labels = rev(unique(variable))))

# create figure 5
(Fig5 <- 
    ggplot(Fig5_data, aes(x = log10(generation_time), y = value))+
    geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
    theme_clean() +
    theme(plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank(), strip.placement = "outside")+
    facet_wrap(~variable, ncol = 2, strip.position = "left")+
    xlab("Generation time (log10)") +
    ylab("")+
  scale_x_continuous(breaks = c(2, 2.5, 3, 3.5)))

# save figure 5
ggsave(here::here("figs", sim.date, "main_text", "Fig5.jpeg"), Fig5, width = 10, height = 11, units = "cm", dpi = 600)
ggsave(here::here("figs", sim.date, "main_text", "Fig5.pdf"), Fig5, width = 10, height = 11, units = "cm")

