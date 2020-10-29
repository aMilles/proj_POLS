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

################################
#### SUPPLEMENTAL FIGURE X1 ####
################################

# SAME as FIG 5 with linear growth


# calculate scaled interquartile range for traits and phenotypic behaviour (only data with linear growth model)
SX1_data <-
    stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
    filter(growth_type == "linear") %>% 
    mutate("Movement rate\n(scaled SD)" = scale(sd_movement_activity)) %>% 
    mutate("Rate of investment\nto reproduction (scaled SD)" = scale(sd_repo_activity)) %>% 
    mutate("Responsiveness\n(BT, scaled SD)" = scale(sdBT)) %>% 
    mutate("Relative investment to\nreproduction (LH, scaled SD)" = scale(sdLH)) %>% 
    dplyr::select(generation_time, "Movement rate\n(scaled SD)", "Rate of investment\nto reproduction (scaled SD)", "Responsiveness\n(BT, scaled SD)", "Relative investment to\nreproduction (LH, scaled SD)") %>% 
    reshape2::melt(id.vars = c("generation_time"))

# create figure
(SX1<- 
        ggplot(SX1_data, aes(x = log10(generation_time), y = value))+
        geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
        theme_clean() +
        theme(plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank(), strip.placement = "outside")+
        facet_wrap(~variable, ncol = 2, strip.position = "left")+
        xlab("Generation time (log10)") +
        ylab("")+
        scale_x_continuous(breaks = c(2, 2.5, 3)))



ggsave(here::here("figs", sim.date, "supplemental", "Fig_SX01.jpeg"), SX1, width = 9, height = 10, units = "cm", dpi = 600)


################################
#### SUPPLEMENTAL FIGURE X3 ####
################################


# calculate scaled difference between median and 75 quantile to exclude effects from lower boundary on variation for traits and phenotypic behaviour (only data with linear growth model)
SX3_data <-
    stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
    filter(growth_type == "logistic") %>% 
    mutate("Movement activity\n(75% quantile - median; scaled)" = scale(upper_movement_activity - median_movement_activity)) %>% 
    mutate("Investment to\nreproduction\n(75% quantile - median; scaled)" = scale(upper_repo_activity - median_repo_activity)) %>% 
    mutate("Responsiveness\n(BT; 75% quantile - median; scaled)" = scale(upperBT - medianBT)) %>% 
    mutate("Relative investment\nto reproduction\n(LH; 75% quantile - median; scaled)" = scale(upperLH - medianLH)) %>% 
    dplyr::select(generation_time, "Movement activity\n(75% quantile - median; scaled)", "Investment to\nreproduction\n(75% quantile - median; scaled)", "Responsiveness\n(BT; 75% quantile - median; scaled)", "Relative investment\nto reproduction\n(LH; 75% quantile - median; scaled)") %>% 
    reshape2::melt(id.vars = c("generation_time"))

# create plot
(SX3 <- 
        ggplot(SX3_data, aes(x = log10(generation_time), y = value))+
        geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
        theme_clean() +
        theme(plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank(), strip.placement = "outside")+
        facet_wrap(~variable, ncol = 2, strip.position = "left")+
        xlab("Generation time (log10)") +
        ylab("")+
        scale_x_continuous(breaks = c(2, 2.5, 3)))

# save plot
ggsave(here::here("figs", sim.date, "supplemental", "Fig_SX03.jpeg"), SX3, width = 9, height = 13, units = "cm", dpi = 600)

