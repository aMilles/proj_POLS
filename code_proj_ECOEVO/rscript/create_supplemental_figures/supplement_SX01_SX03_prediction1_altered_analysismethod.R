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
    mutate("Movement activity" = scale(upper_movement_activity - lower_movement_activity)) %>% 
    mutate("Investment\nto reproduction" = scale(upper_repo_activity - lower_repo_activity)) %>% 
    mutate("Behavioural trait" = scale(upperBT - lowerBT)) %>% 
    mutate("Life-history trait" = scale(upperLH - lowerLH)) %>% 
    dplyr::select(generation_time, "Movement activity", "Investment\nto reproduction", "Behavioural trait", "Life-history trait") %>% 
    reshape2::melt(id.vars = c("generation_time"))


# create plot
(SX1 <- 
        ggplot(SX1_data, aes(x = generation_time, y = value))+
        geom_point(alpha = .2, shape = 16, color = "black")+
        geom_smooth(se = F, method = "loess", formula = y ~ x, col = "coral1")+
        theme_clean() +
        theme(plot.background = element_rect(fill = NA, color = NA))+
        facet_wrap(~variable)+
        xlab("Generation time") +
        ylab("IQR of phenotypic behaviour and traits (scaled)")+
        scale_x_continuous(breaks = c(0, 1000, 2000)))


ggsave(here::here("figs", sim.date, "supplemental", "Fig_SX01.png"), SX1, width = 9, height = 10, units = "cm", dpi = 600)


################################
#### SUPPLEMENTAL FIGURE X3 ####
################################


# calculate scaled difference between median and 75 quantile to exclude effects from lower boundary on variation for traits and phenotypic behaviour (only data with linear growth model)
SX3_data <-
    stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
    filter(growth_type == "logistic") %>% 
    mutate("Movement activity" = scale(upper_movement_activity - median_movement_activity)) %>% 
    mutate("Investment to\nreproduction" = scale(upper_repo_activity - median_repo_activity)) %>% 
    mutate("Behavioural trait" = scale(upperBT - medianBT)) %>% 
    mutate("Life-history trait" = scale(upperLH - medianLH)) %>% 
    dplyr::select(generation_time, "Movement activity", "Investment to\nreproduction", "Behavioural trait", "Life-history trait") %>% 
    reshape2::melt(id.vars = c("generation_time"))

# create plot
(SX3 <- 
        ggplot(SX3_data, aes(x = generation_time, y = value))+
        geom_point(alpha = .2, shape = 16, color = "black")+
        geom_smooth(se = F, method = "loess", formula = y ~ x, col = "coral1")+
        theme_clean() +
        facet_wrap(~variable)+
        theme(plot.background = element_rect(fill = NA, color = NA))+
        xlab("Generation time") +
        ylab("Range between median and 75 - % quantile\nof phenotypic behaviour and traits (scaled)")+
        scale_x_continuous(breaks = c(0, 1000, 2000)))

# save plot
ggsave(here::here("figs", sim.date, "supplemental", "Fig_SX03.png"), SX3, width = 9, height = 13, units = "cm", dpi = 600)

