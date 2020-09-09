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
  out.path <- here("simulations", sim.date,"Additional_test",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}

stacked <- read_csv(out.path) %>% 
  mutate(medianBT = 2 - medianBT)
summary(stacked$pop_dens)
# create a subset where state of lowest and highest population density is retained
stacked_sub <- stacked %>% 
  group_by(tot_coefvar) %>%
  mutate(pop_group = order(pop_dens)) %>% 
  filter(pop_group %in% range(pop_group)) %>% 
  mutate(pop_group = factor(pop_group, levels = c("1", "10"), labels  = c("low", "high")))


# using the subsetted data
SX06_data <- stacked_sub %>%  filter(growth_type == "logistic")

# FIG 4 A: distribution of life history trait and behavioural trait under different densities and coefficients of variation
(SX06_A <- 
    ggplot(SX06_data, aes(x = medianBT, y = medianLH))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = pop_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of variation", values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Population density", values  =c(1,16))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"))+
    xlab("Responsiveness (2-BT)")+
    ylab("Relative investment\nto reproduction (LH)")+
    guides(size = guide_legend(title = "Population\nDensity")))

# Fig 4 B: Distribution of movement behaviour and investment to reproduction
(SX06_B <- 
    ggplot(SX06_data, aes(x = median_movement_activity, y = median_repo_activity))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = pop_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", breaks = c(round(min(SX06_data$tot_coefvar), 1) + 0.2, round(max(SX06_data$tot_coefvar), 1) - 0.2), labels = c(paste0(round(min(SX06_data$tot_coefvar), 1) + 0.2, " (stabile)"), paste0(max(round(SX06_data$tot_coefvar, 1)) - 0.2, " (labile)")), values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Population\ndensity", values  =c(1,16))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    theme(legend.position = "right", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"))+
    xlab("Movement rate")+
    ylab("Rate of investment to reproduction")+
    guides(size = guide_legend(title = "Population\nDensity")))



SX06 <- 
  gridExtra::grid.arrange(
    ggdraw(SX06_A + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
    ggdraw(SX06_B+ theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"), 
    layout_matrix = matrix(c(rep(1, 4), rep(2, 6)), ncol = 10, nrow = 1))


ggsave(here::here("figs", sim.date, "supplemental", paste0("Fig_SX06", ".png")), SX06, width = 16, height = 8, units = "cm", dpi = 600)

