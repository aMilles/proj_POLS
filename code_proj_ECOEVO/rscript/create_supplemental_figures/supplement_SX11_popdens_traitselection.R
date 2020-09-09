library(shiny)
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

stacked <- read_csv(out.path) %>% 
  mutate(medianBT = 2 - medianBT)


# create a subset where state of lowest and highest population density is retained
stacked_sub <- stacked %>% 
  group_by(tot_coefvar) %>%
  mutate(pop_group = order(pop_dens)) %>% 
  filter(pop_group %in% range(pop_group)) %>% 
  mutate(pop_group = factor(pop_group, levels = c("1", "10"), labels  = c("low", "high")))

# using the subsetted data
SX11_data <- stacked_sub %>%  filter(growth_type == "logistic") %>% mutate(pop_dens = pop_dens / 62500)

# FIG 4 A: distribution of life history trait and behavioural trait under different densities and coefficients of variation
(Fig_SX11 <- 
    ggplot(SX11_data, aes(x = medianBT, y = medianLH))+
    geom_line(aes(group = tot_coefvar, color = pop_dens > .9),  alpha = 1)+
    geom_point(aes(color = pop_dens > .9, shape = pop_group),  alpha = 1, size = 2)+
    scale_color_manual("Population density > .9", values = c("turquoise", "coral1"))+
    scale_shape_manual("Population density\n(relative)", values  =c(1,16))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"), legend.box = "vertical", plot.background = element_blank())+
    xlab("Responsiveness (2-BT)")+
    ylab("Relative investment\nto reproduction (LH)")+
    guides(size = guide_legend(title = "Population\nDensity")))


ggsave(here::here("figs", sim.date, "supplemental", paste0("Fig_SX11.png")), Fig_SX11, width = 9, height = 9, units = "cm", dpi = 600)

