library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)

# read csv file with aggregated data


if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  out.path <- here("simulations", sim.date,"Supplement_altered_movementassumption_POLS",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}

#read data
stacked <- read_csv(out.path)

# create a subset where fast and slow end of each population is retained
stacked_sub <- stacked %>% 
  group_by(sim.id) %>% 
  filter(!duplicated(generation_time)) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end")) %>% 
  mutate(POLSwidth = max(generation_time) - min(generation_time))


# using the subsetted data
SX06_data <- stacked_sub %>% 
  filter(growt.type == "linear") %>% 
  group_by(disturbance.intensity, disturbance.interval, maintenance.cost, resource.growth.rate.linear) %>% 
  filter(sim.id == sim.id[1]) %>% 
  ungroup() %>% 
  mutate(parameter.group = factor(paste0(resource.growth.rate.linear, maintenance.cost), labels = c("less maintenance costs", "default", "higher resource growth\nless maintenance costs", "higher resource growth"))) %>% 
  mutate(maintenance.cost.text = paste0("Maintenance cost: ", maintenance.cost)) %>% 
  mutate(resource.growth.rate.linear.text = paste0("Growth rate, linear: ", resource.growth.rate.linear))

# if true, one population per disturbance regime and aparameter combination
all(SX06_data %>%  select(disturbance.intensity, disturbance.interval, maintenance.cost, resource.growth.rate.linear) %>% table == 2)

# distribution of life history trait and behavioural trait under different densities and coefficients of variation
(SX06_A <-
    ggplot(SX06_data, aes(y = medianBT, x = medianLH))+
    geom_line(aes(group = sim.id, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", values = c(0, 0.1, 0.3, 1), breaks = c(.2, .6), labels = c("0.2 (stable)", "0.6 (labile)"))+
    geom_text(aes(label = parameter.group), x = 1.1, y = 1.1, size = 3)+
    scale_shape_manual("Generation time", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Responsiveness")+
    xlab("Reproductive investment threshold")+
    guides(size = guide_legend(title = "Population\nDensity"), shape = guide_legend(direction = "vertical"))+
    facet_grid(maintenance.cost.text~resource.growth.rate.linear.text))


# Distribution of movement behaviour and investment to reproduction
(SX06_B <- 
    ggplot(SX06_data, aes(y = median_movement_activity, x = median_repo_activity))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", breaks = c(.3, .9), values = c(0, 0.1, 0.3, 1))+
    geom_text(aes(label = parameter.group), y = .25, x = .4, size = 3)+
    scale_shape_manual("Generation\ntime", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Movement rate")+
    xlab("Rate of investment to reproduction")+
    guides(size = guide_legend(title = "Population\nDensity"))+
    facet_grid(maintenance.cost.text~resource.growth.rate.linear.text))

# arrange figures in a grid
SX06 <- 
  gridExtra::grid.arrange(
    ggdraw(SX06_A + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
    ggdraw(SX06_B+ theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"), 
    layout_matrix = matrix(c(rep(1, 4), rep(2, 6)), ncol = 1, nrow = 10))

# save arranged figures
ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_L_POLAxesRelaxedAssumption", ".jpeg")), SX06, width = 14, height = 20, units = "cm", dpi = 600)

