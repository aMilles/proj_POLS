library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)


if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"Supplement_HighFreqLowIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

df <- stacked <- read_csv(out.path)
source(here("code", "rscript", "create_main_text_figures", "main_text_Fig4_1stPrediction_PanelD.R"))




# create a subset where fast and slow end of each population is retained
NoFluctuation_data_sub <- stacked %>%
  group_by(disturbance.intensity, disturbance.interval) %>% 
  filter(sim.id == sim.id[1]) %>% 
  group_by(sim.id) %>% 
  filter(!duplicated(generation_time)) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end")) 

# POL axes along gradients of population density

(p1_plot_popdensGenerationTime <- ggplot(NoFluctuation_data_sub, aes(x = pop_dens/2500 , y = log10(generation_time), color = tot_coefvar, group = sim.id))+
    scale_color_viridis_c("Coefficient of\nvariation", values = c(0, 0.1, 0.3, 1), breaks = c(.2, .6), labels = c("0.2 (stable)", "0.6 (labile)"))+
    geom_line()+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_shape_manual("Generation\ntime", values  =c(17,1))+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    
    theme_clean()+
    theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Generation  \ntime (log10)  ")+
    scale_x_continuous(breaks = c(.25, 0.75))+
    scale_y_continuous(breaks = c(2.2, 3))+
    xlab("Population density [n/patch]"))


# distribution of life history trait and behavioural trait under different densities and coefficients of variation
(p1_plot_BRN_paper <-
    ggplot(NoFluctuation_data_sub, aes(y = medianBT, x = medianLH))+
    geom_point(data = stacked, color = "gray90")+
    geom_line(aes(group = sim.id, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", values = c(0, 0.1, 0.3, 1), breaks = c(.2, .6), labels = c("0.2 (stable)", "0.6 (labile)"))+
    scale_shape_manual("Generation time", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Responsiveness")+
    xlab("Reproductive investment\nthreshold")+
    guides(size = guide_legend(title = "Population\nDensity"), shape = guide_legend(direction = "vertical")))



# Distribution of movement behaviour and investment to reproduction
(p1_plot_phenotypic_behaviour_paper <- 
    ggplot(NoFluctuation_data_sub, aes(y = median_movement_activity, x = median_repo_activity))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", breaks = c(round(min(NoFluctuation_data_sub$tot_coefvar), 1)+.1, round(max(NoFluctuation_data_sub$tot_coefvar), 1)), labels = c(paste0(round(min(NoFluctuation_data_sub$tot_coefvar), 1)+.1, " (stable)"), paste0(round(max(NoFluctuation_data_sub$tot_coefvar), 1), " (labile)")), values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Generation\ntime", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    scale_x_continuous(breaks = c(.1, .3, .5))+
    scale_y_continuous(breaks = c(.15,.2,.25))+
    theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Movement\nrate")+
    xlab("Rate of investment\nto reproduction")+
    guides(size = guide_legend(title = "Population\nDensity")))

legend <- grid::rasterGrob(jpeg::readJPEG(here("figs", sim.date, "supplemental", "legend_box_lowtotcoefvar.jpg")))


# arrange figure 4 in a grid
NoFluctuation <- 
  gridExtra::grid.arrange(
    legend,
    ggdraw(p1_plot_popdensGenerationTime + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
    ggdraw(p1_plot_BRN_paper + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"),
    ggdraw(p1_plot_phenotypic_behaviour_paper + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("C"),
    ggdraw(gg_inclination  + theme(plot.background = element_rect(fill = NA, color = NA), legend.position = "none")) + draw_plot_label("D"),
    layout_matrix = matrix(c(1,1,2,2,1,1,2,2,3,3,3,3,3,3,3,3,4,4,5,5,4,4,5,5),nrow = 4))


# save figure 4
ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_F_NoFluctuations", ".jpeg")), NoFluctuation, width = 16, height = 10, units = "cm", dpi = 600)
ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_F_NoFluctuations", ".pdf")), NoFluctuation, width = 16, height = 10, units = "cm", dpi = 600)

