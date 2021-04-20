library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)
library(jpeg)

# read csv file with aggregated data
if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
}




plot.loggrowth <- function(df, name, sim.date){
  stacked <- df
  
  # create a subset where fast and slow end of each population is retained

  Loggrowth_data_sub <- stacked %>%
    group_by(disturbance.intensity, disturbance.interval) %>% 
    filter(sim.id == sim.id[1]) %>% 
    group_by(sim.id) %>% 
    filter(!duplicated(generation_time)) %>% 
    filter(generation_time %in% range(generation_time)) %>%
    mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end")) 
  # POL axes along gradients of population density
  
  (p1_plot_popdensGenerationTime <- ggplot(Loggrowth_data_sub, aes(x = pop_dens/2500 , y = log10(generation_time), color = tot_coefvar, group = sim.id))+
      scale_color_viridis_c("Coefficient of\nvariation", values = c(0, 0.1, 0.3, 1), breaks = c(.2, .6), labels = c("0.2 (stable)", "0.6 (labile)"))+
      geom_line()+
      geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
      scale_shape_manual("Generation\ntime", values  =c(17,1))+
      scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
      
      theme_clean()+
      theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
      ylab("Generation  \ntime (log10)  ")+
      scale_x_continuous(breaks = c(.25, 0.75))+
      scale_y_continuous(breaks = c(2.2, 2.8))+
      xlab("Population density [n/patch]"))
  
  
  # distribution of life history trait and behavioural trait under different densities and coefficients of variation
  (p1_plot_BRN_paper <-
      ggplot(Loggrowth_data_sub, aes(y = medianBT, x = medianLH))+
      geom_point(data = stacked, color = "gray80")+
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
      ggplot(Loggrowth_data_sub, aes(y = median_movement_activity, x = median_repo_activity))+
      geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
      geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
      scale_color_viridis_c("Coefficient of\nvariation", breaks = c(round(min(Loggrowth_data_sub$tot_coefvar), 1)+.1, round(max(Loggrowth_data_sub$tot_coefvar), 1)), labels = c(paste0(round(min(Loggrowth_data_sub$tot_coefvar), 1)+.1, " (stable)"), paste0(round(max(Loggrowth_data_sub$tot_coefvar), 1), " (labile)")), values = c(0, 0.1, 0.3, 1))+
      scale_shape_manual("Generation\ntime", values  =c(17,1))+
      theme_clean()+
      scale_x_continuous(breaks = c(.1, .3, .5))+
      scale_y_continuous(breaks = c(.2, .4, .6))+
      theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
      ylab("Movement\nrate")+
      xlab("Rate of investment\nto reproduction")+
      guides(size = guide_legend(title = "Population\nDensity")))
  
  legend <- grid::rasterGrob(jpeg::readJPEG(here("figs", sim.date, "main_text", "legend_box.jpg")))
  
  #create PANEL D
  # intrapopulation POL axes
  df_inclination <-
    df %>% 
    group_by(sim.id) %>% 
    mutate(inclination_intrapop = coefficients(lm(medianBT ~ medianLH))[2]) %>% 
    mutate(popavg_LH = median(medianLH))
  
  # interpopulation POL axis
  fit <- lm(medianBT ~ medianLH + I(medianLH^2), data = df)
  
  
  # first derivative of interpopulation POL axis
  coefs <- coefficients(fit)
  df_inclination$inclination_interpop <- coefs[2] + 2 * coefs[3] * df_inclination$popavg_LH
  

  # inclinations of intrapopulation POL axes vs inclinations interpopulation POL axis
  (gg_inclination <- 
      ggplot(df_inclination, aes(x = inclination_interpop, y = inclination_intrapop))+
      geom_line(data = data.frame(x = seq(-5, 5, length.out = 1000), y = seq(-5, 5, length.out = 1000)), aes(x = x, y =y), linetype = "dashed")+
      xlim(range(df_inclination$inclination_intrapop))+
      ylim(range(df_inclination$inclination_interpop))+
      geom_point(aes(color = tot_coefvar))+
      ylab("Intra-population    \nPOL axes (slope)   ")+
      xlab("Inter-population\nPOL axis (slope)")+
      theme_clean()+
      xlim(range(df_inclination$inclination_interpop))+ 
      ylim(range(df_inclination$inclination_intrapop))+
      scale_color_viridis_c("Coefficient of variation\nin population density", values = c(0, 0.05, .1, .2, .4,1), breaks = c(0.3, 0.9), labels = c("0.3 (stable)", "0.9 (labile)"))+
      theme(legend.position = "bottom", panel.grid.major.y = element_blank()))
  
  
  
  
  # arrange figure 4 in a grid
  Loggrowth <- 
    gridExtra::grid.arrange(
      legend,
      ggdraw(p1_plot_popdensGenerationTime + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
      ggdraw(p1_plot_BRN_paper + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"),
      ggdraw(p1_plot_phenotypic_behaviour_paper + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("C"),
      ggdraw(gg_inclination  + theme(plot.background = element_rect(fill = NA, color = NA), legend.position = "none")) + draw_plot_label("D"),
      layout_matrix = matrix(c(1,1,2,2,1,1,2,2,3,3,3,3,3,3,3,3,4,4,5,5,4,4,5,5),nrow = 4))
  
  # save figure 4
  ggsave(here::here("figs", sim.date, "supplemental", paste0(name, ".jpeg")), Loggrowth, width = 16, height = 10, units = "cm", dpi = 600)
}

plot.loggrowth(
  df = read_csv(here("simulations", sim.date,"Supplement_LowFreqHighIntensity_loggrowthVerhulst",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")), 
  name = "S1_H_Verhulst",
  sim.date = sim.date)

plot.loggrowth(
  df = read_csv(here("simulations", sim.date,"Supplement_LowFreqHighIntensity_loggrowthRichards",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")), 
  name = "S1_I_Richards",
  sim.date = sim.date)

