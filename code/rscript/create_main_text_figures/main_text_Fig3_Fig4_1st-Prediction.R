library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)


# read csv file with aggregated data
if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  L3 = "MainText_LowFreqHighIntensity"
  out.path <- here("simulations", sim.date,L3,  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

df <- stacked <- read_csv(out.path)
source(here("code", "rscript", "create_main_text_figures", "main_text_Fig4_1stPrediction_PanelD.R"))

####################
#### FIGURE 3 ######
####################

# Stylizied intitial distribution of traits

Fig3A_data <- data.frame(x = runif(400, max = 2), y = runif(400, max = 2))

(Fig3A <- ggplot()+  
    geom_point(data = Fig3A_data, aes(x = x, y = y), shape = 21, size = .5)+
    ylab("Responsiveness")+
    xlab("Reproductive investment threshold")+
    geom_rect(data = stacked, aes(ymax = min(medianBT), ymin = max(medianBT), xmin = min(medianLH), xmax = max(medianLH)), fill = NA, color = "gray50", size = 2)+
    theme_clean()+
    scale_x_continuous(breaks = c(0, 1, 2), labels = c("0", 1, "2"))+
    scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", 1, "2"))+
  theme(text = element_text(size = 8), plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank()))

# create figure with traits on y and x and generation time as color scale wiith aggregated (not subsetted)
(Fig3B <- ggplot(stacked, aes(y = medianBT, x = medianLH))+
    geom_point(aes(fill = log10(generation_time)),  alpha = 1, shape = 21, color = "white")+
    geom_smooth(method = "lm", se = F, formula = y ~ poly(x,2), color = "black", size = 1, linetype = "dashed")+
    scale_fill_viridis_c("Generation time\n(log 10)", breaks = c(2.2, 3), labels = c("2.2 (fast)", "3 (slow)"), direction = -1)+
    ylab("Responsiveness")+
    xlab("Reproductive investment threshold")+
    theme_clean()+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 9), legend.text = element_text(size = 9), legend.key.height = unit(4, "mm"), plot.background = element_rect(fill = NA, color = NA), panel.grid.major.y = element_blank()))


# arrange figure 3 in a grid
Fig3 <- 
  gridExtra::grid.arrange(
    ggdraw(Fig3A + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
    ggdraw(Fig3B + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"), 
    layout_matrix = matrix(c(rep(1, 6), rep(2, 7)), ncol = 1, nrow = 13))

# save figure 3
ggsave(here::here("figs", sim.date, "main_text", paste0("Fig3", ".jpeg")), Fig3, width = 8, height = 14.5, units = "cm", dpi = 600)


####################
#### FIGURE 4 ######
####################

# using the subsetted data

Fig4_data_sub <- stacked %>%
  group_by(disturbance.intensity, disturbance.interval) %>% 
  filter(sim.id == sim.id[1]) %>% 
  group_by(sim.id) %>% 
  filter(!duplicated(generation_time)) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end"))

(p1_plot_popdensGenerationTime <- ggplot(Fig4_data_sub, aes(x = pop_dens/2500 , y = log10(generation_time), color = tot_coefvar, group = sim.id))+
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


# FIG 4 A: distribution of life history trait and behavioural trait under different densities and coefficients of variation
(p1_plot_BRN_paper <-
    ggplot(Fig4_data_sub, aes(y = medianBT, x = medianLH))+
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

#load(here("figs", sim.date, "main_text", "inclination.RData"))


# Fig 4 B: Distribution of movement behaviour and investment to reproduction
(p1_plot_phenotypic_behaviour_paper <- 
    ggplot(Fig4_data_sub, aes(y = median_movement_activity, x = median_repo_activity))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = gt_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", breaks = c(round(min(Fig4_data_sub$tot_coefvar), 1)+.1, round(max(Fig4_data_sub$tot_coefvar), 1)), labels = c(paste0(round(min(Fig4_data_sub$tot_coefvar), 1)+.1, " (stable)"), paste0(round(max(Fig4_data_sub$tot_coefvar), 1), " (labile)")), values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Generation\ntime", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    scale_x_continuous(breaks = c(.1, .3, .5))+
    scale_y_continuous(breaks = c(.15,.2,.25))+
    theme(legend.position = "none", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())+
    ylab("Movement\nrate")+
    xlab("Rate of investment\nto reproduction")+
    guides(size = guide_legend(title = "Population\nDensity")))

legend <- grid::rasterGrob(jpeg::readJPEG(here("figs", sim.date, "main_text", "legend_box.jpg")))


# arrange figure 4 in a grid
Fig4 <- 
  gridExtra::grid.arrange(
    legend,
    ggdraw(p1_plot_popdensGenerationTime + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("A"),
    ggdraw(p1_plot_BRN_paper + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("B"),
    ggdraw(p1_plot_phenotypic_behaviour_paper + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("C"),
    ggdraw(gg_inclination  + theme(plot.background = element_rect(fill = NA, color = NA), legend.position = "none")) + draw_plot_label("D"),
    layout_matrix = matrix(c(1,1,2,2,1,1,2,2,3,3,3,3,3,3,3,3,4,4,5,5,4,4,5,5),nrow = 4))

# save figure 4
ggsave(here::here("figs", sim.date, "main_text", paste0("Fig4", ".jpeg")), Fig4, width = 16, height = 10, units = "cm", dpi = 600)

