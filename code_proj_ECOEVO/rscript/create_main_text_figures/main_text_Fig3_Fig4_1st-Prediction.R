library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)

# read csv file with aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  
  out.path <- here("simulations", sim.date,"Main_Predictions",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}


stacked <- read_csv(out.path)

# create a subset where state of lowest and highest population density is retained
stacked_sub <- stacked %>% 
  group_by(tot_coefvar) %>%
  mutate(pop_group = order(pop_dens)) %>% 
  filter(pop_group %in% range(pop_group)) %>% 
  mutate(pop_group = factor(pop_group, levels = c("1", "10"), labels  = c("low", "high")))

####################
#### FIGURE 3 ######
####################


# Stylizied intitial distribution of traits

Fig3A_data <- data.frame(x = runif(400), y = runif(400, max = 2))

(Fig3A <- ggplot()+  
    geom_point(data = Fig3A_data, aes(x = x, y = y), shape = 21, size = .5)+
    ylab("Responsiveness (BT)")+
    xlab("Relative investment\nto reproduction (LH)")+
    geom_rect(data = stacked %>%  filter(growth_type == "logistic"), aes(ymax = min(medianBT), ymin = max(medianBT), xmin = min(medianLH), xmax = max(medianLH)), fill = NA, color = "gray50", size = 2)+
    theme_clean()+
    scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", .5, "1"))+
    scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", 1, "2"))+
  theme(text = element_text(size = 8), plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank()))

# create figure with traits on y and x and generation time as color scale wiith aggregated (not subsetted)
(Fig3B <- ggplot(stacked %>% filter(growth_type == "logistic"), aes(y = medianBT, x = medianLH))+
    geom_point(aes(fill = log10(generation_time)),  alpha = 1, shape = 21, color = "white")+
    geom_smooth(method = "lm", se = F, formula = y ~ poly(x,2), color = "black", size = 1, linetype = "dashed")+
    scale_fill_viridis_c("Generation time\n(log 10)", breaks = c(2, 3), labels = c("2 (fast)", "3 (slow)"))+
    ylab("Responsiveness (BT)")+
    xlab("Relative investment\nto reproduction (LH)")+
    theme_clean()+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 9), legend.text = element_text(size = 9), legend.key.height = unit(4, "mm"), plot.background = element_rect(fill = NA, color = NA)))


# arrange figure 3 in a grid
Fig3 <- 
  gridExtra::grid.arrange(
    ggdraw(Fig3A + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("a"),
    ggdraw(Fig3B + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("b"), 
    layout_matrix = matrix(c(rep(1, 6), rep(2, 7)), ncol = 1, nrow = 13))



# save figure 3
ggsave(here::here("figs", sim.date, "main_text", paste0("Fig3", ".jpeg")), Fig3, width = 8, height = 14.5, units = "cm", dpi = 600)


####################
#### FIGURE 4 ######
####################

# using the subsetted data
Fig4_data <- stacked_sub %>%  filter(growth_type == "logistic")

# FIG 4 A: distribution of life history trait and behavioural trait under different densities and coefficients of variation
(p1_plot_BRN_paper <- 
    ggplot(Fig4_data, aes(y = medianBT, x = medianLH))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = pop_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of variation", values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Population density", values  =c(1,16))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.position = "bottom", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.key.height = unit(4, "mm"))+
    ylab("Responsiveness (BT)")+
  xlab("Relative investment\nto reproduction (LH)")+
    guides(size = guide_legend(title = "Population\nDensity")))

# Fig 4 B: Distribution of movement behaviour and investment to reproduction
(p1_plot_phenotypic_behaviour_paper <- 
    ggplot(Fig4_data, aes(y = median_movement_activity, x = median_repo_activity))+
    geom_line(aes(group = tot_coefvar, color = tot_coefvar),  alpha = 1)+
    geom_point(aes(color = tot_coefvar, shape = pop_group),  alpha = 1, size = 2)+
    scale_color_viridis_c("Coefficient of\nvariation", breaks = c(round(min(Fig4_data$tot_coefvar), 1) + 0.2, round(max(Fig4_data$tot_coefvar), 1) - 0.2), labels = c(paste0(round(min(Fig4_data$tot_coefvar), 1) + 0.2, " (stabile)"), paste0(round(max(Fig4_data$tot_coefvar), 1), " (labile)")), values = c(0, 0.1, 0.3, 1))+
    scale_shape_manual("Population\ndensity", values  =c(1,16))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5), breaks = c(0.05, 0.15, 0.25))+
    theme(legend.position = "right", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"))+
    ylab("Movement rate")+
    xlab("Rate of investment\nto reproduction")+
    guides(size = guide_legend(title = "Population\nDensity")))


# arrange figure 4 in a grid
Fig4 <- 
  gridExtra::grid.arrange(
    ggdraw(p1_plot_BRN_paper + theme(legend.position = "none", plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("a"),
    ggdraw(p1_plot_phenotypic_behaviour_paper + theme(plot.background = element_rect(fill = NA, color = NA))) + draw_plot_label("b"), 
    layout_matrix = matrix(c(rep(1, 4), rep(2, 6)), ncol = 10, nrow = 1))

# save figure 4
ggsave(here::here("figs", sim.date, "main_text", paste0("Fig4", ".jpeg")), Fig4, width = 16, height = 8, units = "cm", dpi = 600)

