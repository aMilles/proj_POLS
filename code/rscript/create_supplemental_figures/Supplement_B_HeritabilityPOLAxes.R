library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)


if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  L3 <- "Supplement_LowFreqHighIntensity_highh2"
  out.path <- here("simulations", sim.date,"Supplement_LowFreqHighIntensity_highh2",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}


highh2_df <- df <- stacked <- read_csv(out.path)
source(here("code", "rscript", "create_main_text_figures", "main_text_Fig4_1stPrediction_PanelD.R"))


out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
default_df <- read_csv(out.path)

stacked_both <- rbind(highh2_df, default_df) %>% 
  group_by(stochasticity.BT, disturbance.interval, disturbance.intensity) %>% 
  filter(sim.id == sim.id[1]) %>% 
  ungroup() %>% 
  group_by(sim.id) %>% 
  filter(!duplicated(generation_time)) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end"))

Highh2 <- 
ggplot(stacked_both, aes(x = pop_dens/2500, y = medianBT, color = factor(stochasticity.BT, labels = c("high", "low")), shape = gt_group))+
    geom_point()+
    geom_line(aes(group = sim.id))+
  scale_color_discrete("Heritability [h²]")+
    xlab("Population density [individuals/patch]")+
    ylab("Responsiveness")+
  scale_shape_manual("Generation\ntime", values  =c(17,1))+
theme_clean()+
  theme(legend.position = "right", text = element_text(size = 8), legend.title = element_text(size = 8), legend.text = element_text(size = 8), legend.box = "vertical", legend.key.height = unit(4, "mm"), panel.grid.major.y = element_blank())


ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_B_PopDensResponsivenessHeritability", ".jpeg")), Highh2, width = 12, height = 10, units = "cm", dpi = 600)

