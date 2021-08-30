library(tidyverse)
library(here)
library(ggthemes)

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}


stacked <- read_csv(out.path)

(Fig6 <- 
ggplot()+
  geom_point(data = stacked, aes(x = pop_dens/2500, y = r0, fill = log10(generation_time), color =log10(generation_time), group = generation_time),  inherit.aes = F, alpha = 1, shape = 16, size = .5)+
  geom_point(data = stacked, aes(x = pop_dens/2500, y = 0), shape = "|")+
    scale_fill_viridis_c("Generation time\n(log 10)", direction = - 1, breaks = c(2.2, 3), labels = c("2.2 (fast)", "3.0 (slow)"))+
    scale_color_viridis_c("Generation time\n(log 10)", direction = - 1, breaks = c(2.2, 3), labels = c("2.2 (fast)", "3.0 (slow)"))+
  theme_clean()+
  xlab("Population density [n/patch]")+
  ylab("Reproductive rate\n[offspring/time step]")+
  geom_segment(aes(x = 0.2, xend = 0.9, y = 0.008, yend = 0.008), arrow = arrow(length = unit(0.2, "cm")), color = "black")+
  geom_label(aes(x = 0.5, y = 0.008, label = "increasing intraspecific\ncompetition, slower POL"), size = 3)+
  theme(legend.position = "bottom", text = element_text(size = 9), legend.title = element_text(size = 10), legend.text = element_text(size = 9), plot.background = element_rect(fill = NA, color = NA), panel.grid.major.y = element_blank(), strip.text = element_text(size = 8)))

ggsave(here::here("figs", sim.date, "main_text", "Fig6.jpeg"), Fig6, width = 9, height = 8, units = "cm", dpi = 600)
ggsave(here::here("figs", sim.date, "main_text", "Fig6.pdf"), Fig6, width = 9, height = 8, units = "cm", dpi = 600)