library(here)
library(tidyverse)
library(ggthemes)

### compare POL-axes of populations with initially monomorphic vs polymorphic traits

out.path <- here("simulations", sim.date,"Supplement_LowFreqHighIntensity_monomorphic",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")

monomorph_stacked <- read_csv(out.path)

out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed","output_aggregated",  "output_stacked", "stacked_aggregated_output.csv")
polymorph_stacked <- read_csv(out.path)

both_stacked <- rbind(monomorph_stacked %>% mutate(monomorphic = T) %>% mutate(breeding.type = "capital-breeding"), polymorph_stacked %>% mutate(monomorphic = F) %>% mutate(breeding.type = "capital-breeding")) %>% 
  mutate(disturbance.interval = paste0("Dist. interval: ", disturbance.interval)) %>% 
  mutate(disturbance.intensity = paste0("Dist. intensity: ", disturbance.intensity)) %>% 
  mutate(monomorphic = ifelse(monomorphic, "yes", "no"))


(monoVSpolyB <- 
ggplot(both_stacked, aes(x = medianLH, y = medianBT, color = monomorphic, fill = monomorphic, group = monomorphic))+
  geom_point(alpha = .5)+
  facet_wrap(disturbance.intensity~disturbance.interval)+
  ylab("Responsiveness")+
  xlab("Reproductive investment threshold")+
  scale_color_manual("Monomorphic?", values = c("turquoise", "coral1"))+
  scale_fill_manual("Monomorphic?", values = c("turquoise", "coral1"))+
  theme_clean()+
  theme(panel.grid.major.y = element_blank(), legend.position = "top", text = element_text(size = 8), axis.text =  element_text(size = 8), axis.title =  element_text(size = 9), legend.text =  element_text(size = 8), legend.title =  element_text(size = 9))
 +
  scale_x_continuous(breaks = c(0.9, 1.2)))

ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_J_MonomorphicInitialization", ".jpeg")), monoVSpolyB, width = 14, height = 14, units = "cm", dpi = 600)



