library(tidyverse)
library(here)
library(ggthemes)
library(mgcv)
library(ggridges)
library(patchwork)

# read aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_2ndStep_2ndfilter", "output_intermediate_stacked", "stacked_output_2ndStep_2ndfilter.csv")
}

#read simulations

sims <- read_csv(out.path) %>% 
  mutate(pop_dens = pop_dens / 2500) %>% 
  mutate(who = paste0(sim.id, "_", who)) %>% 
  mutate(parental_who = paste0(sim.id, "_", parental_who))

# select individuals that have a parent in the dataset
childs <- 
sims %>% 
  filter(parental_who %in% who) %>% 
  select(who, birth.tick, parental_who, pop_dens_at_birth) %>% 
  setNames(c("child", "birth.date", "who", "my_pop_dens"))


# get the age of the parent at the time of reproduction and calculate the mean age at reproduction over time
GToverTime_joined <- 
childs %>% 
  left_join(sims %>% select(sim.id, who, birth.tick, death.tick, pop_dens, death.cause, BT, LH)) %>% 
  select(child, who, birth.date, sim.id, birth.tick, death.tick, pop_dens, death.cause, my_pop_dens, BT, LH)

GToverTime <- GToverTime_joined %>% 
  filter(!duplicated(child)) %>% 
  filter(birth.date > 15000) %>% 
  mutate(ageatrepro = birth.date - birth.tick) %>%
  group_by(sim.id, cut(birth.date, 100)) %>%
  summarise(ageatrepro = mean(ageatrepro), 
            time = mean(birth.date), 
            pop_dens = mean(my_pop_dens), 
            growth_rate = length(pop_dens)/(pop_dens*2500), 
            medianBT = median(BT), 
            medianLH = median(LH))

# plot 
(gg_GToverTime <-
ggplot(GToverTime, aes(x = pop_dens / 2500, y = log10(ageatrepro)))+
  geom_point(size = .05)+
  labs(y = "Mean age at reproduction [log 10]", 
       x = "Population density [individuals per patch]")+
  theme_clean()+
  theme(panel.grid.major.y = element_blank(), plot.background = element_blank()))

(gg_POLaxes <- ggplot(GToverTime, aes(x = medianLH, y = medianBT, color = log10(ageatrepro)))+
  geom_point(size = .005)+
  scale_color_viridis_c("Mean age at reproduction (log10)")+
  theme_clean()+
  theme(panel.grid.major.y = element_blank(), legend.position = "top", plot.background = element_blank())+
  labs(y =  "Median responsivenes of parents",
       x = "Median reproductive investment\nthreshold of parents"))

combined <- gg_GToverTime + gg_POLaxes & theme(legend.position = "bottom") 

combined_final <- combined + plot_layout(guides = "collect") + patchwork::plot_annotation(tag_levels = c("A"))


ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_P_AverageAgeAtReproduction", ".jpeg")), 
       combined_final, width = 14, height = 9, units = "cm", dpi = 600)

