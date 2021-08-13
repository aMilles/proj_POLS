library(tidyverse)
library(here)
library(ggthemes)
library(mgcv)
library(ggridges)

# read aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_2ndStep_2ndfilter", "output_intermediate_stacked", "stacked_output_2ndStep_2ndfilter.csv")
}

out.path1 <- here("simulations", sim.date, "MainText_LowFreqHighIntensity",  "processed", "output_2ndStep_2ndfilter")

out.path <- list.files(out.path1, full.names = T)[3]

sims <- read_csv(out.path) %>% 
  mutate(pop_dens = pop_dens / 2500) 





names(sims)

childs <- 
sims %>% 
  filter(parental_who %in% who) %>% 
  select(who, birth.tick, parental_who, pop_dens_at_birth) %>% 
  setNames(c("child", "birth.date", "who", "my_pop_dens"))
names(sims)

GToverTime <- 
childs %>% 
  left_join(sims) %>% 
  select(child, who, birth.date, birth.tick, death.tick, pop_dens, death.cause, my_pop_dens) %>% 
  filter(!duplicated(child)) %>% 
  filter(birth.date > 15000 & birth.date < 30000) %>% 
  mutate(ageatrepro = birth.date - birth.tick) %>%
  group_by(cut(birth.date, 1000)) %>% 
  summarise(ageatrepro = mean(ageatrepro), time = mean(birth.date), pop_dens = mean(my_pop_dens), growth_rate = length(pop_dens)/(pop_dens*2500))

gg_GToverTime <-
ggplot(GToverTime, aes(x = pop_dens / 2500, y = log10(ageatrepro)))+
  geom_point()+
  labs(y = "Mean age at reproduction [log 10]", 
       x = "Population density [individuals per patch]")+
  theme_clean()+
  theme(panel.grid.major.y = element_blank())

ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_P_ageatreproductionovertime", ".jpeg")), 
       gg_GToverTime, width = 14, height = 7.5, units = "cm", dpi = 600)
