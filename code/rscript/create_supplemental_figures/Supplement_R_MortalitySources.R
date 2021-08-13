library(tidyverse)
library(here)
library(ggthemes)


if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_2ndStep_2ndfilter", "output_intermediate_stacked", "stacked_output_2ndStep_2ndfilter.csv")
  out.path2 <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}
names(stacked)

# select all individuals from populations with the weakes and stronges density fluctuations
stacked <- read_csv(out.path2) %>% 
  select(sim.id, disturbance.interval, disturbance.intensity, tot_coefvar) %>% 
  filter(tot_coefvar %in% range(tot_coefvar))

deaths <- read_csv(out.path) %>% 
  mutate(pop_dens = pop_dens / 2500)  

# add data from stacked output to individual data
deaths.single.sim <- deaths %>% 
  filter(sim.id %in% stacked$sim.id) %>% 
  left_join(stacked)

# stratify the so it can be arranged in a raster
deaths_subset <- deaths.single.sim %>%
  mutate(starvation = death.cause == "soma") %>%
  mutate(tot_coefvar = ifelse(tot_coefvar == max(tot_coefvar), "Strong density fluctuations", "Weak density fluctuations")) %>% 
  group_by(tot_coefvar) %>% 
  mutate(pop_dens = paste("Population density range:", cut(pop_dens,2))) %>% 
  mutate(LH = ceiling(LH * 5)/5) %>% 
  mutate(BT = ceiling(BT * 5)/5)

# calculate the proportion of individuals that died due to starvation
# for each unique combination of traits, population density, and population density fluctuation
deaths_subset_agg <- deaths_subset %>% 
  group_by(BT, LH, pop_dens, tot_coefvar) %>% 
  mutate(starvation = sum(starvation)/length(starvation)) %>% 
  filter(!duplicated(paste0(BT, LH, pop_dens, tot_coefvar)))

# set theme

theme_set(theme_clean(base_size = 12))
theme_replace(panel.grid.major.y =  element_blank())
theme_replace(plot.background =  element_blank())
theme_replace(legend.box = "vertical")
theme_replace(legend.title = element_text(size = 11))
theme_replace(legend.background = element_blank())

# generate plot

mortality <- 
  ggplot(deaths_subset_agg, aes(x = LH, y =BT, color = starvation, fill = (starvation * 100)))+
  geom_raster()+
  scale_fill_viridis_c("Risk of starvation [%]", 
                       option = "B", 
                       direction = -1, 
                       na.value = "lightyellow",
                       values = c(0, .2, .5, 1))+
  facet_wrap(~pop_dens + tot_coefvar)+
  labs(
    x = "Reproductive investment threshold",
    y = "Responsiveness"
  )+
  theme(legend.position = "top")


ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_R_MortalitySources", ".jpeg")), mortality, width = 15, height = 18, units = "cm", dpi = 600)
