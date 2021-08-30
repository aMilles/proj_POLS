library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)


# read main text results

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}


stacked <- read_csv(out.path)

# determine slow and fast end
Fig4_data_sub <- stacked %>% 
  group_by(sim.id) %>% 
  filter(generation_time == max(generation_time) | generation_time == min(generation_time)) %>% 
  mutate(gt_group = ifelse(generation_time == min(generation_time), "fast end", "slow end"))

# split data by disturbance regime (each disturbance regime is repeated 5 times)
# determine one as the observation and the others as repeats
split_Fig4data <- split(Fig4_data_sub, paste0(Fig4_data_sub$disturbance.interval, Fig4_data_sub$disturbance.intensity))

facet_data <- lapply(split_Fig4data, function(x){
  if(nrow(x) > 0){
    x <- x %>% mutate(default = F)
    other <- Fig4_data_sub[!Fig4_data_sub$sim.id %in% x$sim.id,] %>% 
      mutate(default = T) %>% 
      mutate(disturbance.interval = x$disturbance.interval[1]) %>% 
      mutate(disturbance.intensity = x$disturbance.intensity[1])
    
    return(rbind(x, other))
  }
})

facet_data_bound<- do.call(rbind, facet_data)%>% 
  mutate(disturbance.interval = paste0("Dist. interval: ", disturbance.interval)) %>% 
  mutate(disturbance.intensity = paste0("Dist. intensity: ", disturbance.intensity))


# generate plot
(repeatability <-
    ggplot(facet_data_bound %>% filter(!default), aes(y = medianBT, x = medianLH))+
    geom_point(data = facet_data_bound %>% filter(default), aes(y = medianBT, x = medianLH), color = "gray90")+
    geom_line(aes(group = sim.id),  color = "coral1", alpha = 1)+
    geom_point(aes(shape = gt_group), color = "coral1",   alpha = 1, size =1)+
    scale_shape_manual("POL (generation time)", values  =c(17,1))+
    theme_clean()+
    scale_size_continuous(range = c(.5,5))+
    theme(legend.key.height = unit(4, "mm"),panel.grid.major.y = element_blank(), legend.position = "top", text = element_text(size = 8), axis.text =  element_text(size = 8), axis.title =  element_text(size = 9), legend.text =  element_text(size = 8), legend.title =  element_text(size = 9))+
    ylab("Responsiveness")+
    xlab("Reproductive investment threshold")+
    scale_y_continuous(breaks = c(1.2, 1.5))+
    scale_x_continuous(breaks = c(1, 1.2))+
    facet_wrap(disturbance.intensity ~ disturbance.interval, scales = "fixed"))

ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_C_Repeatability", ".jpeg")), repeatability, width = 10, height = 10, units = "cm", dpi = 600)
ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_C_Repeatability", ".pdf")), repeatability, width = 10, height = 10, units = "cm", dpi = 600)
