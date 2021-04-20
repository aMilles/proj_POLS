library(here)
library(tidyverse)
library(ggthemes)

L3 <- "MainText_LowFreqHighIntensity"
sim.date <- "2021-03-28"
out.path <- list.files(here("simulations", sim.date, L3, "processed", "output_2ndstep_2ndfilter"), full.names = T, pattern = "animals")


out_lowh2 <- lapply(as.list(out.path), function(x){
  test <- read_csv(x)
  
  
  merged <- merge(test, test, by.x = "who", by.y = "parental_who")
  
  return(data.frame(
  "Responsiveness" = cor(merged$BT.x, merged$BT.y),
 "Reproductive investment\nthreshold" = cor(merged$LH.x, merged$LH.y),
  "Movement activity" = cor(merged$movement_activity.x, merged$movement_activity.y),
  "Repoductive investment" = cor(merged$repo_activity.x, merged$repo_activity.y)))
  
})


L3 <- "Supplement_LowFreqHighIntensity_highh2"
sim.date <- "2021-03-28"
out.path <- list.files(here("simulations", sim.date, L3, "processed", "output_2ndstep_2ndfilter"), full.names = T, pattern = "animals")


out_highh2 <- lapply(as.list(out.path), function(x){
  test <- read_csv(x)
  
  
  merged <- merge(test, test, by.x = "who", by.y = "parental_who")
  
  return(data.frame(
    "Responsiveness" = cor(merged$BT.x, merged$BT.y),
    "Reproductive investment\nthreshold" = cor(merged$LH.x, merged$LH.y),
    "Movement activity" = cor(merged$movement_activity.x, merged$movement_activity.y),
    "Repoductive investment" = cor(merged$repo_activity.x, merged$repo_activity.y)))
  
})


out <- rbind(do.call(rbind, out_highh2) %>% mutate(h2 = " high h² (sd = 0.05)"), do.call(rbind, out_lowh2) %>% mutate(h2 = " low h² (sd = 0.75)"))


names(out)
(cors <- out %>% 
  dplyr::select(Responsiveness, Reproductive.investment.threshold, h2) %>% 
  reshape2::melt(id.vars = "h2")  %>% 
  ggplot(aes(x = value, fill = paste0(variable, h2)))+
  geom_rect(xmin = .2, xmax = .3, ymin = -Inf, ymax = Inf, fill = "gray90")+
  geom_text(x = .25, y = 30, label = "Average range of h²\nin empirical studies ")+
  xlim(c(0,1))+
  geom_histogram(bins = 100)+
  theme_clean()+
    scale_fill_brewer("", type = "qual")+
    theme(legend.position = "top")+
    xlab("Heritability (h²)")+
    ylab("Count")+
    theme(panel.grid.major.y  = element_blank(), legend.box = "vertical", legend.direction = "vertical", legend.background = element_blank()))


ggsave(here("figs", sim.date, "Supplemental", "S1_A_Heritability.png"), cors, width = 12, height = 10, units = "cm") 
