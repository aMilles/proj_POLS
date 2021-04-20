library(tidyverse)
library(ggthemes)
library(here)


if(!"sim.date" %in% ls()){
  sim.date = "2021-03-28"
} 

##################################################
############# Fig 1 - Graphical ODD ##############
##################################################

### CREATED IN POWERPOINT

##################################################
### Fig 2 - Behavioural trait / responsiveness ###
##################################################

qs <- seq(-1, 2, length.out = 100)

BT.14 <- pgamma(qs, shape = (2- 1.4), scale = (2 - 1.4))
BT.11 <- pgamma(qs, shape = (2 - 1.1), scale = (2 - 1.1))

df <- data.frame(BT = rep(c(1.4, 1.1), each = 100), prob = c(BT.14, BT.11), qs = qs)


(gg_BT <- 
    ggplot(df, aes(y = 100  * prob, x = qs, group = BT, color = as.factor(BT)))+
    geom_line(size = 1.2)+
    theme_clean()+
    scale_color_brewer("Behavioural trait (BT)", type = "qual")+
    xlab("Perceived difference in harvest rate")+
    ylab("Likelihood to move\nto neighbouring patch [%]")+
    scale_y_continuous(breaks = c(0, 50, 100))+
    theme(legend.position = "bottom", legend.title = element_text(size = 9), text =element_text(size = 8), legend.text = element_text(size = 8)))


ggsave(here("figs", sim.date, "ODD", "Fig2.jpeg"), gg_BT, width = 8, height = 8, unit = "cm", dpi = 600)

##################################################
########### Fig 3- Growth function  #############
##################################################


growth_function <- function(RD, rate = .4, beta = 0.2, limit = 10){
  return(RD * rate*((1 - (RD/limit)^beta)))
}



RD = 0.01
for(i in seq(100)) RD <- append(RD, growth_function(sum(RD), rate = .4, beta = .2, limit = 15))

Richards <- data.frame("t" = 0:100, "Growth rate" = RD, "Resource density" = cumsum(RD)) %>%
  filter(t > 0) %>%  
  mutate(type = "Richards: resource-growth-beta-logistic = .2, resource-growth-limit = 15, resource-growth-rate-logistic = .4")
  


RD = 0.01
for(i in seq(100)) RD <- append(RD, growth_function(sum(RD), rate = .2, beta = 1, limit = 10))
Verhulst <- data.frame("t" = 0:100, "Growth rate" = RD, "Resource density" = cumsum(RD)) %>%
  filter(t > 0) %>%  
  mutate(type = "Verhulst: resource-growth-beta-logistic = 1, resource-growth-limit = 10, resource-growth-rate-logistic = .2")


df <- rbind(Verhulst, Richards)

(Fig3A <- 
    ggplot(df, aes(x = t, y = Resource.density, group = type, color = type))+
    geom_line()+
    theme_clean()+
    scale_color_discrete("Logistic growth equation:")+
    theme(panel.grid.major.y = element_blank(), legend.position = "bottom", legend.box = "vertical", legend.direction = "vertical", panel.background = element_blank(), panel.border = element_blank())+
    xlab("Time steps [t]")+
    ylab("Resource density [resources/patch]"))


(Fig3B <- 
    ggplot(df, aes(y = Growth.rate, x = Resource.density, group = type, color = type))+
    geom_line()+
    theme_clean()+
    scale_color_discrete("Logistic growth equation:")+
    theme(panel.grid.major.y = element_blank(), legend.position = "bottom", legend.box = "vertical", legend.direction = "vertical", panel.background = element_blank(), panel.border = element_blank())+
    ylab("Resource growth rate [resources / t]")+
    xlab("Resource density [resources/patch]"))

Fig3 <- gridExtra::grid.arrange(Fig3A+theme(legend.position = "none"), Fig3B+theme(legend.position = "none"), cowplot::get_legend(Fig3B+theme(legend.text = element_text(size = 8))), layout_matrix = matrix(c(1,1,1,3,2,2,2,3), nc = 2, nr = 4))

ggsave(here("figs", sim.date, "ODD", "Fig3.jpeg"), Fig3, width = 16, height = 10, unit = "cm", dpi = 600)


##################################################
############# Fig 4 - Harvest rate ###############
##################################################


harvest_function <- 
  function(RD_, encounter.rate = .3, handling.time = .7){
    HR = RD_ * encounter.rate / (1 + RD_ * encounter.rate * handling.time)
    return(HR)
  }

harvest_rate <- data.frame("hr" = harvest_function(seq(0,15,length.out = 100)), "RD" = seq(0,15,length.out = 100))

(Fig4A <-
ggplot(harvest_rate, aes(x = RD, y = hr))+
  geom_line(aes(color = "handling-time = 0.7, encounter-rate = 0.3"))+
  xlab("Resource density [resources/patch]")+
  scale_color_manual("Harvest function - parameters: ", values = "black")+
  ylab("Harvest rate [resources/t]")+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  theme_clean()+
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom"))

RD <- 10
HRs <- RDs <- vector(length = 100)
for(t in seq(100)){
  RD <- RD + growth_function(RD, rate = .2, beta = 1, limit = 10)
  HRs[t] <- harvest_function(RD)
  RD <- RD - harvest_function(RD)
  RDs[t] <- RD
}

Fig4B <- ggplot(data = data.frame(t = seq(100), hr = HRs), aes(x = t, y = hr))+
  geom_line()+
  theme_clean()+
  ylab("Harvest rate [resources/t]")+
  xlab("Time steps [t]")+
  theme(panel.grid.major.y = element_blank(), legend.position = "none")


Fig4 <- gridExtra::grid.arrange(Fig4A+theme(legend.position = "none"), Fig4B+theme(legend.position = "none"), cowplot::get_legend(Fig4A), layout_matrix = matrix(c(1,1,1,3,2,2,2,3), nc = 2, nr = 4))


ggsave(here("figs", sim.date, "ODD", "Fig4.jpeg"), Fig4, width = 16, height = 8, unit = "cm", dpi = 600)

