library(tidyverse)
library(ggthemes)
library(here)


if(!"sim.date" %in% ls()){
  sim.date = "2020-08-20"
} 

##################################################
############# Fig 1 - Graphical ODD ##############
##################################################

### CREATED IN POWERPOINT

##################################################
### Fig 2 - Behavioural trait / responsiveness ###
##################################################

qs <- seq(-1, 2, length.out = 100)

BT.14 <- pgamma(qs, shape = (2- 1.4)^2, scale = (2 - 1.4)^2)
BT.11 <- pgamma(qs, shape = (2 - 1.1)^2, scale = (2 - 1.1)^2)

df <- data.frame(BT = rep(c(1.4, 1.1), each = 100), prob = c(BT.14, BT.11), qs = qs)


(gg_BT <- 
    ggplot(df, aes(y = prob, x = qs, group = BT, color = as.factor(BT)))+
    geom_line(size = 1.2)+
    theme_clean()+
    scale_color_brewer("Behavioural trait (BT)", type = "qual")+
    xlab("Perceived difference in harvest rate")+
    ylab("Likelihood to move\nto neighbouring patch [%]")+
    theme(legend.position = "bottom", legend.title = element_text(size = 9), text =element_text(size = 8), legend.text = element_text(size = 8)))


ggsave(here("figs", sim.date, "ODD", "Fig2.png"), gg_BT, width = 8, height = 8, unit = "cm")

##################################################
######### Fig 3 - Life-history-trait / ###########
####### Relative investment to reproduction ######
##################################################

soma <- seq(0, 10, length.out = 100)

LH.2 <- soma * .3^3
LH.5 <- soma * .5^3

df <- data.frame(LH = rep(c(.3, .5), each = 100), prob = c(LH.2, LH.5), qs = soma)


(gg_LH <- 
    ggplot(df, aes(y = prob, x = qs, group = LH, color = as.factor(LH)))+
    geom_line(size = 1.2)+
    theme_clean()+
    scale_color_brewer("Behavioural trait (LH)", type = "qual")+
    xlab("Resources available in soma")+
    ylab("Rate of investment to reproduction")+
    theme(legend.position = "bottom", legend.title = element_text(size = 9), text =element_text(size = 8), legend.text = element_text(size = 8)))


ggsave(here("figs", sim.date, "ODD", "Fig3.png"), gg_LH, width = 8, height = 8, unit = "cm")


##################################################
########### Fig 4 - Growth function  #############
##################################################


growth_function <- 
  function(RD_, growth_factor_logistic = 3, growth_type = "logistic", growthrate_linear = .1, growth_limit_linear = 15){
    if(growth_type == "logistic"){
      RD2 = RD_ - growth_factor_logistic
      SIGMA = 1 / (1 + 2.72 ^ (- 1*RD2))
      GR = SIGMA * (1- SIGMA)
    }else{
      GR = ifelse(RD_ < growth_limit_linear, growth_rate_linear, 0)
    }
    return(GR)
  }




RD = 0
for(i in seq(100)) RD <- append(RD, growth_function(sum(RD)))

df <- data.frame("t" = 0:100, "Growth rate" = RD, "Resource density" = cumsum(RD)) %>%
  filter(t > 0) %>% 
  reshape2::melt(id.vars = "t") %>% 
  mutate(variable = stringi::stri_replace_all_regex(variable, pattern = "\\.", replacement = " "))

(Fig4 <- 
ggplot(df, aes(x = t, y = value))+
  geom_line()+
  facet_wrap(~variable, scales = "free")+
  theme_clean()+
  xlab("Time steps")+
  ylab("Value"))

ggsave(here("figs", sim.date, "ODD", "Fig4.png"), Fig4, width = 12, height = 8, unit = "cm")


##################################################
############# Fig 5 - Harvest rate ###############
##################################################


harvest_function <- 
  function(RD_, harvest_rate_factor = 5, harvest_rate_curvature = 1.5){
    HR = RD_ ^ harvest_rate_curvature / harvest_rate_factor^harvest_rate_curvature
    return(HR)
  }

harvest_rate <- data.frame("hr" = harvest_function(cumsum(RD)), "RD" = cumsum(RD))

(Fig5 <-
ggplot(harvest_rate, aes(x = RD, y = hr))+
  geom_line()+
  xlab("Resource density")+
  ylab("Harvest rate")+
  theme_clean())

ggsave(here("figs", sim.date, "ODD", "Fig5.png"), Fig5, width = 8, height = 8, unit = "cm")

