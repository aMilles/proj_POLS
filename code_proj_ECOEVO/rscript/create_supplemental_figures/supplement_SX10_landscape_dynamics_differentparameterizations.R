library(here)
library(tidyverse)
library(ggthemes)

sim.date <- "2020-08-20"
sim.type <- "test"

ls <- read_csv(here("simulations", sim.date, sim.type, "ls_merged_agg.csv"))


names(ls)
ls$var <- NA

# identify default and changed parameterization

for(cols in c(2, 6:14)){
  different_par_row <- which(as.character(as.vector(as.matrix(ls[,cols]))) != names(sort(table(ls[,cols]), decreasing = T))[1])
  par_name <- names(ls)[cols]
  ls$var[different_par_row] <- apply(cbind(rep(par_name, length(different_par_row)), ls[different_par_row, par_name]), 1, paste, collapse = ": ")
}

ls$var[is.na(ls$var)] <- "default"


# plot variation in harvest rates (mean and coefficient of variation)

ls <- ls %>%  filter(`growth-factor-logistic` == 1.75 | `growth-factor-logistic` == 3 & `harvest-rate-curvature` == 1.5)

(Fig_SX10a <-
    ggplot(ls %>% filter(ticks>5000), aes(color = var, x = `Population density`/62500, y = mean_hr))+
    geom_point(size = .1, alpha = 1)+
    geom_smooth(se = F)+
    scale_color_manual("Parameter",values = c("gray", 
                                              RColorBrewer::brewer.pal(4, "Dark2")))+
    facet_wrap(~paste("Disturbance frequency: ", disturbancefrequency), ncol = 2)+
    theme_clean()+
    scale_y_log10()+
    geom_hline(data = data.frame(disturbancefrequency = c(100, 400), ic = c(NA, 0.25)), aes(yintercept = ic), linetype = "dashed")+
    xlab("Population density")+
    ylab("Mean\nharvest rate")+
    theme(legend.position = "bottom")+
    facet_wrap(~disturbancefrequency))

(Fig_SX10b <-
ggplot(ls, aes(color = var, x = `Population density`/62500, y = sd_hr/mean_hr))+
  geom_point(size = .1, alpha = .1)+
  geom_smooth(se = F)+
  scale_color_manual("Parameter",values = c("gray", 
                                            RColorBrewer::brewer.pal(4, "Dark2")))+
  facet_wrap(~paste("Disturbance frequency: ", disturbancefrequency), ncol = 2)+
  theme_clean()+
    xlab("Population density")+
    ylab("Coefficient of variation\nof harvest rates")+
  scale_y_log10()+
      theme(legend.position = "bottom")+
      geom_vline(data = data.frame(disturbancefrequency = c(100, 400), ic  =c(NA, .9)),  aes(xintercept = ic), linetype = "dashed"))



Fig_SX10 <- 
gridExtra::grid.arrange(Fig_SX10b+theme(legend.position = "none", panel.border = element_blank(), plot.background = element_blank()), Fig_SX10a + theme(strip.text.x = element_blank(), strip.background = element_blank(), panel.background = element_blank(), panel.border = element_blank(), plot.background = element_blank()))
         

ggsave(here("Figs", sim.date, "Supplemental", "Fig_SX10.png"), Fig_SX10, width = 17, height = 12, units = "cm")
