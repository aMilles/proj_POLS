library(here)
library(tidyverse)
library(ggthemes)

# define date and experiment name


# use landscape data output from "Supplement_altered_growth_factor_logistic_landscape"-experiment


if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  out.path <- here("simulations", sim.date,"Supplement_altered_movementassumption_landscape", "ls_merged_agg.csv")
  
}
ls <- read_csv(out.path)

ls$var <- NA

# identify default and changed parameterization

for(cols in c(2, 6:14)){
  different_par_row <- which(as.character(as.vector(as.matrix(ls[,cols]))) != names(sort(table(ls[,cols]), decreasing = T))[1])
  par_name <- names(ls)[cols]
  ls$var[different_par_row] <- apply(cbind(rep(par_name, length(different_par_row)), ls[different_par_row, par_name]), 1, paste, collapse = ": ")
}
table(ls$`maintenance-cost`)
ls$var <- paste0("Maintenance-cost: ", ls$`maintenance-cost`, " Growth-rate-linear: ", ls$`resource-growth-rate-linear`)
ls$var[ls$`maintenance-cost` == 0.15 & ls$`resource-growth-rate-linear` == 0.15] <- "default"
unique(ls$var)


# plot variation in harvest rates (mean and coefficient of variation)


(Fig_SX10a <-
    ggplot(ls %>% filter(ticks>5000), aes(color = var, x = `Population density`/2500, y = mean_hr))+
    geom_point(size = .1, alpha = .1)+
    geom_smooth(se = F)+
    scale_color_manual("Parameter",values = c("gray", RColorBrewer::brewer.pal(3, "Set2")))+
    facet_wrap(~paste("Disturbance frequency: ", `disturbance-interval`), ncol = 2)+
    scale_y_continuous(breaks = c(.15, .5, 1))+
    theme_clean()+
    scale_y_log10()+
    xlab("Population density [indiviudals/patch]")+
    ylab("Mean harvest\nrate [resources/t]")+
    theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_text(size = 10), legend.text = element_text(size = 9), panel.grid.major.y = element_blank())+
    facet_wrap(~`disturbance-interval`))

(Fig_SX10b <-
ggplot(ls, aes(color = var, x = `Population density`/2500, y = sd_hr/mean_hr))+
  geom_point(size = .1, alpha = .1)+
  geom_smooth(se = F)+
    scale_color_manual("Parameter",values = c("gray", RColorBrewer::brewer.pal(3, "Set2")))+
  facet_wrap(~paste("Disturbance interval: ",`disturbance-interval`), ncol = 2)+
  theme_clean()+
    xlab("Population density [individuals/patch]")+
    ylab("Coefficient of variation\nof harvest rates")+
  scale_y_log10()+
      theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_text(size = 10), legend.text = element_text(size = 9), panel.grid.major.y = element_blank()))


# arrange figures in a grid
Fig_SX10 <- 
gridExtra::grid.arrange(Fig_SX10b+theme(legend.position = "none", panel.border = element_blank(), plot.background = element_blank()), 
                        Fig_SX10a + theme(strip.text.x = element_blank(), strip.background = element_blank(), panel.background = element_blank(), panel.border = element_blank(), plot.background = element_blank()), layout_matrix = matrix(c(1,1,2,2,2), nc = 1))
         
# save figure
ggsave(here("Figs", sim.date, "Supplemental", "S1_N_LandscapeDynamics_RelaxedAssumption.jpeg"), Fig_SX10, width = 14, height = 17, units = "cm", dpi = 600)
