library(here)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(raster)
library(fields)

# read data on dynamics in harvest rate and population density
if(!"landscape_file" %in% ls()){
  sim.type =  "MainText_Landscape_fluctuation_Example"
  
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  
  landscape_file <- here("simulations", sim.date, sim.type)
} 

# function to calculate coefficient of variation
coef_var <- function(x) sd(x)/mean(x) 

# read landscape data
landscape <- lapply(as.list(list.files(landscape_file, pattern = "_ls.csv", full.names = T)),function(x) cbind(read_csv(x, col_names = F), basename(x)))
landscape <- do.call(rbind, landscape)
# convert population size to population density (62500 patches)
landscape[,5] <- landscape[,5] / 2500
names(landscape) <- c("ticks", "x", "y", "hr", "Population density", "ID")

# calculate harvest rate and CI for each landscape 
landscape_proc <- landscape %>%
  group_by(ticks, ID) %>% 
  mutate("mhr" = median(hr)) %>%
  mutate("uhr" = quantile(hr, 0.95)) %>%
  mutate("lhr" = quantile(hr, 0.05)) %>% 
  mutate("coefvar" = sd(hr)/mean(hr)) %>% 
  filter(!duplicated(ticks))

landscape_proc <- data.frame(landscape_proc)


#################
### FIGURE 2A ###
#################

Fig2A_data <- landscape_proc %>% 
  filter(ID == ID[length(ID)]) %>% 
  filter(ticks > 5000)

# LANDSCAPE ELEMENT
# Select landscape with minimum and maxium median harvest rate

min_hr.ls <- 
  landscape %>%
  filter(ID == Fig2A_data$ID[1]) %>% 
  filter(ticks == Fig2A_data$ticks[which.min(Fig2A_data$mhr)])

max_hr.ls <- 
  landscape %>% 
  filter(ID == Fig2A_data$ID[1]) %>%
  filter(ticks == Fig2A_data$ticks[which.max(Fig2A_data$mhr)])



# generate small images of landscape with maximum and minimum harvest rate
gg_max_hr <- 
  ggplotGrob(
    ggplot(max_hr.ls, aes(x = x, y = y, fill = hr))+
      geom_raster(interpolate = T)+
      theme_void()+
      scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(min(min_hr.ls$hr), max(max_hr.ls$hr)))+
      theme(legend.position = "none", panel.background = element_rect(fill = "white", color = "gray20"))+
      coord_fixed())

gg_min_hr <- 
  ggplotGrob(
    ggplot(min_hr.ls, aes(x = x, y = y, fill = hr))+
      geom_raster()+
      theme_void()+
      scale_fill_gradient2(low = "white", high = "darkgreen")+
      theme(legend.position = "none", panel.background = element_rect(fill = "white", color = "black"))+
      coord_fixed()
  )


# data.frame for harvest rate plot
landscape_proc <- landscape_proc[, -match(c("x", "y", "hr"), names(landscape_proc))]

### FULL PLOT

(Fig2A <- 
    ggplot(Fig2A_data, aes(x = ticks))+
    geom_vline(aes(xintercept = min_hr.ls$ticks[1]), color = "gray50", linetype = "dashed")+
    geom_vline(aes(xintercept = max_hr.ls$ticks[1]), color = "gray50", linetype = "dashed")+
    geom_ribbon(aes(ymin = lhr, ymax = uhr), fill = "gray90")+
    geom_line(aes(y = mhr))+
    theme_clean()+ 
    ylab("Median harvest rate [n/t]")+
    xlab("Time")+
    scale_x_continuous(breaks = c(5000, 10000, 15000, 20000), limits = c(5000, 20000), labels = c("5,000", "10,000", "15,000", "20,000"))+
    scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2.2))+
    annotation_custom(grob = gg_min_hr, xmin = min_hr.ls$ticks[1] - 2000, xmax = min_hr.ls$ticks[1] + 2000, ymin = 1.4, ymax = 2.1)+
    annotation_custom(grob = gg_max_hr, xmin = max_hr.ls$ticks[1] - 2000, xmax = max_hr.ls$ticks[1] + 2000, ymin = 1.4, ymax = 2.1)+
    geom_point(aes(x = min_hr.ls$ticks[1], y = Fig2A_data$mhr[Fig2A_data$ticks == min_hr.ls$ticks[1]]), shape = 23, fill = "gray", size = 2)+
    geom_point(aes(x = max_hr.ls$ticks[1], y = Fig2A_data$mhr[Fig2A_data$ticks == max_hr.ls$ticks[1]]), shape = 23, fill = "gray", size = 2)+
    theme(panel.border = element_blank(), plot.background = element_blank(), panel.grid = element_blank(), panel.grid.major = element_blank(), panel.grid.major.y = element_blank()))

#################
### FIGURE 2B ###
#################

### GET POPULATION DENSITY FROM DATA WITHOUT DISTURBANCE
#READ ANIMAL FILES AND METADATA THAT HAVE BEEN PROCESSED BY PROCESS_SIMULATION_DATA_STEP1.R
sim.path <- here("simulations", sim.date,  "Supplement_Saturation", "processed")

animal.files <- list.files(sim.path, pattern = "animals.csv", full.names = T)
metadata.files <- list.files(sim.path, pattern = "metadata.csv", full.names = T)

exmp_file <- animal.files[which.max(file.info(animal.files)$size)]
exmp_data_animal <- data.frame(read_csv(exmp_file), ID = substr(basename(exmp_file), 1,30))

# REMOVE DUPLICATE INDIVIDUALS AND TICKS SO ONLY ONE MEASURE OF POPULATION DENSITY PER TIME STEP IS RETAINED
df.density_no_disturbance <- exmp_data_animal %>%
  filter(!duplicated(who)) %>% 
  filter(!duplicated(ticks))
df.density_no_disturbance <- 
  df.density_no_disturbance %>% 
  filter(ticks > 5000) %>% 
  mutate(coef_var_tot = coef_var(ninds))

# calculate deciles of population density for plot (note: weighted deciles were calculated here to account for the size of each group.)
ys <- c(min(Fig2A_data$Population.density), reldist::wtd.quantile(Fig2A_data$`Population.density`, seq(0.1, .9, length.out = 9),weight =  round(Fig2A_data$Population.density * 2500)), max(Fig2A_data$Population.density))

# plot fluctuation in population density with grey-white stripes as deciles, indicate time of lowest and highest harvest rate with vertical dashed lines
(Fig2B <-
    ggplot()+
    geom_rect(data = data.frame(), aes(xmin = 19999,  xmax = Inf,  ymin = ys[-11], ymax = ys[-1]), fill = rep(c("gray90", "white"), 5), color = "black")+
    geom_vline(xintercept = 19999, color = "white")+
    
    geom_line(data = df.density_no_disturbance, aes(x = ticks, y = ninds/2500,
                                                    color = as.character(round(coef_var(df.density_no_disturbance$ninds), 2)), 
                                                    linetype = as.character(round(coef_var(df.density_no_disturbance$ninds), 2))
                                                    ))+
    
    geom_line(data = Fig2A_data, aes(x = ticks, y = Population.density, 
                                     color = as.character(round(coef_var(landscape_proc[landscape_proc$ID == Fig2A_data$ID[1],]$Population.density), 2)), 
                                     linetype = as.character(round(coef_var(landscape_proc[landscape_proc$ID == Fig2A_data$ID[1],]$Population.density), 2))
                                     ))+
    
    geom_line(data = landscape_proc[landscape_proc$ID != Fig2A_data$ID[1],], aes(x = ticks, y = Population.density, group = ID,
                                                                                 color = as.character(round(coef_var(landscape_proc[landscape_proc$ID != Fig2A_data$ID[1],]$Population.density), 2)), 
                                                                                 linetype = as.character(round(coef_var(landscape_proc[landscape_proc$ID != Fig2A_data$ID[1],]$Population.density), 2))
                                                                                 ))+
    theme_clean()+
    scale_color_manual("Coefficient\nof variation", values = c("turquoise", "coral1", "black"))+
    scale_linetype_manual("Coefficient\nof variation", values = c("dotted", "dashed", "solid"))+
    ylab("Population density [n/patch]")+
    geom_point(aes(x = min_hr.ls$ticks[1], y = Fig2A_data$Population.density[Fig2A_data$ticks == min_hr.ls$ticks[1]]), shape = 23, fill = "gray", size = 2)+
    geom_point(aes(x = max_hr.ls$ticks[1], y = Fig2A_data$Population.density[Fig2A_data$ticks == max_hr.ls$ticks[1]]), shape = 23, fill = "gray", size = 2)+
    scale_x_continuous("Time", breaks = c(5000, 10000, 15000, 20000), limits = c(5000, 20000), labels = c("5,000", "10,000", "15,000", "20,000"))+
    scale_y_continuous("Population density", breaks = c(0, 0.5, 1), limits = c(0, 1))+
    theme(panel.grid = element_blank(), panel.grid.major.y = element_blank(), panel.border = element_blank(), plot.background = element_blank(), legend.position = "bottom", legend.title = element_text(size = 9), legend.text = element_text(size = 9)))

################
### FIGURE 2 ###
################

Fig2 <- gridExtra::grid.arrange(ggdraw(Fig2A)+draw_plot_label("A"), ggdraw(Fig2B)+draw_plot_label("B"), ncol = 1)

ggsave(here("figs", sim.date, "main_text", "Fig2.jpeg"), Fig2, width = 10, height = 14, units = "cm")

