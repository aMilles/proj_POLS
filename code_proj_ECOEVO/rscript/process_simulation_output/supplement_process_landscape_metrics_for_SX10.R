# This script calculates landscape metrics (standard deviation and mean of harvest rates based on the raw NetLogo output)
# Data is stored for later analysis

library(here)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(raster)
library(fields)

# read data on dynamics in harvest rate and population density
if(!"landscape_file" %in% ls()){
  sim.date = "2020-08-20"
  sim.type = "test"
  landscape_file <- here("simulations", sim.date, sim.type)
} 

landscape <- lapply(as.list(list.files(landscape_file, pattern = "_ls.csv", full.names = T)),function(x) data.table::data.table(read_csv(x, col_names = F), ID = substring(basename(x), 1, 25)))


# calculate mean harvest rate and standard deviation
landscape_agg <- lapply(landscape, function(x){
  
  names(x) <- c("ticks", "x", "y", "hr", "Population density", "ID")
  
  
  x_agg <- 
    x %>% 
    group_by(ticks) %>% 
    mutate(mean_hr = mean(hr)) %>%
    mutate(sd_hr = sd(hr)) %>% 
    filter(!duplicated(ticks)) 
  return(x_agg)
})

ls<- do.call(rbind, landscape_agg)

metadata <- lapply(as.list(list.files(landscape_file, pattern = "_metadata.csv", full.names = T)),function(x) data.table::data.table(read_csv(x, col_names = T), ID = substr(basename(x), 1, 25)))

ls_meta <- do.call(rbind, metadata)

#generate output with landscape metrics (sd, mean) to process in supplement_SX10_landscape_dynamics_differentparameterizations.R

merged_ls <- merge(ls_meta, ls, by = "ID")
table(merged_ls$`harvest-rate-curvature`)
write_csv(here("simulations", sim.date, sim.type, "ls_merged_agg.csv"), x = merged_ls)
