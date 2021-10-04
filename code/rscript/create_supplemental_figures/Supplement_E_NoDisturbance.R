library(here)
library(ggthemes)
library(raster)
library(tidyverse)
.packages()
#READ STEP 2 ANIMAL FILES AND METADATA

if(!"sim.date" %in% ls()){
  sim.date = "2021-03-28"
  
} 



# read data that has been processed by process_simulation_data_step1.R
sim.path <- here("simulations", sim.date, "Supplement_Saturation", "processed")
animal.files <- list.files(sim.path, pattern = "animals.csv", full.names = T)
metadata.files <- list.files(sim.path, pattern = "metadata.csv", full.names = T)

# store single simulation runs as elements of a list 
list.sims <- lapply(as.list(animal.files), function(x){
  
  df <- data.frame(readr::read_csv(x), ID = substr(basename(x), 11,35))
  
  df %>% 
    group_by(who) %>%
    mutate(death.tick = ifelse(length(ticks) == 2, max(ticks), 100000)) %>%
    mutate(birth.tick = min(ticks))
})

list.metadata <- lapply(as.list(metadata.files), function(x) data.frame(readr::read_csv(x), ID = substr(basename(x), 1,25)))

# STACK SIMULATIONS
df.sims <- do.call(rbind, list.sims)
df.metadata <- do.call(rbind, list.metadata)

# MERGE ANIMAL FILES AND METADATA
df.merged <- merge(df.sims, df.metadata, by = "ID")

df.density <- df.merged
###########################
### SUPPLEMENTAL FIGURE ###
###########################

SX4A_data <- df.density %>%
  group_by(ID) %>% 
  filter(!duplicated(ticks))

# avoid scientific abbrevation of numbers
options(scipen = 10)

# generate plot element A
(SX4_A <- 
  ggplot(SX4A_data, aes(x = ticks, y = ninds.x / 2500, group = ID, color = ID)) +
  geom_line()+
  theme_clean()+
  xlab("Time")+
  ylab("Population density [n / patch]")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(0, 50000, 100000)))
  
### rasterize the abundance of LH and BT at linear growth
df.ras_reproducing <- df.density %>% 
  group_by(ID) %>% 
  filter(generation_time > 0) %>%
  filter(birth.tick > 50000) %>% 
  filter(!duplicated(who)) %>% 
  ungroup() %>% 
  dplyr::select(-ID)

r <- raster(ncols = 25, nrows = 25, xmn = 0, xmx = 2, ymn = 0, ymx = 2)

rdat_lin <- df.ras_reproducing %>% 
  dplyr::select(LH, BT)


# create two rasters which show the abundance of a certain combination of BT and LH as a proportion of all combinations 
r_lin <- raster::rasterize(rdat_lin, r, fun = sum)

r_lin <- r_lin / cellStats(r_lin, sum)

r_lin[is.na(r_lin)]<- cellStats(r_lin, min)

# convert the raster-data to data format that is readable by ggplot
gg_df <- reshape2::melt(setNames(data.frame(rasterToPoints(stack(r_lin))), c("x", "y", "linear")),id.vars = c("x", "y"))

# create plot element B
(SX4_B <- 
  ggplot(gg_df)+
  geom_raster(aes(x = x, y = y, fill = (100 * value)))+
  scale_fill_viridis_c("Proportion of\nreproducing\npopulation [%]", na.value = "white", values  = c(0, .5,.75, 1), direction = 1, option = "A")+
  xlab("Reproductive investment threshold")+
  ylab("Responsiveness")+
  theme_clean()+
  theme(panel.grid.major = element_blank(), legend.text = element_text(size = 9), legend.title = element_text(size = 9), legend.key.height = unit(.3, "cm")))

SX4_C_data <-
df.merged %>% 
  filter(generation_time > 0) %>% 
  group_by(round(ticks / 5000), ID) %>%
  mutate(meanLH = mean(LH)) %>% 
  mutate(meanBT = mean(BT)) %>% 
  mutate(sdBT = diff(range(quantile(BT, c(0.025, 0.75))))) %>% 
  mutate(sdLH = diff(range(quantile(BT, c(0.025, 0.75))))) %>% 
  mutate(ticks = mean(ticks)) %>% 
  filter(!duplicated(paste0(round(ticks/5000))))


# arrange the plots
SX4 <- gridExtra::grid.arrange(SX4_A, SX4_B, nrow = 2)

# save the plots
ggsave(here("figs", sim.date, "supplemental", "S1_E_NoDisturbance.png"), plot = SX4, width = 9, height = 11, units = "cm", dpi = 600)
ggsave(here("figs", sim.date, "supplemental", "S1_E_NoDisturbance.pdf"), plot = SX4, width = 9, height = 11, units = "cm", dpi = 600)


#detach raster-package to avoid namespace conflicts
detach("package:raster", unload=TRUE)
