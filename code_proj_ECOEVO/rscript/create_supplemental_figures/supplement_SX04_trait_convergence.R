library(tidyverse)
library(here)
library(foreach)
library(doSNOW)
library(parallel)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(shiny)
library(raster)

#READ STEP 2 ANIMAL FILES AND METADATA

if(!"sim.date" %in% ls()){
  sim.date = "2020-08-20"
} 

sim.path <- here("simulations", sim.date, "S1_Saturation", "processed")

animal.files <- list.files(sim.path, pattern = "animals.csv", full.names = T)
metadata.files <- list.files(sim.path, pattern = "metadata.csv", full.names = T)

substr(basename(animal.files), 1, 25) == substr(basename(metadata.files), 1, 25)


list.sims <- lapply(as.list(animal.files), function(x){
  
  df <- data.frame(readr::read_csv(x), ID = substr(basename(x), 1,30))
  
  df %>% 
    group_by(who) %>%
    mutate(death.tick = ifelse(length(ticks) == 2, max(ticks), 100000)) %>%
    mutate(birth.tick = min(ticks))
})

list.metadata <- lapply(as.list(metadata.files), function(x) data.frame(readr::read_csv(x), ID = substr(basename(x), 1,30)))

# STACK SIMULATIONS
df.sims <- do.call(rbind, list.sims)
df.metadata <- do.call(rbind, list.metadata)

# MERGE ANIMAL FILES AND METADATA
df.merged <- merge(df.sims, df.metadata, by = "ID")

df.density <- df.merged %>% 
  group_by(ID) %>% 
  filter(!duplicated(who))

###########################
### SUPPLEMENTAL FIGURE ###
###########################

SX4A_data <- df.density %>%
  group_by(ID) %>% 
  filter(!duplicated(ticks))

# Y = POPULATION DENSITY
options(scipen = 10)

(SX4_A <- 
  ggplot(SX4A_data, aes(x = ticks, y = ninds.x / (SX4A_data$ninds.y * 10), group = ID, color = ID)) +
  geom_line()+
  facet_wrap(~growthtype)+
  theme_clean()+
  xlab("Time")+
  ylab("Population density [n / patch]")+
  theme(legend.position = "none"))+
  scale_x_continuous(breaks = c(0, 50000, 100000))


### raster 
df.ras <- df.density[,-match("ID", names(df.density))]

r <- raster(ncols = 100, nrows = 100, xmn = 0, xmx = 2, ymn = 0, ymx = 1)

rdat_lin <- df.ras %>% 
  filter(growthtype == "linear") %>% 
  dplyr::select(BT, LH)

rdat_log <- df.ras %>% 
  filter(growthtype == "logistic") %>% 
  dplyr::select(BT, LH)


r_lin <- raster::rasterize(rdat_lin, r, fun = sum)
r_log <- raster::rasterize(rdat_log, r, fun = sum)


r_lin <- r_lin / cellStats(r_lin, sum)
r_log <- r_log / cellStats(r_log, sum)


r_lin[is.na(r_lin)]<- cellStats(r_lin, min)
r_log[is.na(r_log)]<- cellStats(r_log, min)


gg_df <- reshape2::melt(setNames(data.frame(rasterToPoints(stack(r_lin, r_log))), c("x", "y", "linear", "logistic")),id.vars = c("x", "y"))


(SX4_B <- 
  ggplot(gg_df)+
  geom_raster(aes(x = x, y = y, fill = (100 * value)))+
  facet_wrap(~variable)+
  scale_fill_viridis_c("Proportion of\npopulation [%]", na.value = "black")+
  ylab("Life-history trait")+
  xlab("Behavioural trait")+
  theme_clean()+
  theme(panel.grid.major = element_blank()))
  

SX4 <- gridExtra::grid.arrange(SX4_A, SX4_B, nrow = 2)


ggsave(here("figs", sim.date, "supplemental", "Fig_SX04.png"), plot = SX4, width = 15, height = 11, units = "cm")


################################################################
#### SHINY APP TO ANALYZE CHANGE IN DISTRIBUTION OVER TIME #####
################################################################

if(F){
  input = data.frame(time = 50000)
  
  shinyApp(
    shinyUI(fluidPage(
      fluidRow(
        column(6, sliderInput("time", "Time:", min = 0, max = 100000, step = 5000, value = 50000))),
      fluidRow(
        column(6, plotOutput("density"))
      )
    )
    ), 
    shinyServer(function(input, output){
      output$density <-  renderPlot({
        df.temp <- df.density %>% 
          filter(birth.tick < input$time & death.tick > input$time)
        summary(df.temp)
        
        
        MASS::fitdistr(df.temp$BT, "normal")[1]
        df.temp <- df.temp %>% 
          group_by(growthtype) %>% 
          mutate(BT_mean = round(MASS::fitdistr(BT, "normal")$estimate[1], 3)) %>% 
          mutate(BT_sd = round(MASS::fitdistr(BT, "normal")$estimate[2], 3)) %>% 
          mutate(LH_mean = round(MASS::fitdistr(LH, "normal")$estimate[1], 3)) %>% 
          mutate(LH_sd = round(MASS::fitdistr(LH, "normal")$estimate[2], 3))
        
        
        BT_plot <- 
          ggplot(df.temp, aes(x = BT)) +
          geom_density()+
          facet_wrap(~growthtype)+
          theme_clean()+
          geom_text(aes(label = paste0("mean: ", BT_mean, "\nsd: ", BT_sd)), x = .5, y = 2)+
          xlab("Behavioural trait")
        
        
        LH_plot <- 
          ggplot(df.temp, aes(x = LH)) +
          geom_density()+
          facet_wrap(~growthtype)+
          theme_clean()+
          geom_text(aes(label = paste0("mean: ", LH_mean, "\nsd: ", LH_sd)), x = .5, y = 2)+
          xlab("Physiological trait")
        
        
        gridExtra::grid.arrange(
          BT_plot, LH_plot
        )
        
      }
      
      )
    })
  )
  
}

