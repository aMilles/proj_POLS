library(shiny)
library(tidyverse)
library(here)
library(ggthemes)
library(cowplot)
library(ggplot2)
library(GGally)
# read csv file with aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  
  out.path <- here("simulations", sim.date,"Main_Predictions",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

# create dataset for correlogram

SX12_data <- read_csv(out.path) %>% 
  dplyr::select("medianBT", "medianLH", "longevity", "em_rate", "generation_time", "movement_activity", "repo_activity", "n_offspring", "r_buffer", "soma") %>% 
  mutate(fe = (r_buffer + n_offspring * 50 + soma) / longevity) %>% 
  dplyr::select("medianBT", "medianLH", "longevity", "em_rate", "generation_time", "movement_activity", "repo_activity", "fe")


SX12_data <- setNames(SX12_data, c("BT", "LH", "life span", "EM rate", "GT", "MA", "RI", "FE"))



GGscatterPlot <- function(data, mapping, ..., 
                          method = "spearman") {
  
  #Get correlation coefficient
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  cor <- cor(x, y, method = method)
  #Assemble data frame
  df <- data.frame(x = x, y = y)
  # PCA
  nonNull <- x!=0 & y!=0
  dfpc <- prcomp(~x+y, df[nonNull,])
  df$cols <- predict(dfpc, df)[,1]
  # Define the direction of color range based on PC1 orientation:
  dfsum <- x+y
  colDirection <- ifelse(dfsum[which.max(df$cols)] < 
                           dfsum[which.min(df$cols)],
                         1,
                         -1)
  #Get 2D density for alpha
  dens2D <- MASS::kde2d(df$x, df$y)
  df$density <- fields::interp.surface(dens2D , 
                                       df[,c("x", "y")])
  
  if (any(df$density==0)) {
    mini2D = min(df$density[df$density!=0]) #smallest non zero value
    df$density[df$density==0] <- mini2D
  }
  #Prepare plot
  pp <- ggplot(df, aes(x=x, y=y, color = cols, alpha = 1/density)) +
    ggplot2::geom_point(shape=16, show.legend = FALSE) +
    ggplot2::scale_color_viridis_c(direction = colDirection) +
    #                scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    ggplot2::scale_alpha(range = c(.05, .6)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col="darkred") +
    ggplot2::geom_label(
      data = data.frame(
        xlabel = min(x, na.rm = TRUE),
        ylabel = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)),
      mapping = ggplot2::aes(x = xlabel, 
                             y = ylabel, 
                             label = lab),
      hjust = 0, vjust = 1,
      size = 3, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    ) +
    theme_minimal()
  
  return(pp)
}

#create correlogram and save it

(FigSX12 <- GGally::ggpairs(SX12_data, upper = list(continuous = wrap("cor", size = 3)), lower = list(continuous = wrap("smooth", size = .1, color = "gray")))+theme_clean()+theme(axis.text.x =  element_text(size = 6, angle = 45), axis.text.y = element_text(size = 6), panel.grid.major.y = element_blank(), panel.grid = element_blank(), panel.grid.minor.y = element_blank()))
  

ggsave(here("figs", sim.date, "supplemental", "Fig_SX12.jpeg"), FigSX12, width = 16, height = 10, dpi = 600, units = "cm")
