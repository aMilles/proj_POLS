library(tidyverse)
library(here)
library(ggthemes)
library(mgcv)
library(patchwork)

# read aggregated data

if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  
  out.path <- here("simulations", sim.date,"MainText_LowFreqHighIntensity",  "processed", "output_2ndStep_2ndfilter", "output_intermediate_stacked", "stacked_output_2ndStep_2ndfilter.csv")
}

# read dataset with all individuals
sims <- read_csv(out.path) %>% 
  mutate(pop_dens = pop_dens / 2500)


# sample a representative subset to speed up the computation
sims_subset <- sims[sample(seq(nrow(sims)), size = 100000), ]

# calculate GAMs of 4 levels of population density with fitness ~ trait
preds <- lapply(split(sims_subset, cut(sims_subset$pop_dens, 4)), function(x){
  pred.df <- data.frame(LH = seq(0,2, length.out = 100), BT = seq(.5, 2, length.out = 100), pop_dens = mean(x$pop_dens))
  x$fitness <- x$n_offspring / x$longevity
  fit_LH <- gam(fitness ~ s(LH, k = 4), data = x, family = "gaussian") 
  fit_BT <- gam(fitness ~ s(BT, k = 4), data = x, family = "gaussian")
  
  pred.df <- 
    pred.df %>% 
    mutate(pLH = predict.gam(fit_LH, newdata = pred_df, type = "response")) %>% 
    mutate(pBT = predict.gam(fit_BT, newdata = pred_df, type = "response")) %>%
    mutate(pBTrel = pBT / max(pBT)) %>% 
    mutate(pLHrel = pLH / max(pLH)) 
  return(pred.df)
})


# plot reproductive rate across gradients of density and traits
df <- do.call(rbind, preds)

FitnessLH <- ggplot(df, aes(x = LH, y = pLHrel, group = pop_dens, color = pop_dens, fill = pop_dens), alpha = .5)+
    geom_ribbon(aes(ymin = 0, ymax = pLHrel), alpha = .5)+
    theme_clean()+
    scale_fill_viridis_c("Population density [individuals per patch]", option = "A", end = .8, guide = guide_legend())+
    scale_color_viridis_c("Population density [individuals per patch]", option = "A", end = .8, guide = guide_legend())+
    theme(text = element_text(size = 8), 
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8), 
          legend.box = "vertical", 
          legend.key.height = unit(4, "mm"), 
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())+
    labs(x = "Reproductive investement threshold",
         y = "Relative number of\noffspring per time step") +
  ylim(0, 1)

FitnessBT <- ggplot(df, aes(x = BT, y = pBTrel, group = pop_dens, color = pop_dens, fill = pop_dens))+
    geom_ribbon(aes(ymin = 0, ymax = pBTrel), alpha = .5)+
    theme_clean()+
    scale_fill_viridis_c("Population density [individuals per patch]", option = "A", end = .8, guide = guide_legend())+
    scale_color_viridis_c("Population density [individuals per patch]", option = "A", end = .8, guide = guide_legend())+
    theme(text = element_text(size = 8), 
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8), 
          legend.box = "vertical", 
          legend.key.height = unit(4, "mm"), 
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())+
    labs(x = "Responsiveness",
         y = "Relative number of\noffspring per time step") +
    ylim(0, 1)


combined <- FitnessLH + FitnessBT & theme(legend.position = "bottom")
combined_final <- combined + plot_layout(guides = "collect")



ggsave(here::here("figs", sim.date, "supplemental", paste0("S1_Q_traitPopDensityReproduction", ".jpeg")), 
       combined_final, width = 16, height = 9, units = "cm", dpi = 600)

