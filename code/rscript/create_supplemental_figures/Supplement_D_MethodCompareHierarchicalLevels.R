library(tidyverse)
library(here)
library(ggthemes)


# read csv file with aggregated data
if(!"out.path" %in% ls()){
  
  if(!"sim.date" %in% ls()){
    sim.date = "2021-03-28"
  } 
  L3 = "MainText_LowFreqHighIntensity"
  out.path <- here("simulations", sim.date,L3,  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

if(!"df" %in% ls()) df <- read_csv(out.path)

# intrapopulation POL axes
df_inclination <-
  df %>% 
  group_by(sim.id) %>% 
  mutate(inclination_intrapop = coefficients(lm(medianBT ~ medianLH))[2]) %>% 
  mutate(popavg_LH = median(medianLH))
  
# interpopulation POL axis
fit <- lm(medianBT ~ medianLH + I(medianLH^2), data = df)


# first derivative of interpopulation POL axis
coefs <- coefficients(fit)
df_inclination$inclination_interpop <- coefs[2] + 2 * coefs[3] * df_inclination$popavg_LH
df_inclination$predicted_inclination <- fit$fitted.values

# show individual intrapopulation POL axes against interpopulation POL axes
  for(i in seq(1, 600, 10)){
    # calculate tangent of interpopulation POL axis at median reproductive threshold of a focal population
    
    slope_interpop <-  df_inclination$inclination_interpop[df_inclination$sim.id == df$sim.id[i]][1]
    x_pos <- (df_inclination %>% filter(sim.id == df$sim.id[i]) %>% ungroup() %>% dplyr::select(popavg_LH))[1,] %>% mutate(medianLH = popavg_LH) %>% dplyr::select(medianLH)
    y <- predict(fit,  x_pos)
    xs <- seq(x_pos$medianLH-.5, x_pos$medianLH+.5, length.out = 500)
    tangent <- y + slope_interpop * (xs - x_pos$medianLH)
    interpop_tangent <- data.frame(x = xs, y = tangent)
    
    gg <- ggplot(df, aes(x = medianLH, y = medianBT))+
      geom_point(color = "gray90")+
      geom_vline(data = df %>% filter(sim.id == sim.id[i]), aes(xintercept = median(medianLH)[1]), linetype = "dashed")+
      geom_point(data = df %>% filter(sim.id == sim.id[i]), color = "coral1", size = 3)+
      geom_line(data = interpop_tangent, aes(x = x, y = y), color = "gray50")+
      geom_smooth(data = df %>% filter(sim.id == sim.id[i]), color = "coral2", method = "lm", se = F, formula = y ~ x)+
      geom_line(data = df_inclination, aes(y = predicted_inclination))+
      theme_clean()+
      xlim(range(df$medianLH))+
      ylim(range(df$medianBT))+
      xlab("Reproductive investment threshold")+
      ylab("Responsiveness")

    dir.create(here("figs", sim.date, "supplemental", "S1_D"),  showWarnings = F)
    ggsave(here::here("figs", sim.date, "supplemental", "S1_D", paste0("FigD_", i, ".jpeg")), gg, width = 8, height = 8, units = "cm", dpi = 600)
    ggsave(here::here("figs", sim.date, "supplemental", "S1_D", paste0("FigD_", i, ".pdf")), gg, width = 8, height = 8, units = "cm", dpi = 600)} 