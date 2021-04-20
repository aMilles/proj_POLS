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


# show individual intrapopulation POL axes against interpopulation POL axes
if(F){
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

    ggsave(here::here("figs", sim.date, "supplemental", paste0("zz_Fig_SXX_slopecomparison_", i, ".jpeg")), gg, width = 8, height = 8, units = "cm", dpi = 600)
  } 
}


  
# inclinations of intrapopulation POL axes vs inclinations interpopulation POL axis
(gg_inclination <- 
ggplot(df_inclination, aes(x = inclination_interpop, y = inclination_intrapop))+
  geom_line(data = data.frame(x = seq(-5, 5, length.out = 1000), y = seq(-5, 5, length.out = 1000)), aes(x = x, y =y), linetype = "dashed")+
  xlim(range(df_inclination$inclination_intrapop))+
  ylim(range(df_inclination$inclination_interpop))+
  geom_point(aes(color = tot_coefvar))+
  ylab("Intra-population    \nPOL axes (slope)   ")+
  xlab("Inter-population\nPOL axis (slope)")+
  theme_clean()+
  xlim(range(df_inclination$inclination_interpop))+ 
    ylim(range(df_inclination$inclination_intrapop))+
  scale_color_viridis_c("Coefficient of variation\nin population density", values = c(0, 0.05, .1, .2, .4,1), breaks = c(0.3, 0.9), labels = c("0.3 (stable)", "0.9 (labile)"))+
  theme(legend.position = "bottom", panel.grid.major.y = element_blank()))



if("L3" %in% ls()){
  if(L3 == "MainText_LowFreqHighIntensity") {
    gg_inclination <- gg_inclination + 
      scale_y_continuous(breaks = c(0.7, 1.4, 2.1, 2.8), limits =range(df_inclination$inclination_intrapop) )+
      scale_x_continuous(breaks = c(0.7, 1.4), limits = range(df_inclination$inclination_interpop))
  }
  
} 
