library(tidyverse)
library(here)
library(ggthemes)

# read csv file with aggregated data

if(!"out.path" %in% ls()){    
  if(!"sim.date" %in% ls()){
    sim.date = "2020-08-20"
  } 
  out.path <- here("simulations", sim.date,"Supplement_altered_growth_factor_logistic_POLS",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
  
}


# read data
stacked <- read_csv(out.path) 
names(stacked)
# assign 
SX07_data <-
  stacked[, - which(names(stacked) == "tot_coefvar")] %>% 
  filter(growth_type == "logistic") %>% 
mutate("Movement rate\n(scaled SD)" = scale(sd_movement_activity)) %>% 
  mutate("Rate of investment\nto reproduction (scaled SD)" = scale(sd_repo_activity)) %>% 
  mutate("Responsiveness\n(BT, scaled SD)" = scale(sdBT)) %>% 
  mutate("Relative investment to\nreproduction (LH, scaled SD)" = scale(sdLH)) %>% 
  dplyr::select(generation_time, "Movement rate\n(scaled SD)", "Rate of investment\nto reproduction (scaled SD)", "Responsiveness\n(BT, scaled SD)", "Relative investment to\nreproduction (LH, scaled SD)") %>% 
  reshape2::melt(id.vars = c("generation_time"))
# create figure
(SX07A <- 
    ggplot(SX07_data, aes(x = log10(generation_time), y = value))+
    geom_point(alpha = 1, shape = 21, fill = "gray40", color = "white", size = .8)+
    theme_clean() +
    theme(plot.background = element_rect(fill = NA, color = NA), panel.grid.major = element_blank(), panel.grid.major.y = element_blank(), strip.placement = "outside")+
    facet_wrap(~variable, ncol = 2, strip.position = "left")+
    xlab("Generation time (log10)") +
    ylab("")+
    scale_x_continuous(breaks = c(2, 2.5, 3))
  
)



model_stacked <- stacked %>% filter(growth_type == "logistic")

# generation generation time groups divided by median
model_stacked$gt_group <- cut(model_stacked$generation_time, c(0, quantile(model_stacked$generation_time, seq(.5, .5, length.out = 1)), max(model_stacked$generation_time)))
levels(model_stacked$gt_group) <- c("fast", "slow")

model_stacked$pop_dens <- model_stacked$pop_dens / (250 * 250)

# fit linear models for each growth type
fit_log <- glm(r0 ~ gt_group + gt_group : pop_dens, model_stacked %>% filter(growth_type == "logistic"), family = "gaussian")

# create data.frame with population density, generation time groups and growth type
pred.df <- expand.grid(gt_group = unique(model_stacked$gt_group), pop_dens = seq(min(model_stacked$pop_dens), max(model_stacked$pop_dens), length.out = 100), growth_type = c("logistic"))

# predict r0
pred.df$p <- NA
pred.df$p[pred.df$growth_type == "logistic"] <- predict(fit_log, pred.df[pred.df$growth_type == "logistic",], type = "response")

SX07B <- 
ggplot(pred.df %>% filter(growth_type == "logistic"), aes(x = pop_dens, y = p))+
  geom_point(data = model_stacked %>% filter(growth_type == "logistic"), aes(x = pop_dens, y = r0, fill = gt_group, color = gt_group, group = gt_group),  inherit.aes = F, alpha = 1, shape = 21, size = 1, color = "white")+
  geom_line(aes(color = gt_group, group = gt_group), size = 1, linetype = "solid")+
  scale_fill_brewer("Generation time (POL)", type = "qual", palette = "Set2")+
  scale_color_brewer("Generation time (POL)", type = "qual", palette = "Set2")+
  theme_clean()+
  xlab("Population density [n/patch]")+
  ylab("Reproductive rate [offspring/t]")+
  theme(legend.position = "top", text = element_text(size = 10), legend.title = element_text(size = 10), legend.text = element_text(size = 10), plot.background = element_rect(fill = NA, color = NA))

SX07 <- gridExtra::grid.arrange(SX07A, SX07B, layout_matrix = as.matrix(c(1,1,1,1,1,1,1,1, 2,2,2,2), nc = 2))

ggsave(here::here("figs", sim.date, "supplemental", paste0("Fig_SX07", ".jpeg")), SX07, width = 10, height = 15.5, units = "cm", dpi = 600)

