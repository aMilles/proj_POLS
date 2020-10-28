library(tidyverse)
library(here)
library(ggthemes)


if(!"out.path" %in% ls()){
    
    if(!"sim.date" %in% ls()){
        sim.date = "2020-08-20"
    } 
    
    out.path <- here("simulations", sim.date,"Main_Predictions",  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")
}

stacked <- read_csv(out.path) 

### FIT LINEAR MODELS 
### r0 ~ generation_time + generation_time : population_density + population_density 
### with generation time grouped into slow and fast types by the median

model_stacked <- stacked

# generation generation time groups divided by median
model_stacked$gt_group <- cut(model_stacked$generation_time, c(0, quantile(model_stacked$generation_time, seq(.5, .5, length.out = 1)), max(model_stacked$generation_time)))
levels(model_stacked$gt_group) <- c("fast", "slow")

model_stacked$pop_dens <- model_stacked$pop_dens / (250 * 250)

# fit linear models for each growth type
fit_log <- glm(r0 ~ gt_group + gt_group : pop_dens + pop_dens, model_stacked %>% filter(growth_type == "logistic"), family = "gaussian")

# create data.frame with population density, generation time groups and growth type
pred.df <- expand.grid(gt_group = unique(model_stacked$gt_group), pop_dens = seq(min(model_stacked$pop_dens), max(model_stacked$pop_dens), length.out = 100), growth_type = c("logistic"))

# predict r0
pred.df$p <- NA
pred.df$p <- predict(fit_log, pred.df, type = "response")



################
### FIGURE 6 ###
################


(Fig6 <-
    ggplot(pred.df, aes(x = pop_dens, y = p))+
    geom_point(data = model_stacked %>% filter(growth_type == "logistic"), aes(x = pop_dens, y = r0, fill = gt_group, color = gt_group, group = gt_group),  inherit.aes = F, alpha = 1, shape = 21, size = 1, color = "white")+
    geom_line(aes(color = gt_group, group = gt_group), size = 1, linetype = "solid")+
    scale_y_continuous(limits = c(0, .0215), breaks = c(0, 0.0075, 0.01, 0.0138, .02), labels = c("0.00", expression("r"[0]), 0.01, expression("r"[0]), .02))+
    scale_fill_brewer("Generation time (POL)", type = "qual", palette = "Set2")+
    scale_color_brewer("Generation time (POL)", type = "qual", palette = "Set2")+
     geom_segment(aes(x = 0.25, xend = 0.35, y = 0.0108, yend = 0.0088), arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "turquoise")+
     geom_segment(aes(x = 0.25, xend = 0.35, y = 0.005, yend = 0.0041), arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "coral1", )+
    geom_text(data = pred.df %>% group_by(gt_group) %>% filter(pop_dens == pop_dens[which.min(abs(pop_dens - 0.3))]), aes(x = pop_dens, y = p + c(0.003, -0.002)), label = expression(gamma), color = c("turquoise", "coral1"))+
    theme_clean()+
    xlab("Population density [n/patch]")+
    ylab("Reproductive rate [offspring/t]")+
    theme(legend.position = "top", text = element_text(size = 10), legend.title = element_text(size = 10), legend.text = element_text(size = 10), plot.background = element_rect(fill = NA, color = NA)))

ggsave(here::here("figs", sim.date, "main_text", "Fig6.png"), Fig6, width = 9, height = 8, units = "cm", dpi = 600)
