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
fit_lin <- glm(r0 ~ gt_group + gt_group : pop_dens + pop_dens, model_stacked %>% filter(growth_type == "linear"), family = "gaussian")
fit_log <- glm(r0 ~ gt_group + gt_group : pop_dens + pop_dens, model_stacked %>% filter(growth_type == "logistic"), family = "gaussian")

# create data.frame with population density, generation time groups and growth type
pred.df <- expand.grid(gt_group = unique(model_stacked$gt_group), pop_dens = seq(min(model_stacked$pop_dens), max(model_stacked$pop_dens), length.out = 100), growth_type = c("linear", "logistic"))

# predict r0
pred.df$p <- NA
pred.df$p[pred.df$growth_type == "linear"] <- predict(fit_lin, pred.df[pred.df$growth_type == "linear",], type = "response")
pred.df$p[pred.df$growth_type == "logistic"] <- predict(fit_log, pred.df[pred.df$growth_type == "logistic",], type = "response")



################
### FIGURE 6 ###
################


(Fig6 <-
    ggplot(pred.df %>% filter(growth_type == "logistic"), aes(x = pop_dens, y = p, color = gt_group, group = gt_group))+
    geom_point(data = model_stacked %>% filter(growth_type == "logistic"), aes(x = pop_dens, y = r0, fill = gt_group),  inherit.aes = F, alpha = 1, shape = 21, size = 1, color = "white")+
    geom_line(size = 1, linetype = "solid")+
    scale_y_continuous(limits = c(0, .012))+
    scale_fill_brewer("Generation time", type = "qual", palette = "Set2")+
    scale_color_brewer("Generation time", type = "qual", palette = "Set2")+
    theme_clean()+
    xlab("Population density [n / patch]")+
    ylab("Reproductive rate [offspring / t]")+
    theme(legend.position = "top", text = element_text(size = 10), legend.title = element_text(size = 10), legend.text = element_text(size = 10), plot.background = element_rect(fill = NA, color = NA)))

ggsave(here::here("figs", sim.date, "main_text", "Fig6.png"), Fig6, width = 8, height = 8, units = "cm", dpi = 600)
