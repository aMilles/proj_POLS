library(here)
library(tidyverse)

sim.date <- "2020-08-20"

dir.create(here("figs", sim.date))
dir.create(here("figs", sim.date, "supplemental")) # PATH TO FIGURES IN SUPPLEMENTAL FIGURES
dir.create(here("figs", sim.date, "main_text")) # PATH TO FIGURES IN MAIN TEXT
dir.create(here("figs", sim.date, "ODD")) # PATH TO FIGURES IN ODD



### MAIN TEXT FIGURES

for(script in list.files(here("code_proj_ECOEVO", "rscript", "create_main_text_figures"), full.names = T)[-1]){
  source(script)
  rm(list = ls()[!ls() %in% c("sim.date")])
}
  

### SUPPLEMENTAL FIGURES  (including ODD)

for(script in list.files(here("code_proj_ECOEVO", "rscript", "create_supplemental_figures"), full.names = T)){
  source(script)
  rm(list = ls()[!ls() %in% c("sim.date")])
}


file.copy("graphical_ODD.png", here("figs", sim.date, "main_text"))
file.rename(here("figs", sim.date, "main_text", "graphical_ODD.png"), here("figs", sim.date, "main_text", "Fig1.png"))

file.copy("graphical_ODD.png", here("figs", sim.date, "ODD"))
file.rename(here("figs", sim.date, "ODD", "graphical_ODD.png"), here("figs", sim.date, "ODD", "Fig1.png"))
