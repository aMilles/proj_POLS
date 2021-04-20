library(here)
library(tidyverse)

# select the sim.date - to create supplemental and main text figures, all experiments stored in the BehaviourSpace and the sensitivty analysis  need to be finished and processed!
sim.date <- "2021-03-28"

# create folders to store figures

dir.create(here("figs", sim.date))
dir.create(here("figs", sim.date, "supplemental")) # PATH TO FIGURES IN SUPPLEMENTAL FIGURES
dir.create(here("figs", sim.date, "main_text")) # PATH TO FIGURES IN MAIN TEXT
dir.create(here("figs", sim.date, "ODD")) # PATH TO FIGURES IN ODD

# copy graphical ODD to the correct folder
file.copy("graphical_ODD.jpg", here("figs", sim.date, "main_text"))
file.rename(here("figs", sim.date, "main_text", "graphical_ODD.jpg"), here("figs", sim.date, "main_text", "Fig1.jpg"))

file.copy("graphical_ODD.jpg", here("figs", sim.date, "ODD"))
file.rename(here("figs", sim.date, "ODD", "graphical_ODD.jpg"), here("figs", sim.date, "ODD", "Fig1.jpg"))

file.copy("mainText_legend_box.jpg", here("figs", sim.date, "main_text"))
file.rename(here("figs", sim.date, "main_text", "mainText_legend_box.jpg"), here("figs", sim.date, "main_text", "legend_box.jpg"))

file.copy("mainText_legend_box.jpg", here("figs", sim.date, "supplemental"))
file.rename(here("figs", sim.date, "supplemental", "mainText_legend_box.jpg"), here("figs", sim.date, "supplemental", "legend_box.jpg"))

file.copy("supplement_legend_box.jpg", here("figs", sim.date, "supplemental"))
file.rename(here("figs", sim.date, "supplemental", "supplement_legend_box.jpg"), here("figs", sim.date, "supplemental", "legend_box_lowtotcoefvar.jpg"))

### MAIN TEXT FIGURES

main.text.figure.scripts <- list.files(here("code", "rscript", "create_main_text_figures"), full.names = T)

# creation of PanelD in Fig 4 is in separate script that is sources by other scripts (so not needed to be sourced separately)
main.text.figure.scripts <- main.text.figure.scripts[-which(stringi::stri_count_regex(main.text.figure.scripts, "main_text_Fig4_1stPrediction_PanelD.R") == 1)]

for(script in main.text.figure.scripts[-1]){
  source(script)
  rm(list = ls()[!ls() %in% c("sim.date", "main.text.figure.scripts")])
}
### SUPPLEMENTAL FIGURES  (including ODD)

for(script in list.files(here("code", "rscript", "create_supplemental_figures"), full.names = T, pattern = "Supplement_[A-Z]")){
  source(script)
  rm(list = ls()[!ls() %in% c("sim.date")])
}
