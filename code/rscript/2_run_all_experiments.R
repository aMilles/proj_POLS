library(here)


#### THIS SCRIPT RUNS ALL EXPERIMENTS HEADLESS (VIA COMMAND LINE) AND FROM R

#### THIS SCRIPT WILL NEED QUITE SOME TIME TO FINISH 
#### ABOUT ONE DAY WITH THE NUMBER OF CORES - UP TO 20 - AS SPECIFIED BELOW
#### PROCESSING THE FILES WILL NEED SEVERAL GBs

#### IN CASE YOU ONLY WANT TO REPRODUCE RESULTS OF THE MAIN TEXT 
#### REMOVE ALL ROWS WITH EXPERIMENTS NAMED SUPPLEMENT_xy IN THE EXPERIMENTS DATA.FRAME 


##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ###
##### ADJUST THIS TO YOUR NETLOGO 6.1.1 LOCATION ###
path2headless <- "\"Y:/Home/milles/NetLogo 6.1.1/netlogo-headless.bat\""


# Initialize some folders
dir.create("simulations")
dir.create(here("simulations", "2021-03-28"))

dir.create("figs")

dir.create("table")
dir.create("table", "Supplement")

#### IF YOU OPENED THE R-PROJECT, I.E. WORKING DIRECTORY IS IN THE MAIN FOLDRE OF THE PROJECT NO CHANGE NEEDED
path2netlogo <- substring(dirname(path2headless), first = 2)
path2model <- here("code", "model", "POLS_model.nlogo")
path2sims <- here("simulations", "2021-03-28")

experiments <- data.frame(exp_name = c(
  "MainText_Landscape_fluctuation_Example",
  "MainText_LowFreqHighIntensity",
  "Supplement_LowFreqHighIntensity_monomorphic",
  "Supplement_LowFreqHighIntensity_incomebreeding",
  "Supplement_LowFreqHighIntensity_loggrowthVerhulst",
  "Supplement_LowFreqHighIntensity_loggrowthRichards",
  "Supplement_LowFreqHighIntensity_highh2",
  "Supplement_HighFreqLowIntensity",
  "Supplement_Saturation",
  "Supplement_altered_movementassumption_landscape",
  "Supplement_altered_movementassumption_POLS"
), threads = c(
  2,
  20,
  20,
  20,
  20,
  20,
  20,
  20,
  3,
  4,
  20
), stringsAsFactors = F)


#experiments$threads <- ifelse(experiments$threads < 5, experiments$threads, 5)

# RUN ALL EXPERIMENTS SETUP IN THE BEHAVIOUR SPACE OF NETLOGO HEADLESS

# without experiments that have been processed already
experiments<- experiments[!file.exists(here("simulations", "2021-03-28", experiments[,1])),]

for(experiment in seq(nrow(experiments))){
  dir.create(paste0(path2sims, "/", experiments$exp_name[experiment]))
  print(paste("Running", experiments$exp_name[experiment], "with", experiments$threads[experiment], "cores!"))
  shell.cmd <- ( paste(path2headless, "--model", path2model, "--experiment", experiments$exp_name[experiment], "--threads", experiments$threads[experiment]))
  shell(shell.cmd)
}

# RUN SENSITIVITY ANALYSIS WHICH IS SETUP IN R WITH THE NLRX PACKAGE 
# (MAKE SURE PACKAGES ARE INSTALLED)
# nlrx, tidyverse, future

source(here("code", "rscript", "process_simulation_output", "supplement_process_SensitivityAnalysis.R"))

