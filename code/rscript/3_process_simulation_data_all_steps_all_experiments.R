library(here)

L1 <- "simulations" #L1 folder where simulations are stored
sim.date <- L2 <- "2021-03-28" #L2 date of experiment conduction

experiment.files <- list.files(here(L1, L2), full.names = T)
experiments <- basename(experiment.files[order(file.info(experiment.files)$mtime, decreasing = F)])

n_cores <- 3 #NUMBER OF CORES TO USE FOR PARALLEL PROCESSING OF SIMULATION DATA
return_f2_ <- T #RETURN INTERMEDIATE PRODUCT FOR FURTHER ANALYSES (SET FALSE TO SAVE STORAGE)
start.at_ <- 10000 #INITIAL TIME STEPS TO DISMISS 

# select experiments which haven't been processed yet
experiments_to_process <- vector()
for(L3 in experiments) {
  if(!file.exists(here(L1, L2, L3, "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv"))){
    experiments_to_process <- append(experiments_to_process, L3)
  }
}

# "MainText_Landscape_fluctuation_Example" does not require processing (only xx_ls.csv files used)
experiments_to_process <- experiments_to_process[- which(experiments_to_process == "MainText_Landscape_fluctuation_Example")]

for(L3 in experiments_to_process){

  ##############
  ### STEP 1 ###
  ##############
  
  ### CONVERT SIMULATION DATA TO A DATA FRAME FORMAT
  
  source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step1.R"))
  
  ### CLEANUP
  rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_", "experiments")])
  
  ##############
  ### STEP 2 ###
  ##############
  
  ### AGGREGATE INDIVIDUAL DATA BY SUBPOPULATIONS WHICH ARE DEFINED BY THE THE DECILES OF POPULATION DENSITY
  
  source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step2.R"))
  
  ### CLEANUP
  rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_", "experiments")])
  
  ##############
  ### STEP 3 ###
  ##############
  
  ### COMBINE AGGREGATED DATA FROM SINGLE SIMULATION RUNS TO A SINGLE DATASET 
  source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step3.R"))
  
  ### CLEANUP
  rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_", "experiments")])
}


### CALCULATE LANDSCAPE METRICS FOR SUPPLEMENT ###
if(!file.exists(here(L1, L2, "MainText_Landscape_fluctuation_Example", "ls_merged_agg.csv"))){
  source(here("code", "rscript", "process_simulation_output", "supplement_process_landscape_metrics.R"))
}

