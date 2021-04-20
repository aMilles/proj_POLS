library(here)

L1 <- "simulations" #L1 folder where simulations are stored
sim.date <- L2 <- "2021-03-28" #L2 date of experiment conduction
L3 <- "MainText_LowFreqHighIntensity" 

#L3 experiment_name: only experiments with create-output = T and save-landscape = F

n_cores <- 5 #NUMBER OF CORES TO USE FOR PARALLEL PROCESSING OF SIMULATION DATA
return_f2_ <- T #RETURN INTERMEDIATE PRODUCT FOR FURTHER ANALYSES (SET FALSE TO SAVE STORAGE)
start.at_ <- 10000 #INITIAL TIME STEPS TO DISMISS 
##############
### STEP 1 ###
##############

### CONVERT SIMULATION DATA TO A DATA FRAME FORMAT

source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step1.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_")])

##############
### STEP 2 ###
##############

### AGGREGATE INDIVIDUAL DATA BY SUBPOPULATIONS WHICH ARE DEFINED BY THE THE DECILES OF POPULATION DENSITY

source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step2.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_")])

##############
### STEP 3 ###
##############

### COMBINE AGGREGATED DATA FROM SINGLE SIMULATION RUNS TO A SINGLE DATASET 
source(here("code", "rscript", "process_simulation_output", "process_simulation_data_step3.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date", "return_f2_", "start.at_")])

