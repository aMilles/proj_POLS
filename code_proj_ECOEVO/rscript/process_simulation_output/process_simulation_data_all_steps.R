library(here)

L1 <- "simulations" #L1 folder where simulations are stored
sim.date <- L2 <- "2020-08-20" #L2 date of experiment conduction
L3 <- "S1_Saturation" #L3 experiment_name: only experiments with create-output = T and save-landscape = F

n_cores <- 2 #NUMBER OF CORES TO USE FOR PARALLEL PROCESSING OF SIMULATION DATA

##############
### STEP 1 ###
##############

### CONVERT SIMULATION DATA TO A DATA FRAME FORMAT

source(here("code_proj_ECOEVO", "rscript", "process_simulation_data_step1.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date")])

##############
### STEP 2 ###
##############

### AGGREGATE INDIVIDUAL DATA BY SUBPOPULATIONS WHICH ARE DEFINED BY THE THE DECILES OF POPULATION DENSITY

source(here("code_proj_ECOEVO", "rscript", "process_simulation_data_step2.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date")])

##############
### STEP 3 ###
##############

### COMBINE AGGREGATED DATA FROM SINGLE SIMULATION RUNS TO A SINGLE DATASET 
source(here("code_proj_ECOEVO", "rscript", "process_simulation_data_step3.R"))

### CLEANUP
rm(list = ls()[!ls() %in% c(paste0("L", seq(3)),"n_cores", "sim.date")])

