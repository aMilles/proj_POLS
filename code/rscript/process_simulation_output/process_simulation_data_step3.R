### PROCESSING STEP 3 ###
# IN THIS SCRIPT 
# USING THE STEP-2-PROCESSED OUTPUT 
# to aggregate output of all simulations of one experiment to one data set

library(tidyverse)
library(here)
library(ggthemes)

# if simulation path is not yet defined via the process_simulation_data_all_steps.R script, define it here
if(any(!paste0("L", seq(3)) %in% ls())){
  L1 <- "simulations"
  L2 <- "2021-03-28"
  L3 <- "MainText_LowFreqHighIntensity"
}



# if simulation output from 2020-08-20 is used BT needs to be transformed by 2 - BT, later simulations do not require this transformation as it is implemented in the model 

transform.BT <- L2 == "2020-08-20" 

# get files with aggregated simulations
agg.path <- here(L1,  L2,L3,  "processed", "output_aggregated")
stacked.file <-  here(L1,  L2,L3,  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")


aggregated.files <- list.files(agg.path, full.names = T)

# stack the data into one dataset
stacked_aggregated <- do.call(rbind, lapply(as.list(aggregated.files), function(x) readr::read_csv(x)))

if (transform.BT) stacked_aggregated$medianBT <- 2 - stacked_aggregated$medianBT

# save them in a folder

dir.create(dirname(stacked.file))

stacked_aggregated %>% 
  select(-ticks, 
         -who, 
         -BT, 
         -LH, 
         -age, 
         -frepro, 
         -n_offspring, 
         -soma, 
         -death.tick, 
         -birth.tick, 
         -max_birthtick,
         -max_deathtick,
         -death.cause,
         -movement_activity,
         -repo_activity,
         )
names(stacked_aggregated)

data.table::fwrite(stacked_aggregated, file = stacked.file)



### DO THE SAME WITH INTERMEDIATE STEP 2 DATA

# get files with aggregated simulations
intermediate.path <- here(L1,  L2,L3,  "processed", "output_2ndStep_2ndfilter")

# currently these files are not generated in step 2
if(file.exists(intermediate.path)){
  stacked.file <-  here(L1,  L2,L3,  "processed", "output_2ndStep_2ndfilter", "output_intermediate_stacked", "stacked_output_2ndStep_2ndfilter.csv")
  
  
  intermediate.files <- list.files(intermediate.path, full.names = T)
  
  # stack the data into one dataset
  stacked_intermediate <- do.call(rbind, lapply(as.list(intermediate.files), function(x) readr::read_csv(x)))
  
  if (transform.BT) stacked_intermediate$medianBT <- 2 - stacked_intermediate$medianBT
  
  
  # save them in a folder
  dir.create(dirname(stacked.file))
  data.table::fwrite(stacked_intermediate, file = stacked.file)
}

