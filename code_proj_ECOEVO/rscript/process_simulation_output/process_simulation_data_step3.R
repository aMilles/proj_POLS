library(shiny)
library(tidyverse)
library(here)
library(ggthemes)

# if simulation path is not yet defined via the process_simulation_data_all_steps.R script, define it here
if(any(!paste0("L", seq(3)) %in% ls())){
  L1 <- "simulations"
  L2 <- "2020-07-25"
  L3 <- "Main_predictions"
}

# get files with aggregated simulations
agg.path <- here(L1,  L2,L3,  "processed", "output_aggregated")
stacked.file <-  here(L1,  L2,L3,  "processed", "output_aggregated", "output_stacked", "stacked_aggregated_output.csv")


aggregated.files <- list.files(agg.path, full.names = T)

# stack the data into one dataset
stacked_aggregated <- do.call(rbind, lapply(as.list(aggregated.files), function(x) readr::read_csv(x)))

# save them in a folder
dir.create(dirname(stacked.file))
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
  
  # save them in a folder
  dir.create(dirname(stacked.file))
  data.table::fwrite(stacked_intermediate, file = stacked.file)
}


### 

# get files with aggregated simulations
agg.path_alt <- here(L1,  L2,L3,  "processed", "output_aggregated_alternative")

# currently these files are not generated in step 2
if(file.exists(agg.path_alt)){
  stacked.file <-  here(L1,  L2,L3,  "processed", "output_aggregated_alternative", "output_aggregated_stacked", "stacked_aggregated_output.csv")
  
  
  agg_alt.files <- list.files(agg.path_alt, full.names = T)
  
  # stack the data into one dataset
  stacked_agg_alt <- do.call(rbind, lapply(as.list(agg_alt.files), function(x) readr::read_csv(x)))
  
  # save them in a folder
  dir.create(dirname(stacked.file))
  data.table::fwrite(stacked_agg_alt, file = stacked.file)
}