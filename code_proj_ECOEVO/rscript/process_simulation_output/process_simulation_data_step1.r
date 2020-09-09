library(here)
library(tidyverse)
library(parallel)
library(doSNOW)
library(foreach)


# if simulation path is not yet defined via the process_simulation_data_all_steps.R script, define it here
if(any(!paste0("L", seq(3)) %in% ls())){
  L1 <- "simulations"
  L2 <- "2020-07-29"
  L3 <- "Main_predictions"
}


# read .csv files located at the path to simulation output
path2sim <- here(L1, L2, L3)

animal.files <- list.files(path2sim, pattern = "animals", full.names = T)

metadata.files <- list.files(path2sim, pattern = "metadata", full.names = T)



# create a new directory to store processed simulation output (surpress warnings if directory already exists)
dir.create(here(L1, L2, L3, "processed"), showWarnings = F)


# check which simulations have already been processed (if any) to avoid unneccesary computations 
output.sim.ids <- do.call(rbind, strsplit(list.files(here(L1, L2, L3, "processed"), pattern = ".csv"), "_"))[, 1]

already.done <- stringi::stri_replace_all_regex(basename(animal.files), "_animals.csv", "") %in% output.sim.ids[duplicated(output.sim.ids)]


animal.files <- animal.files[!already.done]
metadata.files <- metadata.files[!already.done]

# remove metadata.files which do not have a corresponding animals.csv
file.remove(metadata.files[(!substr(basename(metadata.files),1 ,20) %in% substr(basename(animal.files),1 ,20))])

# generate a warning if metadata.files and animal.files are not in the same order
if(!all(substr(basename(metadata.files),1 ,20) == substr(basename(animal.files),1 ,20))) warning("IDs of metadata.files and animal.files do not match")


process_step_1 <- function(file.nr, animal.files, metadata.files, L1, L2, L3){
  if(file.info(animal.files[file.nr])$size > 10000){
    
    ### READ METADATA AND ANIMAL DATA ###
    animals <- readr::read_csv(animal.files[file.nr], col_names = F)

     
    metadata <- readr::read_csv(metadata.files[file.nr])
    
    
    ### SPLIT DATA ###
    split.animals <- split(animals, seq(NROW(animals)))
    dsplit.animals <-lapply(split.animals, stringi::stri_split_regex, pattern = "\\[")
    
    
    ### LISTS ARE EXPORTED FROM NETLOGO AS A CHARACTER STRING, THIS FUNCTIONS SPLITS SINGLE DATA POINTS AND ARRANGES THE DATA IN DATA.FRAME
    dfs <- 
      lapply(dsplit.animals, function(x) {
        
        x <- x[[1]]
        
        for(col in seq(length(x))){
          vectorized <- stringi::stri_split_regex(x[col], " ")[[1]]
          if(length(vectorized) > 1){
            v <- stringi::stri_replace_all_regex(vectorized, pattern = "]", replacement = "")
            v <- v[stringi::stri_length(v) > 0]
            
            df <- setNames(data.frame(v[-1]), v[1])
            assign(paste0("output_", v[1]), df)  
          }
        }
        combined_columns <- do.call("cbind", mget(ls(pattern = "output_")))
        return(combined_columns)
      })
    
    
    merged_data <- do.call(rbind, dfs)
    
    # WRITE PROCESSED DATA TO THE PROCESSED-FOLDER
    data.table::fwrite(merged_data, here::here(L1, L2, L3, "processed", paste0("processed_", basename(animal.files[file.nr]))))
    
    data.table::fwrite(metadata, here::here(L1, L2, L3, "processed", paste0("processed_", basename(metadata.files[file.nr]))))
  }
}


# EXECUTE THIS PROCESSING STEP IN PARALLEL

cl <- makeCluster(n_cores)
registerDoSNOW(cl)
foreach(i = seq(length(animal.files))[order(file.size(animal.files), decreasing = F)
], .packages = "here") %dopar%
  process_step_1(file.nr = i, animal.files = animal.files, metadata.files = metadata.files, L1 = L1, L2 = L2, L3 = L3)
stopCluster(cl)


