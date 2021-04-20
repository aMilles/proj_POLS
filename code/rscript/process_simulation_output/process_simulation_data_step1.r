### PROCESSING STEP 1 ###
# IN THIS SCRIPT 
# USING THE RAW SIMULATION OUTPUT
# data wrangling to rearrange NetLogo Output of individuals in single simulations to a data.frame object

library(here)
library(tidyverse)
library(parallel)
library(doSNOW)
library(foreach)


# if simulation path is not yet defined via the process_simulation_data_all_steps.R script, define it here
if(any(!paste0("L", seq(3)) %in% ls())){
  L1 <- "simulations"
  L2 <- "2021-03-28"
  L3 <- "MainText_LowFreqHighIntensity"
}

# borrowed from https://stackoverflow.com/questions/17288197/reading-a-csv-file-organized-horizontally
read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  x = readLines(file)
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}


### Transose Netlogo-Output and check whether the simulation really finished or has been processed already
process_step_1 <- function(files, L1, L2, L3, id, min.ticks = 100000, skip10000 = T){
  
  target.file <- here::here(L1, L2, L3, "processed", paste0("processed_", id, "_animals.csv"))
  
  if(skip10000) files <- files[-which(stringi::stri_count_regex(files, "_10000animals.csv") == 1)]
  
  if(file.exists(target.file)){
    print(paste0("I'm lazy and processed ", target.file, " already"))
  }else{
    if(length(files) >= ifelse(skip10000, (min.ticks/10000) - 1, min.ticks/10000)){
      transposed.list <- lapply(as.list(files), read.tcsv, header = T)
      transposed.bound <- do.call(rbind, transposed.list)
      data.table::fwrite(transposed.bound, target.file)
    }else{
     print(paste0("Population went extinct before ", min.ticks,  "-  will not process"))
    } 
  }
}


# get simulation files

path2sim <- here(L1, L2, L3)

animal.files <- list.files(path2sim, pattern = "animals", full.names = T)
file.remove(animal.files[which(stringi::stri_count_regex(animal.files, "_0animals") == 1)])


animal.files <- list.files(path2sim, pattern = "animals", full.names = T)

metadata.files <- list.files(path2sim, pattern = "metadata", full.names = T)

ids <- do.call(rbind, str_split(basename(animal.files), "_"))[,1]

animal.files.grouped <- split(animal.files, ids)


# remove simulations that have certainly not converged
finished.ids <- unique(ids)[unlist(lapply(animal.files.grouped, length)) == 10]
unfinished.animal.files <- do.call(c, animal.files.grouped[unlist(lapply(animal.files.grouped, length)) < 10])
if(length(unfinished.animal.files) > 0) file.remove(unfinished.animal.files)
animal.files.grouped <- animal.files.grouped[unlist(lapply(animal.files.grouped, length)) >= 10]


# copy metadata of finished simulations to processed

metadata.ids <- do.call(rbind, str_split(basename(metadata.files), "_"))[,1]
dir.create(here(L1, L2, L3, "processed"))
file.copy(metadata.files[metadata.ids %in% finished.ids], here(L1, L2, L3, "processed"))
file.remove(metadata.files[!metadata.ids %in% finished.ids])

## test
if(F){
  files <- animal.files.grouped[[1]]
  id = names(animal.files.grouped)[1]
  process_step_1(files, L1 = L1, L2 = L2, L3 = L3, id = id)
}

# EXECUTE THIS PROCESSING STEP IN PARALLEL
n_cores = 5
cl <- makeCluster(n_cores)
registerDoSNOW(cl)
foreach(i = seq(length(animal.files.grouped)), .packages = c("here", "tidyverse")) %dopar%
  process_step_1(files = animal.files.grouped[[i]], id = names(animal.files.grouped)[i], skip10000 = T, L1 = L1, L2 = L2, L3 = L3)
stopCluster(cl)




