### PROCESSING STEP 2 ###
# IN THIS SCRIPT 
# USING THE STEP-1-PROCESSED OUTPUT 
# Further life history traits are calculated
# data of individuals is aggregated to the level of subpopulations (median trait values, IQR, standard deviation are calculated)
# subpopulations are defined by the decile of population density individuals experienced
# output only returns data of the aggregated level

library(tidyverse)
library(here)
library(foreach)
library(doSNOW)
library(parallel)
library(tibble)

# if simulation path is not yet defined via the process_simulation_data_all_steps.R script, define it here
if(any(!paste0("L", seq(3)) %in% ls())){
  L1 <- "simulations"
  L2 <- "2020-08-20"
  L3 <- "Main_predictions"
}


# paths to step1 data
sim.path <- here(L1, L2,  L3, "processed")

# paths to aggregated data 
out.path_aggregated <- here(L1, L2,  L3,  "processed", "output_aggregated")
out.path_intermediate <- here(L1, L2,  L3,  "processed", "output_2ndStep_2ndfilter")

# read files to step1 data 
animal.files <- list.files(sim.path, pattern = "animals.csv", full.names = T)
metadata.files <- list.files(sim.path, pattern = "metadata.csv", full.names = T)

# give warning if animal files and metadata files do not match
if(!all(substr(basename(animal.files), 1, 25) == substr(basename(metadata.files), 1, 25))) warning("Metadata and animal files do not match!")
match(substr(basename(metadata.files), 1, 25), substr(basename(animal.files), 1, 25))


# check for files that have already been processed to save computation time
already_done <- basename(metadata.files) %in% list.files(out.path_aggregated)


# arrange corresponding metadata and animal files in a vector as an element of a list
file_df <- data.frame(metadata.files, animal.files)[!already_done, ]
temp_file <- t(apply(file_df, 1, paste))
file_list <- split(temp_file, seq(nrow(temp_file)))

start.at = 0
X = file_list[[2]]
# STEP 2 FUNCTION
process_sim_step2 <- 
  function(X, out.path_aggregated, out.path_intermediate, start.at = 5000, return_f2 = F) {
    
    
    # function to calculate coefficient of variation
    coef_var <- function(x) sd(x)/mean(x) 
    
    # read metadata and ...
    metadata <- read_csv(X[1])
    animals <- readr::read_csv(X[2])
    
    #######################
    ### 1st filter step ###
    #######################
    
    # corresponding animal data, 
    # add metadata to animal data
    # calculate time of death and birth
    # remove individuals which were born before the start.at tick (default = 5,000)
    # remove individuals from the dataset which have immigrated (only 1st generation!)
    filtered_step1 <- animals %>% 
      mutate(growth_type = metadata$growthtype) %>% 
      mutate(sim.id = strsplit(basename(X[2]), "_")[[1]][2]) %>% 
      group_by(who) %>% 
      mutate(death.tick = ifelse(length(ticks > 2), max(ticks), 40000)) %>% 
      mutate(birth.tick = min(ticks)) %>% 
      filter(birth.tick > start.at) %>% 
      ungroup() %>% 
      mutate(max_birthtick = max(birth.tick)) %>%
      mutate(max_deathtick = max(death.tick))
    
    # if any animals remain in the dataset, process data
    if(nrow(filtered_step1) > 0){
      
      
      # process population density data
      population_data <- rbind(c(0, metadata$ninds), filtered_step1[, c("ticks", "ninds")])
      
      # population density is measured if an animal dies or reproduces, aggregate this data for each time step
      population_data <-
        population_data %>% 
        group_by(ticks) %>% 
        mutate(ninds = mean(ninds)) %>% 
        filter(!duplicated(ticks))
      
      # times without changes in population size (no death / no birth) are filled with previous values
      approx.this <- data.frame(ticks = seq(start.at, max(population_data$ticks)))
      
      population_data <- merge(approx.this, population_data, all.x = T) %>% 
        fill(ninds, .direction = "down") %>% 
        fill(ninds, .direction = "up")
      

      #######################
      ### 2nd filter step ###
      #######################
      
      # add traits (age of first reproduction, life span) and phenotypic behaviour for individuals
      # animals state is saved at birth and death (2 rows) now this is information is redunant and the data is reduced to one row
      
      filtered_step2 <- 
        filtered_step1 %>% 
        group_by(who) %>%
        filter(max(generation_time) > 0) %>% 
        mutate(death.cause = ifelse(min(soma > 0), "environment", "soma")) %>%
        mutate(frepro = max(frepro)) %>% 
        mutate(longevity = max(age)) %>% 
        mutate(times_moved = max(times_moved)) %>% 
        mutate(movement_activity = times_moved / longevity) %>% 
        mutate(n_offspring = max(n_offspring)) %>% 
        mutate(r_buffer = 50 * n_offspring + max(r_buffer)) %>% 
        mutate(repo_activity = r_buffer / longevity) %>%
        mutate(r0 = n_offspring / longevity) %>%
        mutate(generation_time = max(generation_time)) %>%
        filter(!duplicated(who)) %>% 
        filter(!is.na(death.tick) & !is.na(birth.tick)) 
      
      summary(filtered_step2$movement_activity)
      
      # calculate the mean population density experienced by an individual
      vec.ind.list <- apply(filtered_step2[, c("birth.tick", "death.tick")], 1, function(x) {
        population_data[(x[1] - (start.at - 1)):(x[2] - (start.at - 1)), "ninds"]
      })
      
      filtered_step2$pop_dens <- unlist(lapply(vec.ind.list, mean))
      
      # if there are any NAs, drop them
      print(paste0(nrow(filtered_step2) - nrow(filtered_step2 %>% drop_na()), " rows were removed as they contain NA values"))
      filtered_step2 <- filtered_step2 %>%
        drop_na()
      
      # assign groups to individuals based on the deciles of population density
      filtered_step2$pop_group <- cut(filtered_step2$pop_dens, breaks = unique(c(0, quantile(filtered_step2$pop_dens, probs = seq(0.1, .9, length.out = 9)), max(filtered_step2$pop_dens))))
      
      
      # calculate the coefficient of variation
      filtered_step2$tot_coefvar <- coef_var(population_data$ninds)
      
    
      # create output before aggregation
      # this is disabled to save space
      if(return_f2){
        intermediate_output <- filtered_step2 %>% 
          select(BT, LH, movement_activity, repo_activity, tot_coefvar, pop_dens, r0, generation_time, growth_type)
        
        dir.create(out.path_intermediate, showWarnings = F)
        readr::write_csv(intermediate_output, path = paste0(out.path_intermediate, "/", basename(X[2])))
      }
      
      #######################
      ### 3rd filter step ###
      #######################
      # group individuals by the decile of population density as calculated before
      # calculate trait distribution for each of these populations including interquartile range
      # calculate mean of longevity and further metrics
      # calulate distriubtion of phenotypic behaviour for each of these populations including interquartile range
      filtered_step3 <-
        filtered_step2 %>%
        group_by(pop_group) %>%
        mutate(group_size = length(ninds)) %>%
       
        # aggregate metrics for each group
        mutate(pop_dens = mean(pop_dens)) %>%
        mutate(longevity = mean(longevity)) %>% 
        mutate(r_buffer = mean(r_buffer)) %>% 
        mutate(times_moved = median(times_moved)) %>% 
        mutate(generation_time = median(generation_time)) %>% 
        mutate(em_rate = sum(death.cause == "environment")/length(death.cause)) %>% 

        # movement activity distribution
        mutate(median_movement_activity = median(movement_activity)) %>% 
        mutate(upper_movement_activity = quantile(movement_activity, 0.75)) %>%
        mutate(lower_movement_activity = quantile(movement_activity, 0.25)) %>%
        mutate(sd_movement_activity = sd(movement_activity)) %>% 
        
        
        # investment to reproduction distribution
        mutate(median_repo_activity = median(repo_activity)) %>%
        mutate(upper_repo_activity = quantile(repo_activity, 0.75)) %>%
        mutate(lower_repo_activity = quantile(repo_activity, 0.25)) %>%
        mutate(sd_repo_activity = sd(repo_activity)) %>% 
        
        # behavioural trait distribution
        mutate(medianBT = median(BT)) %>%
        mutate(upperBT = quantile(BT, 0.75)) %>%
        mutate(lowerBT = quantile(BT, 0.25)) %>%
        mutate(sdBT = sd(BT)) %>% 
        
        # life history trait distribution
        mutate(medianLH = median(LH)) %>%
        mutate(upperLH = quantile(LH, 0.75)) %>%
        mutate(lowerLH = quantile(LH, 0.25)) %>%
        mutate(sdLH = sd(LH)) %>% 
        
        # retain one data point per aggregation 
        filter(!duplicated(pop_group))
      
      filtered_step3 <- data.frame(filtered_step3, metadata)
      
      # save aggregated data
      dir.create(out.path_aggregated, showWarnings = F)
      readr::write_csv(filtered_step3, path = paste0(out.path_aggregated, "/", basename(X[2] )))
      
    }
  }


# run function in parallel

cl <- makeCluster(n_cores)
registerDoSNOW(cl)
foreach(i = seq(length(file_list)), .packages = c("tidyverse", "here")) %dopar%
  process_sim_step2(X = file_list[[i]], out.path_aggregated = out.path_aggregated, out.path_intermediate = out.path_intermediate, start.at = 5000, return_f2 = F)
stopCluster(cl)

