# This script uses the nlrx-package to run NetLogo from R with a specified set of parameters for sensitivity analysis

library(nlrx)
library(tidyverse)
library(here)
library(future)

# Parameters for sensitivity analysis (name, default, min, max, use-case)
ps <-    c("n-inds",                             .1,   0.01,     1,    "all",
           "maintenance-cost",                  0.15,  0.01,     0.25, "all",
           "reproduction-threshold",           50,    30,      100,    "all",
           "resource-growth-rate-logistic",     0.01,     0.2,      2,    "log",
           "resource-growth-rate-linear",       0.15,  0.01,     0.3,  "linear",
           "resource-growth-limit",            15,     5,       20,    "all",
           "handling-time",                     0.7,   0,        1,    "all",
           "encounter-rate",                    0.3,   0,        1,    "all",
           "stochasticity-BT",                  0.5,   0,        1,    "all",
           "stochasticity-LH",                  0.5,   0,        1,    "all")


# convert Parameters above into data.frame
ps_df <- setNames(data.frame(matrix(ps, nc = 5, nr = 10, byrow = T)[,c(1,5)], apply(matrix(ps, nc = 5, nr = 10, byrow = T)[,2:4], 2, as.numeric), stringsAsFactors = F), c("param", "use_case", "def", "low", "up"))


# set a lower and upper state (s1, s2) for each parameter
param_combs <- ps_df %>% 
  mutate(s1 = (def + low)/2) %>% 
  mutate(s2 = (def + up) / 2) 

# replace default value with s1 and s2 respectively, do not vary paramters that do not affect scenarios with logistic growth
log_growth <- param_combs %>% 
  mutate(s1 = ifelse(use_case != "log", s1, def)) %>% 
  mutate(s2 = ifelse(use_case != "log", s2, def)) %>% 
  dplyr::select(param, def, s1, s2) 

# create matrix of parameter sets
param_mat <- matrix(rep(log_growth$def, 20), nrow = 10, ncol = 20)

for(step in seq(1, ncol(param_mat), by = 2)){
  param_nr <- ceiling(step/2)
  param_mat[, step] <- log_growth$def
  param_mat[, step + 1] <- log_growth$def
  param_mat[param_nr, step + 1] <- log_growth$s1[param_nr]
  param_mat[param_nr, step] <- log_growth$s2[param_nr]
}

# repliacte the matrix so two different disturbance-frequency levels are added
log_oat <- do.call("rbind", replicate(setNames(data.frame(t(param_mat)), log_growth$param), n = 2, simplify = F))
log_oat$`disturbance-interval` <- rep(c(100, 500), each = 20)

#log_oat <- log_oat[!duplicated(log_oat),]
# convert the parameter sets to a nlrx-readable format
variables <- as.list(log_oat)

variables <- lapply(variables, function(x) list("values" = x))

# create a list of parameters kept constant (remember to specify the output-directory!)
# adjust the output-directory!

out.dir <- paste0("\"", here("simulations", "2021-03-28", "Supplement_SensitivityAnalysis"), "/\"")
out.dir <- stringi::stri_replace_all_regex(out.dir, getwd(), "")


constants <-
list("disturbance-intensity" = 50, 
     "create-output" = "true",
     "save-landscape" = "false", 
     "plot-update-frequency" = 9999999,
     "plot-animals" = "false",
     "plot-landscape" = "false",
     "growth-type" = "\"linear\"",
     "output-directory" = out.dir,
     "breeding-type" = "\"capital-breeding\"",
     "monomorphic" = "false",
     "restart?" = "false")

dir.create(here("simulations", "2021-03-28", "Supplement_SensitivityAnalysis"))


# output variables and constants to include them in the supplement
write.table(log_oat, file = here("table", "Supplement", "sensitvity_analysis_variables.txt"), sep = ",", quote = FALSE, row.names = F)

write.table(data.frame(constants)[-8], file = here("table", "Supplement", "sensitvity_analysis_constants.txt"), sep = ",", quote = FALSE, row.names = F)



######################################
#### RUN THE SENSITIVITY ANALYSIS ####
######################################

# PATH TO NETLOGO AND THE MODEL 
if(!"path2netlogo" %in% ls()) path2netlogo <- "Y:/Home/milles/NetLogo 6.1.1"
if(!"path2model" %in% ls()) path2model <- file.path("Y:/Home/milles/proj_POLS/code/model/POLS_model.nlogo")

# setup the nl object
nl <- nl(nlversion = "6.1.1", nlpath = path2netlogo, modelpath = path2model, jvmmem = 4096)

 #runtime may get formatted otherwise.. 
options(scipen = 100)

# setup the experiment
nl@experiment <- experiment(expname = "Supplement_SensitivityAnalysis", 
                            outpath = here(),
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 100001,  #scipen is resetted in split run?! - so instead of 100000, 99999 ..
                            variables = variables, 
                            constants = constants)


# define the simulation design (simple = only constants required)
nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)

# run sensitivity analysis in parallel
plan(multisession, gc = T, workers = 4)
results <- nlrx::run_nl_all(nl, split = 4)

