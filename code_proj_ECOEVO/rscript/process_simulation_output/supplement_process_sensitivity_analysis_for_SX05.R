library(nlrx)
library(tidyverse)
library(here)
library(future)

# Parameters for sensitivity analysis
ps <-    c("n-inds",                   6250,     1,     10000, "all",
           "maintenance-cost",           0.15,  0.01,      0.25, "all",
           "reproduction-threshold",     50,    30,       100, "all",
           "growth-factor-logistic",     3,     0.5,       7, "log",
           "growth-rate-linear",         0.1,   0.01,      0.3, "linear",
           "growth-limit-linear",       15,     5,        20, "linear",
           "harvest-rate-factor",        5,     0.5,      10, "all",
           "harvest-rate-curvature",     1.5,   0.1,       3, "all",
           "stochasticity-BT",           0.05,  0.01,      0.1, "all",
           "stochasticity-LH",           0.03,  0.01,      0.1, "all")


# convert Parameters above into data.frame
ps_df <- setNames(data.frame(matrix(ps, nc = 5, nr = 10, byrow = T)[,c(1,5)], apply(matrix(ps, nc = 5, nr = 10, byrow = T)[,2:4], 2, as.numeric), stringsAsFactors = F), c("param", "use_case", "def", "low", "up"))


# set a lower and upper state (s1, s2) for each parameter
param_combs <- ps_df %>% 
  mutate(s1 = (def + low)/2) %>% 
  mutate(s2 = (def + up) / 2) 

# replace default value with s1 and s2 respectively, do not vary paramters that do not affect scenarios with logistic growth
log_growth <- param_combs %>% 
  mutate(s1 = ifelse(use_case != "linear", s1, def)) %>% 
  mutate(s2 = ifelse(use_case != "linear", s2, def)) %>% 
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
log_oat$`disturbance-frequency` <- rep(c(100, 400), each = 20)

log_oat <- log_oat[!duplicated(log_oat),]
# convert the parameter sets to a nlrx-readable format
variables <- as.list(log_oat[31:34,])

variables <- lapply(variables, function(x) list("values" = x))

# create a list of parameters kept constant (remember to specify the output-directory!)
constants <-
list("disturbance-intensity" = 90, 
     "create-output" = "true",
     "save-landscape" = "false", 
     "plot-update-frequency" = 9999999,
     "plot-animals" = "false",
     "plot-landscape" = "false",
     "growth-type" = "\"logistic\"",
     "output-directory" = "\"Y:/Home/milles/proj_POLS2/simulations/2020-08-20/sensitivity_analysis/\"")


dir.create(substr(dirname(constants$`output-directory`),2,999))

# output variables and constants to include them in the supplement
write.table(log_oat, file = here("table", "Supplement", "sensitvity_analysis_variables.txt"), sep = ",", quote = FALSE, row.names = F)

write.table(data.frame(constants)[-8], file = here("table", "Supplement", "sensitvity_analysis_constants.txt"), sep = ",", quote = FALSE, row.names = F)

#write.csv("Y:/Home/milles/proj_POLS2/table/test.csv)", x = constants)

######################################
#### RUN THE SENSITIVITY ANALYSIS ####
######################################

# PATH TO NETLOGO AND THE MODEL 
netlogopath <- "Y:/Home/milles/NetLogo 6.1.1"
modelpath <- file.path("Y:/Home/milles/proj_POLS2/code_proj_ECOEVO/model/ecoevo_model_250x250_red.nlogo")

# setup the nl object
nl <- nl(nlversion = "6.1.1", nlpath = netlogopath, modelpath = modelpath, jvmmem = 4096)

# setup the experiment
nl@experiment <- experiment(expname = "sensitivity_analysis", 
                            outpath = here(),
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 40000,
                            variables = variables, 
                            constants = constants)


# define the simulation design (simple = only constants required)
nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)


# run sensitivity analysis in parallel
plan(multisession)
results <- nlrx::run_nl_all(nl, split = 4)
