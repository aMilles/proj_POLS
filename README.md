# Fluctuations in density-dependent selection drive the evolution of a pace-of-life-syndrome in populations

## Purpose of this project
The purpose of this project is to explore three predictions made by Wright et al. (2019) on density-dependent selection of different paces of life (POL). The addressed predictions state that, (H1) selected behavioural and life history traits covary with the POL along different population densities (intra-population) and degrees of fluctuation in population density (inter-population) forming a POLS due to density-dependent selection, (H2) the degree of variation in behavioural and life history traits covaries with the POL as it is linked to the degree of fluctuations in population density, and (H3) density-dependent selection along the POLS is reflected in a trade-off between $r_{0}$ and $\gamma$, i.e. $r_{0}$ and $\gamma$ covary with the POL.


## Content of this repository
This repository contains the model as well as the R-Script neccessary to reproduce all results in the main text, supplement as well as figures in the ODD protocol.

In order to run the the model and the subsequent analyses the following folder structure is neccessary:

"/code": rscripts and model code stored<br/>
"/simulations" output of the NetLogo model and output processed using R programming language (folder will be generated) d<br/>
"/figs", figures generated using R programming language and processed simulation data (folder will be generated) <br/>
"/tables" tables that contain information about the parameterization are generated (folder will be generated) <br/>


## How to generate results
**To replicate all results (including the supplement) published in JOURNAL XY follow these steps:**

0. go to [code/rscript](code/rscript) 
1. check for missing packages and install them by running [1_install_missing_packages.R](code/rscript/1_install_missing_packages.R). This script compares packages needed in this project with packages existing in your library. It does not, however, not compare their versions. In case, any errors occur, make sure you use R 3.6.0 and the lastest releases (2021-04-20).

2. adjust location of NetLogo executable and the number of cores (default is 5 cores/workers used) in [2_run_all_experiments.R](code/rscript/2_run_all_experiments.R). After that, run the script to 1) start experiments (generated via BehaviourSpace within NetLogo) headless via command line and 2) via R (sensitivity analysis) using the nlrx package running [supplement_process_SensitivityAnalysis](code/rscript/process_simulation_output/supplement_process_SensitivityAnalysis")

3. adjust the number of cores (default is 5 cores/workers used)  in [3_process_simulation_data_all_steps_all_experiments.R](code/rscript/3_process_simulation_data_all_steps_all_experiments.R).  This script will run scripts in [process_simulation_output](code/rscript/process_simulation_output")

5. generate the figures with ["4_create_all_Figures"](code/rscript/"4_create_all_Figures"). This will run all scripts located in ["create_supplemental_figures"](code/rscript/create_supplemental_figures") and ["create_main_text_figures"](code/rscript/create_main_text_figures") and generate figures in the respective folders in "figs/YYYY-MM-DD/...".


**If you want to run individual experiments:**

0. make sure the folder strucuture indicated above exists (as scripts that generate these folders may have been skipped with this approach)
1. run experiment with model ("code/model/POLS_model.nlogo")
2. to aggregate data to (intra-)populational level 
      - open process_simulation_output/process_simulation_data_all_steps.R
      - specify the name and folder of the experiment
      - run the script
3  once all data has been processed (takes some time)
      - run scripts in create_main_text_figures to reproduce figures included in the main text
