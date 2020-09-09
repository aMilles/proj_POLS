# proj_POLS
NetLogo Model and RScripts to analyse predictions of the density-dependence POLS framework

In code_proj_ECOEVO, rscripts and model code are stored<br/>
In simulations, output of the NetLogo model is stored<br/>
In figs, output of the R scripts using simulation output is stored<br/>
In tables, table output of the R scripts using simulation output is stored<br/>


To replicate the results published in ... 

1. run all experiments saved in the NetLogo-Model as well as the rscript "supplement_process_sensitivity_analysis_for_SX05.R" if a one-at-a-time sensitivity analysis is also required. Please consider:<br/>
     - the sensitivity analysis script runs NetLogo from R and requires an installation of NetLogo 6.1.1 in the parent folder of the project or and adjustment of the path to the NetLogo installation<br/>
      - create the respective folders in the simulations folder like "simulations/YYYY-MM-DD/Main_Predictions/" manually<br/>
      - adjust the output-directory of the Experiments to your folder structure
  
2. run the "process_simulation_data_all_steps.R" script in "code_proj_ECOEVO\rscript\process_simulation_output" for all experiments except for "Landscape_fluctuations_Example" "test_growth_factor_logistic" and "test_harvest_rate_curvature" (processing is not required here, as only "SIMID_ls.csv"-files are used which do not require this postprocessing. 

3. run the "supplement_process_landscape_metrics_for_SX10.R" script in "code_proj_ECOEVO\rscript\process_simulation_output" to postprocess the "Landscape_fluctuations_Example"-experiment.

4. run the "create_all_Figures.R" script to generate the result and supplemental figures. Consider that: <br/>
    - "sim.date" needs to be respecified if it deviates from the default setting
    - the "create_all_Figures.R" script runs all scripts in the create_main_text_figures and create_supplemental_figures folder
    
