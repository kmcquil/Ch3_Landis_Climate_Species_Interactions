# Metadata

## LANDIS-II NECN Model 
We ran LANDIS-II with the NECN extension that has been revised to include species-level transpiration. We included the Landis model and all extensions in the following container. The container can be compiled to a .sif file and then used to run the landscape scenarios. 

Container: landis_necn_scrpple_bioharvest.sdef 

## Models 
All landscape scenarios are included in the ../Models folder. 
- Diagnostics: Includes markdown files showing the model diagnostics for the flux tower and watershed models. 
  
- Future: Includes all scenarios set up to run at watershed scale using future climate. 
  
- Flux_Tower_Diagnostics: Includes the flux tower model run for 50 years using climate randomly sampled from the historical record. This was used for the flux tower diagnostics. 
  
- Flux_Tower_V1_ZR_19012023: Includes the flux tower model run from 1995 - 2021 and is used for calibration against flux tower data. 
  
- Flux_Tower_V1_ZR_19012023_OG: Includes the flux tower model run from 1995 - 2021 using the original version of NECN. 
  
- Landis_WS_Diagnostics: Includes the watershed model run for 50 y ears using climate randomly sampled from the historical record. This was used for the watershed diagnostics. 
  
- Landis_WS_V1_ZR_19012023: Includes the watershed model run from 1995 - 2021 and is used for watershed calibration. 
  
- Landis_WS_V1_ZR_19012023_OG: Includes the watershed model run from 1995 - 2021 using the original version of NECN. 


## Scripts 
Create the figures for model diagnostics 
- model_diagnostics.R

A text file with commands to run each landscape scenario is supplied to the submission script. 
- commands_mdoel_batch.txt 
- model_batch.csh 

Analyzing the results of the multiscale calibration and future scenarios
- make_figures_tables.R
- analyze_future_scenarios.R