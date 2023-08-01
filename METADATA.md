# Metadata

## LANDIS-II NECN Model 
We ran LANDIS-II with the NECN extension that has been revised to include species-level transpiration. We included the Landis model and all extensions in the following container. The container can be compiled to a .sif file and then used to run the landscape scenarios. 

Container: landis_necn_scrpple_bioharvest.sdef 

## Models 
All landscape scenarios are included in the ../Models folder. 
- ../Models/Diagnostics includes markdown files showing the model diagnostics for the flux tower and watershed models. 
- ../Models/Future includes all scenarios set up to run at watershed scale using future climate. 
- ../Models/Flux_Tower_Diagnostics includes the flux tower model run for 50 years using climate randomly sampled from the historical record. This was used for the flux tower diagnostics. 
- ../Models/Flux_Tower_V1_ZR_19012023 includes the flux tower model run from 1995 - 2021 and is used for calibration against flux tower data. 
- ../Models/Flux_Tower_V1_ZR_19012023_OG includes the flux tower model run from 1995 - 2021 using the original version of NECN. 
- Landis_WS_Diagnostics includes the watershed model run for 50 y ears using climate randomly sampled from the historical record. This was used for the watershed diagnostics. 
- Landis_WS_V1_ZR_19012023 includes the watershed model run from 1995 - 2021 and is used for watershed calibration. 
- Landis_WS_V1_ZR_19012023_OG includes the watershed model run from 1995 - 2021 using the original version of NECN. 


## Scripts 
Create the figures for model diagnostics 
- ../Scripts/model_diagnostics.R

A text file with commands to run each landscape scenario is supplied to the submission script. 
- ../Scripts/commands_mdoel_batch.txt 
- ../Scripts/model_batch.csh 

Analyzing the results of the multiscale calibration and future scenarios
- ../Scripts/make_figures_tables.R
- ../Scripts/analyze_future_scenarios.R