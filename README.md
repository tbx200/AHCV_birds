# AHCV_birds

DOI: 10.5281/zenodo.7885604

Formatted data and analysis code for "AREAS OF HIGH CONSERVATION VALUE SUPPORT SPECIALIST FOREST BIRDS"


README for the data and code repository for "AREAS OF HIGH CONSERVATION VALUE SUPPORT SPECIALIST FOREST BIRDS"


This repository includes the formatted data and code needed for the HMSC analysis for the study.

Data
- Bird data from the Swedish Bird Survey between 2010 and 2019
- AHCV status of each observation
- Proportion of each route that was surveyed
- Climate data
- Landcover data
- Coordinates have been removed

Code files were all based on code provided with the Hmsc course (all scripts can be retrieved here: https://www2.helsinki.fi/en/researchgroups/statistical-ecology/hmsc):
- S1_read_data_submission.R - Read the data and check if everything is correct
- S2_fit_models_submission.R - Define and fit the HMSC models
- S3_evaluate_convergence_submission.R - Evaluate and report the model convergence
- S4_compute_model_fit_submission.R - Compute the model fit statistics
- S5_show_model_fit_submission.R - Report the model fit statistics
- S6_show_parameter_estimate_submission.R - Compute and report the variance partitioning
- S7_predictions_submission.R - Use the model to extract the predicted marginal effect of AHCV on the birds.
