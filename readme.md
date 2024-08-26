This repository contains all code and data needed to replicate 'Police use of force killings in the US have increased and exposure remains unequal' by Frank Edwards and Ryan Brown. Data are derived from Mapping police violence (https://mappingpoliceviolence.org/); SEER Population data (https://seer.cancer.gov/popdata/download.html); and CDC WONDER Underlying Cause of Death (https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html). 

make_all.R is the primary make script for the file. After uncommenting lines 3-8, you can execute all code needed to estimate models used in the paper. 

- QA.R deduplicates and cleans mapping police violence data and outputs data ready for multiple imputation
- impute.R conducts and outputs multiple imputations of MPV data
- models.R estimates and outputs models for life table estimation. Note that this script is computationally intensive and may be best to run remotely
- make_bsts.R estimates and outputs Bayesian structural time series models over imputed data
- make_causalEst.R estimates and outputs BSTS models with regression predictors
- make_posterior_preds.R processes the output of models.R and outputs posterior predictions
- make_table scripts contain functions to produce multiple decrement life tables
- read_nat.R reads and pre-processes all study data
- make_figures.r makes study visuals
- appx.r makes appendix visuals