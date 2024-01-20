# OFC_midbrain_TMS


# This repository contains data and code for the manuscript "Midbrain signaling of identity prediction errors depends on orbitofrontal cortex networks"

The code has been tested using R (4.1.2)/RStudio (2022.07.1), Matlab (version R2020b, Mathworks Inc).

## SourceData and SourceCode

- All figure panels (in 'Figs' folder) can be replicated by runing the SourceCode using SourceData
- All source codes are written in R, but require some packages to run the code
- The 'Setup.R' code listed all R packages. If any package cannot be loaded, then it needs to be installed by running install.package(package_name)
- The 'Setup.R' code also has some common theme settings for using ggplot2

### Global connectedness analysis
- source data for all 10 ROIs are stored in the same spreadsheet 'GlobalConnectedness.xlsx'
- 10 ROIs are: Targeted OFC seed and LPFC stimulation site (Fig 2c), two functional OFC and LPFC ROIs (Supplementary Fig 2), and six additional ROIs (Supplementary Fig 3).
- running code 'GlobalConn.R'

### Behavioral analysis
- running code 'Behavior.R' (Fig 4)
- Fig 4c,d,e require Behavioral modeling results (see below)

### Identity prediction error
- We have separate code and data for analyses in different ROIs:
1. midbrain and LPFC: code 'iPE.R' (Fig 5b,c, Supplementary Fig 5b,c)
2. left OFC: code 'iPE_lOFC.R' (Supplementary Fig 5d)
3. additional ROIs: code 'iPE_others.R' (Supplementary Fig 6)

### Multivariate pattern similarity analysis
- We have included code for running this analysis on the lateral OFC functional ROI 'MVPA.R' (Fig 6d)

### Simulations of the learning model
- Illustration of the learning model using different learning rates: 'Beh_Simu_illustration.R' (Fig3a)
- Simulations of behavioral accuracy, identity prediction errors, and pattern similarity with different learning rates: 'Beh_Simu_varied_paras.R' (Fig 3b,c; Fig 6b)

### Summary of screening data
- code 'summary_screening.R' (Supplementary Fig 1)

### Motion parameters
- code 'CompareMotion.R' (Supplementary Fig 4)
- plot the change of motion across time and test if motion can explain away the TMS effect on global connectedness

## Behavioral modeling folder
- two learning model variants: diff & same
- model parameters estimated: mu, alpha, theta
- Folder 'JagsSamples' contains the posterior estimates of each model after running 'Run_Model_jags.R' code

## GlobalConnectedness folder
- contains matlab code for (1) extracting and filtering functional imaging data, (2) conducting global connectedness calculation




