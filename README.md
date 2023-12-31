
# This repository contains data and code for the manuscript "Midbrain signaling of identity prediction errors depends on orbitofrontal cortex networks"

The code has been tested using R (4.1.2)/ RStudio (2022.07.1), Matlab (version R2020b, Mathworks Inc).

## SourceData and SourceCode

- All figure panels (in 'Figs' folder) can be replicated by running the SourceCode using SourceData.xlsx
- All source codes are written in R, but require some packages to run the code
- The 'Setup.R' code listed all R packages. If any package cannot be loaded, then it needs to be installed by running install.package(package_name)
- The 'Setup.R' code also has some common theme settings for using ggplot2

### Global connectedness analysis
- 10 ROIs are: Targeted OFC seed and LPFC stimulation site (Fig 2c), two functional OFC and LPFC ROIs (Extended Data Fig 2), and six additional ROIs (Extended Data Fig 3).
- running code 'GlobalConn.R'

### Behavioral analysis
- running code 'Behavior.R' (Fig 4)

### Identity prediction error
- We have separate code and data for analyses in different ROIs:
1. midbrain and LPFC: code 'iPE.R' (Fig 5b,c, Extended Data Fig 5b,c)
2. left OFC: code 'iPE_lOFC.R' (Extended Data Fig 5d)
3. additional ROIs: code 'iPE_others.R' (Extended Data Fig 6)

### Multivariate pattern similarity analysis
- We have included code for running this analysis on the lateral OFC functional ROI 'MVPA.R' (Fig 6d)

### Simulations of the learning model
- Learning function for simulating the task: 'Func_Simu.R'
- Illustration of the learning model using different learning rates: 'Beh_Simu_illustration.R' (Fig 3a)
- Simulations of behavioral accuracy, identity prediction errors, and pattern similarity with different learning rates: 'Beh_Simu_varied_paras.R' (Fig 3b,c; Fig 6b)

### Summary of screening data
- code 'summary_screening.R' (Extended Data Fig 1)

### Motion parameters
- code 'CompareMotion.R' (Extended Data Fig 4)
- plot the change of motion across time and test if motion can explain away the TMS effect on global connectedness

## Behavioral modeling folder
- two learning model variants: diff (different learning rate parameters in different TMS sessions) & same (same learning rate in different TMS sessions)
- Folder 'JagsSamples' contains the posterior estimates of each model after running 'Run_Model_jags.R' code

## GlobalConnectedness folder
- contains matlab code for (1) extracting and filtering functional imaging data, (2) conducting global connectedness calculation
- 'masks' folder contains gray matter, white matter, and CSF masks used in the analysis




