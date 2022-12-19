# Wrangling In The Antipodes (WITA)
## https://wranglingintheantipodes.wordpress.com/

### WITA is a blog maintained by Tristan Louth-Robins to explore acoustic ecology with data science tools. 

This repo includes scripts, some datasets and other resources relevant to research covered on the blog. *Not everything published to the blog is featured here yet, I'm in the process of doing this though.* 

--- Latest update: 20/10/2022 --- 

### "8. Preparing big audio files for analysis" - published 20/10/2021
URL: https://wranglingintheantipodes.wordpress.com/2021/10/20/preparing-big-audio-files-for-analysis/

WITA_08-label_data.R 
- R script for generating label data in conjunction with Audacity.

### "10. NDSI: parameters in context" - published 9/4/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/04/09/ndsi-parameters-in-context/

WITA_10-NDSI_params_in_context.R
- R script for exploring NDSI data within a research context. The code permits the user to recreate the EDA and visualisations featured in the published post.
WITA_10-dataset.csv
- Dataset to be used in conjunction with the above R code.

### "11. Acoustic detection in monitoR – an overview" - published 10/8/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/08/10/acoustic-detection-in-monitor-an-overview/

WITA_11-intro_to_monitoR.R
- R script for using `monitoR` package to generate correlation templates and perform analysis. **Note: acoustic data is not provided and the user will have to supply their own data.**

### "12. Acoustic Complexity as an indicator of tidal activity" - published 19/12/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/12/19/acoustic-complexity-as-an-indicator-of-tidal-activity/

WITA_12-EDA_ladybay.R
- R script using `tidyverse`, `ggplot` and related packages to analyse tidied dataset of pre-processed Acoustic Complexity (ACI) values from deployed AudioMoth in reef location. Analysis explores potential association between ACI and tidal activity over 22-hour period of deployment. The dataset can be found in the /datasets directory. The script for pre-processing the acoustic data can be found in the 'AudioMoth - General scripts' section of this README file.**

## AudioMoth - General scripts

WITA_G-audiomoth_compute_indices_and_tidy_data.R (Version 2.4 - October 2022)
- R script which can be used in conjunction with AudioMoth acoustic data. Contains functions which will perform the following workflow:
1) Compute user-defined acoustic indicies specific to the R package `acousticecology` and export the results as a .csv dataset.
2) Tidy data function and call to import the .csv dataset and tidy/factorise variable data.
3) Write tidy data to updated .csv file for subsequent EDA.

Current issues (20/12/22): there's a couple of bugs with the factorisation of seasons I haven't resolved yet. See Issues log. 


