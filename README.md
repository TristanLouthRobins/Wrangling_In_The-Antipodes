# Wrangling In The Antipodes (WITA)
## https://wranglingintheantipodes.wordpress.com/

### WITA is a blog maintained by Tristan Louth-Robins to explore acoustic ecology with data science tools. 

This repo includes code, some datasets and other resources relevant to research covered on the blog. *Not everything published to the blog is featured here yet, I'm in the process of doing this though.* 

--- Latest update: 20/10/2022 --- 

### "Preparing big audio files for analysis" - published 20/10/2021
URL: https://wranglingintheantipodes.wordpress.com/2021/10/20/preparing-big-audio-files-for-analysis/

WITA_08-label_data.R 
- R code for generating label data in conjunction with Audacity.

### "NDSI: parameters in context" - published 9/4/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/04/09/ndsi-parameters-in-context/

WITA_10-NDSI_params_in_context.R
- R code for exploring NDSI data within a research context. The code permits the user to recreate the EDA and visualisations featured in the published post.
WITA_10-dataset.csv
- Dataset to be used in conjunction with the above R code.

### "Acoustic detection in monitoR – an overview" - published 10/8/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/08/10/acoustic-detection-in-monitor-an-overview/

WITA_11-intro_to_monitoR.R
- R code for using `monitoR` package to generate correlation templates and perform analysis. **Note: acoustic data is not provided and the user will having to supply their own data.**

## AudioMoth - General code

WITA_G-audiomoth_compute_indices_and_tidy_data.R (Version 2.4 - October 2022)
- R code which can be used in conjunction with AudioMoth acoustic data. Contains functions which will perform the following workflow:
1) Compute user-defined acoustic indicies specific to the R package `acousticecology` and export the results as a .csv dataset.
2) Tidy data function and call to import the .csv dataset and tidy/factorise variable data.
3) Write tidy data to updated .csv file for subsequent EDA.

Current issues: there's a couple of bugs with the factorisation I haven't resolved yet. See Line 123. 


