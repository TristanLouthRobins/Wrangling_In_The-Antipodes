# Wrangling In The Antipodes (WITA)
## https://wranglingintheantipodes.wordpress.com/

**WITA is a blog maintained by Tristan Louth-Robins to explore acoustic ecology with data science tools.**

### Index
- (14. Introduction to the Acoustic Diversity Index)[#14. Introduction to the Acoustic Diversity Index]

This repo includes scripts, some datasets and other resources relevant to research covered on the blog. *Not everything published to the blog is featured here yet, I'm in the process of doing this though.* 

--- Latest update: 3/7/2025 ---

#### 14. Introduction to the Acoustic Diversity Index
URL: https://wranglingintheantipodes.wordpress.com/](https://wranglingintheantipodes.wordpress.com/2024/03/15/introduction-to-the-acoustic-diversity-index-adi/

#### "13. Return to Lady Bay: Part 1 - Tides, seagrass and acoustic complexity" - published 14/04/2023
URL: https://wranglingintheantipodes.wordpress.com/2023/04/14/return-to-lady-bay-part-1-tides-and-seagrass-exploration/

WITA_13-EDA_pt1.R
- R script using `tidyverse`, `ggplot` and related package to analyse tidied dataset of pre-processed Acoustic Complexity (ACI) values from deployed AudioMoth in reef location. Analysis explores the acoustic complexity values present in a seagrass meadow located in a shallow tidal pool. The script for pre-processing the acoustic data can be found in the 'AudioMoth - General scripts' section of this README file.**

![WITA_13-patchwork_project](https://user-images.githubusercontent.com/62044678/232259527-899b6ab7-3d50-4de7-9d9c-08524a28961a.png)

#### "12. Acoustic Complexity as an indicator of tidal activity" - published 19/12/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/12/19/acoustic-complexity-as-an-indicator-of-tidal-activity/

WITA_12-EDA_ladybay.R
- R script using `tidyverse`, `ggplot` and related packages to analyse tidied dataset of pre-processed Acoustic Complexity (ACI) values from deployed AudioMoth in reef location. Analysis explores potential association between ACI and tidal activity over 22-hour period of deployment. The dataset can be found in the /datasets directory. The script for pre-processing the acoustic data can be found in the 'AudioMoth - General scripts' section of this README file.**

![wita_12-scatter-smooth](https://user-images.githubusercontent.com/62044678/231952164-102b760e-e0be-46a8-9e7d-d44393b43edd.png)

#### "11. Acoustic detection in monitoR – an overview" - published 10/8/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/08/10/acoustic-detection-in-monitor-an-overview/

WITA_11-intro_to_monitoR.R
- R script for using `monitoR` package to generate correlation templates and perform analysis. **Note: acoustic data is not provided and the user will have to supply their own data.**

#### "10. NDSI: parameters in context" - published 9/4/2022
URL: https://wranglingintheantipodes.wordpress.com/2022/04/09/ndsi-parameters-in-context/

WITA_10-NDSI_params_in_context.R
- R script for exploring NDSI data within a research context. The code permits the user to recreate the EDA and visualisations featured in the published post.
WITA_10-dataset.csv
- Dataset to be used in conjunction with the above R code.

#### "8. Preparing big audio files for analysis" - published 20/10/2021
URL: https://wranglingintheantipodes.wordpress.com/2021/10/20/preparing-big-audio-files-for-analysis/

WITA_08-label_data.R 
- R script for generating label data in conjunction with Audacity.

#### AudioMoth - General scripts

WITA_G-audiomoth_compute_indices_and_tidy_data.R 
Version history:
2.0 - all three steps now in single script.
2.5 - fixed the error in factorisation turning all month variables into a 'Summer' category.
2.6 - cleaned up file import code, cleaner and more efficient.
2.7 - new function for user input of site variable.
2.8 - (9th July 2023): feature to create directory/folder for outputted results
2.81 - (10th July 2023): code tidy
2.9 - (2nd March 2024): code tidy, extensive testing with single datasets and extended params specific to given indices.
2.91 - (8th June 2024): error fix to compute_indices function
3.0 - (14th June 2024): cleaned up script to accomodate more flexible usibility.

- R script which can be used in conjunction with AudioMoth acoustic data. Contains functions which will perform the following workflow:
1) Compute user-defined acoustic indicies specific to the R package `acousticecology` and export the results as a .csv dataset.
2) Tidy data function and call to import the .csv dataset and tidy/factorise variable data.
3) Write tidy data to updated .csv file for subsequent EDA.

26/12/22: No current issues in this script.


