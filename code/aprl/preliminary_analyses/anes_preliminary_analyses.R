# Title: Preliminary analyses of ANES for White identity and gender project

# Notes:
    #* Description: Rscript to perform the exploratory analyses of the 2016 and 2020 ANES for the white identity and gender project
    #* Updated: 2022-08-25
    #* Updated by: dcr 
    #* Comments: will need to clean the ideological identification variable (7-point) too. Code I have doesn't already do that for you.

# Setup
    #* Make sure you have installed the packages needed for this analysis. Uncomment and run those you don't have 
# install.packages('box')
# install.packages('dplyr')
# install.packages('modelsummary')
    #* Clean the data sets
        #** 2016  ANES
source('code/preliminary_analyses/anes_2016_cleaning.r')
        #** 2020 ANES
source('code/preliminary_analyses/anes_2020_cleaning.r')
    #* Modularly load the functions we'd need for analyses
box::use(
    dplyr = dplyr[select, rename],
    modelsummary = modelsummary[datasummary_skim]
)

# Analyses


# Tables
    #* Descriptive statistics
        #** 2016 ANES
anes_2016[['cleaned']] |>
    select(wid, female, pid) |>
    rename(`White identity` = wid,
            `Female` = female,
            `Party ID` = pid) |>
    datasummary_skim(histogram = FALSE, title = '2016 Descriptive statistics \\label{tab:descriptive_stats_2016}', notes = 'Data source: 2016 American National Election Study.')
        #** 2020 ANES
anes_2020[['cleaned']] |>
    select(wid, female, pid) |>
    rename(`White identity` = wid,
            `Female` = female,
            `Party ID` = pid) |>
    datasummary_skim(histogram = FALSE, title = '2020 Descriptive statistics \\label{tab:descriptive_stats_2020}', notes = 'Data source: 2020 American National Election Study.', output = 'figures/preliminary_analyses/2020_descriptive_statistics.tex')
