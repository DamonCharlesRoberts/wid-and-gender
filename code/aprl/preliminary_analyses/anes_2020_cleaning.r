# Title: 2020 ANES Cleaning for Gender and White identity project

# Notes:
    #* Description: R Script to do the cleaning for the 2020 ANES 
    #* Updated: 2022-08-22
    #* Updated by: dcr

# Setup
    #* Modularly load functions
box::use(
    dplyr = dplyr[filter, mutate,]
)
    #* Create list object
anes_2020 = list()
    #* Load dataset
anes_2020[['original']] = read.csv('../data/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv')

# Clean the data
anes_2020[['cleaned']] = anes_2020[['original']] |>
    #* Filter only white respondents
    filter(V201549x == 1) |>
    mutate(
        #* wid - How important is being white to identity
            #** 1 = Extremely important - 5 = Not at all important
            #** RECODE TO: 1 = Not at all important - 5 = Extremely important
        wid = ifelse(V202499x == 1, 5,
                ifelse(V202499x == 2, 4,
                    ifelse(V202499x == 3, 3,
                        ifelse(V202499x == 4, 2,
                            ifelse(V202499x == 5, 1, NA))))),
        #* pid - Partisan identification
            #** 1 = Strong democrat - 7 = Strong republican
        pid = ifelse(V201231x <= 0, NA, V201231x),
        #* female - Respondent sex
            #** 1 = Male, 2 = Female
            #** RECODE TO: 0 = Male, 1 = Female
        female = ifelse(V201600 == 1, 0, 
                    ifelse(V201600 == 2, 1, NA)),
        #* age - Respondent age
        age = ifelse(V201507x <= 0, NA, V201507x),
        #* edu - Education
            #** 1 = < high school, 2 = HS, 3 = Some post-HS, 4 = Bachelors, 5 = Graduate
        edu = ifelse(V201511x <= 0, NA, V201511x),
        #* gender_discrim - How much discrimination has R faced because of gender?
            #** coded as: 1 = A great deal, 5 = None at all
            #** recoded to: 1 = none at all, 5 = A great deal
        gender_discrim = ifelse(V202538 == 1, 5,
                            ifelse(V202538 == 2, 4,
                                ifelse(V202538 == 3, 3,
                                    ifelse(V202538 == 2, 4,
                                        ifelse(V202538 == 1, 5, NA))))),
        #* conserv_media - Whether R indicates they visit Fox or Brietbart regularly
            #* coded as: two measures of whether they mentioned that they visit a news site regularly or not
            #* recoded to: 0 = did not mention Fox OR Brietbart, 1 = did mention Fox OR Brietbart
        conserv_media = ifelse(V201634f == 1 | V201634e == 1, 1,
                            ifelse(V201634f != 1 & V201634f != 1, 0, NA))
    )