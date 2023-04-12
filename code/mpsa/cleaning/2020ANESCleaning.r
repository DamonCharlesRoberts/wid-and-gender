# Title: 2020 ANES Cleaning

# Notes:
    #* Description: 
        #** Cleaning script for the 2020 ANES
    #* Updated:
        #** 2023-04-06
        #** dcr

# Setup
    #* Load functions
box::use(
    data.table[...]
)
    #* Create list object to store data
Anes2020 <- list()
    #* Load original dataset
Anes2020[["original"]] <- setDT(
    read.csv(
        file = "../../data/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"
    )
)

# Cleaning
Anes2020[["cleaned"]] <- Anes2020[["original"]][
    #* Filter to include only White respondents
    V201549x == 1
][
    #* wid - How important is being white to identity
        #** <= 0 - NA, 1 - Extremely Important to 5 - Not at all important
        #** NA - NA, 1 - Not at all important to 5 - Extremely Important
    ,wid := fcase(
        V202499x == 1, 5,
        V202499x == 2, 4,
        V202499x == 3, 3,
        V202499x == 4, 2,
        V202499x == 5, 1
    )
][
    #* pid - Partisan identification
        #** <= 0 - NA, 1 - Strong Democrat to 7 - Strong Republican
        #** NA - NA, 1 - Strong Democrat to 7 - Strong Republican
    ,pid := fcase(
        V201231x > 0, V201231x
    )
][
    #* female - Self-identified gender of respondent
        #** 1 = Male, 2 = Female
        #** 0 = Male, 1 = Female
    ,female := fcase(
        V201600 == 2, 1,
        V201600 == 1, 0
    )
][
    #* workway - Blacks should work way up
        #** <= 0 - NA, 1 - Agree Strongly to 5 - Disagree strongly
        #** Average of: NA - NA, 1 - Disagree Strongly to 5 - Agree Strongly
    ,work := fcase(
        V202300 == 1, 5,
        V202300 == 2, 4,
        V202300 == 3, 3,
        V202300 == 4, 2,
        V202300 == 5, 1
    )
][
    #* generations - slavery has made it difficult
        #** <= 0 - NA, 1 - Agree Strongly to 5 - Disagree strongly
        #** NA - NA, 1 - Agree Strongly to 5 - Disagree strongly
    ,generations := fcase(
        V202301 > 0, V202301
    )
][
    #* deserve - Blacks have gotten less than they deserve
        #** <= 0 - NA, 1 - Agree strongly to 5 - Disagree strongly
        #** NA - NA, 1 - Agree Strongly to 5 - Disagree strongly
    ,deserve := fcase(
        V202302 > 0, V202302
    )
][
    #* try - Blacks must try harder to get ahead
        #** <= 0 - NA, 1 - Strongly agree to 5 - Disagree strongly
        #** NA - NA, 1 - Agree strongly to 5 - Disagree strongly
    ,try := fcase(
        V202303 > 0, V202303
    )
][
    #* raceResent - Racial resentment; Average of 4 questions
        #** Average of: NA - NA, 1 - Low to 5 - High
    ,raceResent := rowMeans(
        .SD,
        na.rm = TRUE
    )
    ,.SDcols = c("work", "generations", "deserve", "try")
]
