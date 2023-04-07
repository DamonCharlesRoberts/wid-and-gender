# Title: 2016 ANES Cleaning

# Notes:
    #* Description: 
        #** Cleaning script for the 2016 ANES
    #* Updated:
        #** 2023-04-06
        #** dcr

# Setup
    #* Load functions
box::use(
    haven[read_dta],
    data.table[...]
)
    #* Create list object to store data
Anes2016 <- list()
    #* Load original dataset
Anes2016[["original"]] <- setDT(
    read_dta(
        file = "../../data/anes_timeseries_2016_dta/anes_timeseries_2016.dta"
    )
)

# Cleaning
Anes2016[["cleaned"]] <- Anes2016[["original"]][
    #* Filter to include only White respondents
    V161310x == 1
][
    #* wid - How important is being white to identity
        #** <= 0 - NA, 1 - Extremely Important to 5 - Not at all important
        #** NA - NA, 1 - Not at all important to 5 - Extremely Important
    ,wid := fcase(
        V162327 == 1, 5,
        V162327 == 2, 4,
        V162327 == 3, 3,
        V162327 == 4, 2,
        V162327 == 5, 1
    )
][
    #* pid - Partisan identification
        #** <= 0 - NA, 1 - Strong Democrat to 7 - Strong Republican
        #** NA - NA, 1 - Strong Democrat to 7 - Strong Republican
    ,pid := fcase(
        V161158x > 0, V161158x
    )
][
    #* female - Self-identified gender of respondent
        #** 1 = Male, 2 = Female
        #** 0 = Male, 1 = Female
    ,female := fcase(
        V161342 == 2, 1,
        V161342 == 1, 0
    )
][
    #* workway - Blacks should work way up
        #** <= 0 - NA, 1 - Agree Strongly to 5 - Disagree strongly
        #** Average of: NA - NA, 1 - Disagree Strongly to 5 - Agree Strongly
    ,work := fcase(
        V162211 == 1, 5,
        V162211 == 2, 4,
        V162211 == 3, 3,
        V162211 == 4, 2,
        V162211 == 5, 1
    )
][
    #* generations - slavery has made it difficult
        #** <= 0 - NA, 1 - Agree Strongly to 5 - Disagree strongly
        #** NA - NA, 1 - Agree Strongly to 5 - Disagree strongly
    ,generations := fcase(
        V162212 > 0, V162212
    )
][
    #* deserve - Blacks have gotten less than they deserve
        #** <= 0 - NA, 1 - Agree strongly to 5 - Disagree strongly
        #** NA - NA, 1 - Agree Strongly to 5 - Disagree strongly
    ,deserve := fcase(
        V162213 > 0, V162213
    )
][
    #* try - Blacks must try harder to get ahead
        #** <= 0 - NA, 1 - Strongly agree to 5 - Disagree strongly
        #** NA - NA, 1 - Agree strongly to 5 - Disagree strongly
    ,try := fcase(
        V162214 > 0, V162214
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
