# Title: 2016 ANES cleaning for Gender and White identity project

# Notes:
    #* Description: R script for cleaning the 2016 ANES
    #* Updated: 2022-08-25
    #* Updated by: dcr 

# Setup
    #* modularly load functions
box::use(
    haven = haven[read_dta],
    dplyr = dplyr[filter, mutate, case_when, rename, select, select_if]
)
    #* create list object
anes_2016 = list()
    #* load dataset
anes_2016[['original']] = read_dta('../data/anes_timeseries_2016_dta/anes_timeseries_2016.dta')

# Cleaning
anes_2016[['cleaned']] = anes_2016[['original']] |>
    #* Filter only white respondents
    filter(V161310x == 1) |>
    mutate(
    #* wid - How important is being white to identity
        #** 1 = Extremely important - 5 = Not at all important
        #** RECODE TO: 1 = Not at all important - 5 = Extremely important
    wid = ifelse(V162327 <= 0, NA, V162327),
    wid = case_when(V162327 == 1 ~ 5,
                         V162327 == 2 ~ 4,
                         V162327 == 3 ~ 3,
                         V162327 == 4 ~ 2,
                         V162327 == 5 ~ 1),
    #* pid - Partisan identification
        #** 1 = Strong Democrat - 7 = Strong Republican
    pid = ifelse(V161158x <= 0, NA, V161158x),
    #* ideo - Ideology
        #** 1 = Extremely liberal - 7 Extremely conservative
    ideo = ifelse(V161126 <= 0, NA, V161126),
    #* female - Is respondent female or male
        #** 1 = Male, 2 = Female
        #** RECODE TO: 0 = Male, 1 = Female
    female = ifelse(V161342 == 2, 1,
                         ifelse(V161342 == 1, 0,
                                ifelse(V161342 == 3, 0, NA))),
    #* age - Age
        #** Continuous variable of age
    age = ifelse(V161267 <= 0, NA, V161267),
    #* edu - Education
        #** 1 = < high school, 2 = HS, 3 = Some post-HS, 4 = Bachelors, 5 = Graduate
    edu = ifelse(V161270 <= 0, NA, V161270),
    #* gender_discrim - Discrimination in US against women
        #** coded as: 1 = a great deal, 5 = none at all
        #** RECODE TO: 1 = none at all, 5 = a great deal
    gender_discrim = ifelse(V162362 == 1, 5,
                        ifelse(V162362 == 4, 2,
                            ifelse(V162362 == 3, 3,
                                ifelse(V162362 == 2, 4, 
                                    ifelse(V162362 == 1, 5, NA))))),
    #* conserv_media - Whether R indicates they visit Fox regularly
        #** coded as: 0 = not selected, 1 = selected
    conserv_media = ifelse(V161452 >= 0, V161452, NA)
    ) |>
select(wid, pid, female, age, edu, ideo, gender_discrim, conserv_media)
#select(-c(version, V160001, V160001_orig, V160101, V160101f, V160101f, V160102, V160102f, V160102w, V160201, V160201f, V160201w, V160202, V160202f, V160202w, V160502, V160501, V161001, V161002, V161010a:V161023, V161025, V161026:V161063, V161069, V161070a, V161072, V161073a, V161075, V161076a, V161078, V161079a, V161080, V161080a, V161083, V161083a, V161084, V161084a, V161085, V161085a, V161098, V161099a, V161101, V161102a, V161104, V161105a, V161107, V161108a, V161114a, V161127:V161135b, V161114b, V161138a, V161138b, V161140, V161140a, V161141, V161141a, V161142, V161142a, V161150a:V161151b, V161155:V161157, V161193, V161193a, V161195, V161195a, V161196, V161193a, V161204a, V161204a, V161204b, V161213, V161213a, V161214, V161214a, V161224, V161224a, V161226, V161226a, V161227, V161227a, V161228, V161228a, V161229, V161229a, V161233, V161233a, V161235, V161235a, V161238:V161240b, V161245:V161263, V161266a:V161266p, V161267a, V161267b, V161267c, V161267x, V161274b, V161275x, V161259, V161260, V161267a, V161267b, V161274b, V161276x:V161301b, V161308x, V161310a:V161310f, V161310x, V161311, V161312a, V161313, V161314, V161314a, V161318, V161319, V161320, V161321, V161322, V161323, V161325a:V161325e, V161331a:V161331b, V161332, V161333, V161335a, V161338, V161342, V161342a, V161351:V161521, V162001, V162014b, V162016b, V162021a:V162028, V162028, V162030, V162031, V162033, V162033a, V162034:V162057, V162070a:V162076b,V162114:V162114b, V162116a:V162125, V162146, V162147, V162135, V162136, V162149, V162150:V162155b, V162161, V162162, V162176, V162176a, V162180, V162180a, V162188, V162188a, V162189, V162189a, V162191, V162191a, V162193, V162193a, V162229a:V162229b, V162230a, V162230b, V162231a, V162231b, V162238a, V162238b, V163001a:V168528)) |>
#    select_if(~ (is.numeric(.x) | is.integer(.x)))