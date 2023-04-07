
# Title: 2020 CCES variables for Gender and White identity project

# Notes:
#* Updated: 2022-10-18
#* Updated by: dcr

# Load modularly load functions
box::use(
    haven = haven[read_dta],
    dplyr = dplyr[filter, mutate, case_when]
)

# Create empty list object for datasets
cces_2020 <- list()

# Load data
cces_2020[['Original']] <- read_dta("data/cces/cces_2020/CES20_Common_OUTPUT_vv.dta")


# Cleaning
cces_2020[['Clean']] <- cces_2020[['Original']] |>
    #* Filter to only include white respondents
    filter(race == 1)
    mutate(
        #* gender
        #** coded as 1 = male,  2 = female
        #** recoded as female - 0 = male, 1 = female, NA = all else
        female = case_when(gender == 1 ~ 0,
                            gender == 2 ~ 1),
        
        #* CC20_340a = 7-point ideology scale
        #** coded as 1=very liberal, 2=liberal, 3=somewhat liberal, 4=middle of the road, 5=somewhat conservative, 6=conservative, 7=very conservative, 8=not sure 
        #** recoded as ideo - 1 = very liberal, 2 = liberal, 3 = somewhat liberal, 4 = middle of the road/not sure, 5 = somewhat conservative, 6 = conservative, 7 = very conservative, NA = all else
        ideo = case_when(CC20_340a == 1 ~ 1,
                            CC20_340a == 2 ~ 2,
                            CC20_340a == 3 ~ 3,
                            CC20_340a == 4 ~ 4,
                            CC20_340a == 5 ~ 5,
                            CC20_340a == 6 ~ 6,
                            CC20_340a == 7 ~ 7,
                            CC20_340a == 8 ~ 4),
        
        #* pid7 = 7-scale partisanship 
        #** coded as 1=strong dem, 2=not very strong dem, 3=the dem party (shown if pid3 is 3,4,5), 4=neither, 5= the rep party (shown if pid3 is 3,4,5), 6 = not very strong republican, 7=strong rep, 8=not sure, 9 = don't know
        #** recoded as pid - 1 = strong dem, 2 = not very strong dem, 3 = lean dem, 4 = independent, 5 = lean rep, 6 = not very strong rep, 7 = strong rep, NA = all else
        pid = case_when(pid_7 == 1 ~ 1,
                        pid_7 == 2 ~ 2,
                        pid_7 == 3 ~ 3,
                        pid_7 == 4 ~ 4,
                        pid_7 == 5 ~ 5,
                        pid_7 == 6 ~ 6,
                        pid_7 == 7 ~ 7),
        
        #* birthyr - year in which you were born (in case we need age in our models)
            #* coded as: year in which you were born
            #* recoded as age - 2020 - birthyr 
        age = (2020-birthyr),
        
        #*seeself_race01 = Some social categories may be more important to the way you see yourself than others. 
        #** coded as 1= Not at all important to who I am, and 7= extremely important to who I am
        #** recoded as 1 = Not at all important to who I am to 7 = extremely important to who I am
        
        wid = case_when(seeself_race01 == 1 ~ 1,
                        seeself_race01 == 2 ~ 2,
                        seeself_race01 == 3 ~ 3,
                        seeself_race01 == 4 ~ 4,
                        seeself_race01 == 5 ~ 5,
                        seeself_race01 == 6 ~ 6,
                        seeself_race01 == 7 ~ 7)
    
    )

# Variables to consider cleaning later

#** CC20_440a = White people in the U.S. have certain advantages because of the color of their skin.
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly disagree
#** recoded as 1 = strongly disagree, 2 = somewhat disagree, 3 = neither agree nor disagree, 4 = somewhat agree, 5 = strongly agree


#* CC20_441_ =  Irish, Italians, Jewish and many other minorities overcame prejudice and worked their way up. Blacks should do the same without any special favors.
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly agree
#** recoded as

#* CC20_441b = Generations of slavery and discrimination have created conditions that make it difficult for blacks to work their way out of the lower class.
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly agree
#** recoded as

#*CC20_441e = I resent when Whites deny the existence of racial discrimination. 
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly agree
#** recoded as

#*CC20_441f = Whites get away with offenses that African Americans would never get away with.
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly agree
#** recoded as

#*CC20_441g = Whites do not go to great lengths to understand the problems African Americans face.
#** coded as 1=strongly agree, 2=somewhat agree, 3=neither agree nor disagree, 4=somewhat disagree, 5=strongly agree
#** recoded as

################################ potential suitable variables from Baird's data ---------------------



#*seeself_pol01
#** coded as 1= Not at all important to who I am, and 7= extremely important to who I am
#** recoded as

#*link_race01 = linked fate
#** coded as 1= Not at all likely, and 7= extremely likely
#** recoded as

#*link_pol01 = linked fate
#** coded as 1= Not at all likely, and 7= extremely likely
#** recoded as