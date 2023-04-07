# title: What's up with White women? 
# data: ANES
# author: komal 

# install libraries
library(pacman)
p_load(foreign, haven, tidyverse, stargazer, lmtest, gplots, cowplot, jtools, estimatr, modelsummary,staggered, ggeffects, ordinal, mediation, Hmisc, MASS)

# load data
anes_2020 <- read.csv('C://Users//kpk03//Desktop//Dropbox//kk_dcr_wid_gender//data//anes_timeseries_2020_csv_20220210//anes_timeseries_2020_csv_20220210.csv')

anes_2016 <- read_dta('C://Users//kpk03//Desktop//Dropbox//kk_dcr_wid_gender//data//anes_timeseries_2016_dta//anes_timeseries_2016.dta')

#* mutate variables ----
anes_2020 <- anes_2020 %>% 
  dplyr::filter(V201549x == 1) %>% 
  dplyr::mutate(
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
    dem = ifelse(pid %in% c("1", "2", "3"), 1, 0),
    income = ifelse(V202468x <=0, NA, V202468x),
    liberal = ifelse(V201200 %in% c("1","2", "3"), 1,
                     ifelse(V201200 %in% c("5","6","7"), 0, NA)),  #liberal=1,else 0, moderate not considered
    south = ifelse(V201575 %in% c("1", "5", "10", "11", "12", "13", "21", "22", "24", "28", "37", "40", "45", "47", "48","51", "54"), 1, 0),
    egal = ifelse(V202260 ==1, 0,
                  ifelse(V202260 ==2, 0.25,
                         ifelse(V202260==3,0.5,
                                ifelse(V202260==4,0.75,
                                       ifelse(V202260==5,1,NA))))), #higher values, more egalitarian views
    V202300r <- ifelse(V202300 ==1, 0,
                       ifelse(V202300 ==2, 0.25,
                              ifelse(V202300==3,0.5,
                                     ifelse(V202300==4,0.75,
                                            ifelse(V202300==5,1,NA))))),
    V202301r <- ifelse(V202301 ==1,1,
                       ifelse(V202301==2,0.75,
                              ifelse(V202301==3,0.5,
                                     ifelse(V202301==4,0.25,
                                            ifelse(V202301==5,0, NA))))),
    V202302r <- ifelse(V202302 ==1,1,
                       ifelse(V202302==2,0.75,
                              ifelse(V202302==3,0.5,
                                     ifelse(V202302==4,0.25,
                                            ifelse(V202302==5,0, NA))))),
    V202303r <- ifelse(V202303 ==1,0,
                       ifelse(V202303==2,0.25,
                              ifelse(V202303==3,0.5,
                                     ifelse(V202303==4,0.75,
                                            ifelse(V202303==5,1,NA))))),
    resent=(V202300r+V202301r+V202302r+V202303r)/4, #higher values, more racial resentment
    ft_blm <- ifelse(V202174 <=0, NA, 
                      ifelse(V202174 %in% c(998:999), NA, V202174))
                ) %>%
  mutate_each_(funs(factor(.)), c("dem", "female", "liberal", "south"))


# EDA
m1 <- lm(as.numeric(wid) ~ female, anes_2020)
m2 <- lm(wid ~ female + liberal, anes_2020)
m3 <- lm(wid ~ female + age, anes_2020)
m4 <- lm(wid ~ female + age + edu, anes_2020)
m5 <- lm(wid ~ female + age + edu + south, anes_2020)
m6 <- lm(wid ~ female + age + edu + south + liberal, anes_2020)
m7 <- lm(as.numeric(wid) ~ female*age + edu + south + liberal, anes_2020)
m8 <- lm(wid ~ female*V202534 + age + edu + liberal, anes_2020)
stargazer(m1, m2,m3,m4,m5,m6,m7,m8,type="text")

x1 <- lm(wid ~ female*dem, anes_2020)
p <- ggpredict(x1, c("female", "dem"))
plot(p)

o1 <- polr(wid ~ female, anes_2020,Hess = T)
summary(o1)
o2 <- polr(wid ~ female + liberal + age + edu + south, anes_2020,Hess = T)
summary(o2)
ci = confint.default(o2)
exp(cbind(OR = coef(o2), ci)) #all else equal, the odds of women being more likely to subscribe to wid is 1.27 times that of women.

m2 <- lm(V202534 ~ female + dem + age + edu + south, anes_2020)
m3 <- lm(wid ~ female + dem + age + edu + south + V202534, anes_2020)
med1 <- mediate(m2, m3, treat = "female", mediator = "V202534",robustSE = TRUE, sims = 1000, dropobs = T)
summary(med1)

ggplot(anes_2020, aes(x=resent, fill=female))+geom_histogram()
cor(anes_2020$wid, anes_2020$V202534, use = "complete.obs")


dem_2020 <- anes_2020 %>% 
  filter(dem == 1)

m1 <- lm(as.numeric(wid) ~ female, dem_2020)
m2 <- lm(wid ~ female + liberal, dem_2020)
m3 <- lm(wid ~ female + age, dem_2020)
m4 <- lm(wid ~ female + age + edu, dem_2020)
m5 <- lm(wid ~ female + age + edu + south, dem_2020)
m6 <- lm(wid ~ female + age + edu + south + liberal, dem_2020)
m7 <- lm(as.numeric(wid) ~ female + age + edu + south + liberal, dem_2020)
stargazer(m1, m2,m3,m4,m5,m6,m7,type="text")

m2 <- lm(V202534 ~ female + age + edu + south, dem_2020)
m3 <- lm(wid ~ female*V202534 + age + edu + south, dem_2020)
med1 <- mediate(m2, m3, treat = "female", mediator = "V202534",robustSE = TRUE, sims = 1000, dropobs = T)
summary(med1)

rep_2020 <- anes_2020 %>% 
  filter(dem == 0)

m1 <- lm(as.numeric(wid) ~ female, rep_2020)
m2 <- lm(wid ~ female + liberal, rep_2020)
m3 <- lm(wid ~ female + age, rep_2020)
m4 <- lm(wid ~ female + age + edu, rep_2020)
m5 <- lm(wid ~ female + age + edu + south, rep_2020)
m6 <- lm(wid ~ female + age + edu + south + liberal, rep_2020)
m7 <- lm(as.numeric(wid) ~ female*age + edu + south + liberal, rep_2020)
stargazer(m1, m2,m3,m4,m5,m6,m7,type="text")    

m2 <- lm(V202534 ~ female + age + edu + south, rep_2020)
m3 <- lm(wid ~ female*V202534 + age + edu + south, rep_2020)
med1 <- mediate(m2, m3, treat = "female", mediator = "V202534",robustSE = TRUE, sims = 1000, dropobs = T)
summary(med1)

table(anes_2020$V201601, anes_2020$V201600)
