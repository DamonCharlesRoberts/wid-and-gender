#lucid combined data
#courtney nava, cu boudler

library(foreign)
library(stargazer)
library(ggplot2)
library(gmodels)
library(sjPlot)
library(vtable)
library(psych)

setwd("/Users/cjjhn/Dropbox/Dissertation/data")


combined <- read.csv(file="combinedluciddata.csv", header=TRUE, sep=",")

#higher values mean stronger white identity 

#being white is important
combined$white1<-NA
combined$white1[combined$Identity_1=="Strongly agree"]<-6
combined$white1[combined$Identity_1=="Agree"]<-5
combined$white1[combined$Identity_1=="Somewhat agree"]<-4
combined$white1[combined$Identity_1=="Somewhat disagree"]<-3
combined$white1[combined$Identity_1=="Disagree"]<-2
combined$white1[combined$Identity_1=="Strongly disagree"]<-1
table(combined$white1)

#white people have a lot to be proud of
combined$white2<-NA
combined$white2[combined$Identity_2=="Strongly agree"]<-6
combined$white2[combined$Identity_2=="Agree"]<-5
combined$white2[combined$Identity_2=="Somewhat agree"]<-4
combined$white2[combined$Identity_2=="Somewhat disagree"]<-3
combined$white2[combined$Identity_2=="Disagree"]<-2
combined$white2[combined$Identity_2=="Strongly disagree"]<-1
table(combined$white2)

#whites have a lot in common
combined$white3<-NA
combined$white3[combined$Identity_3=="Strongly agree"]<-6
combined$white3[combined$Identity_3=="Agree"]<-5
combined$white3[combined$Identity_3=="Somewhat agree"]<-4
combined$white3[combined$Identity_3=="Somewhat disagree"]<-3
combined$white3[combined$Identity_3=="Disagree"]<-2
combined$white3[combined$Identity_3=="Strongly disagree"]<-1
table(combined$white3)

#whites unable to find a job bc of minorities
combined$white4<-NA
combined$white4[combined$Identity_4=="Strongly agree"]<-6
combined$white4[combined$Identity_4=="Agree"]<-5
combined$white4[combined$Identity_4=="Somewhat agree"]<-4
combined$white4[combined$Identity_4=="Somewhat disagree"]<-3
combined$white4[combined$Identity_4=="Disagree"]<-2
combined$white4[combined$Identity_4=="Strongly disagree"]<-1
table(combined$white4)

#whites need to work together for laws helping whites
combined$white5<-NA
combined$white5[combined$Identity_5=="Strongly agree"]<-6
combined$white5[combined$Identity_5=="Agree"]<-5
combined$white5[combined$Identity_5=="Somewhat agree"]<-4
combined$white5[combined$Identity_5=="Somewhat disagree"]<-3
combined$white5[combined$Identity_5=="Disagree"]<-2
combined$white5[combined$Identity_5=="Strongly disagree"]<-1
table(combined$white5)

#additive index for white identity, higher values = higher white identity
combined$whiteidentity<- combined$white1+combined$white2+combined$white3+combined$white4+combined$white5
table(combined$whiteidentity)
hist(combined$whiteidentity)

#stereotypes
#higher value means they have a stronger ascription to NEGATIVE stereotypes
combined$lazy<-NA
combined$lazy[combined$Stereotypes_1=="Describes Black Americans extremely well."]<-6
combined$lazy[combined$Stereotypes_1=="Describes Black Americans well."]<-5
combined$lazy[combined$Stereotypes_1=="Describes Black Americans somewhat well."]<-4
combined$lazy[combined$Stereotypes_1=="Describes Black Americans somewhat poorly."]<-3
combined$lazy[combined$Stereotypes_1=="Describes Black Americans poorly."]<-2
combined$lazy[combined$Stereotypes_1=="Describes Black Americans extremely poorly."]<-1
table(combined$lazy)

combined$determined<-NA
combined$determined[combined$Stereotypes_2=="Describes Black Americans extremely poorly."]<-6
combined$determined[combined$Stereotypes_2=="Describes Black Americans poorly."]<-5
combined$determined[combined$Stereotypes_2=="Describes Black Americans somewhat poorly."]<-4
combined$determined[combined$Stereotypes_2=="Describes Black Americans somewhat well."]<-3
combined$determined[combined$Stereotypes_2=="Describes Black Americans well."]<-2
combined$determined[combined$Stereotypes_2=="Describes Black Americans extremely well."]<-1
table(combined$determined)


combined$hardwork<-NA
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans extremely poorly."]<-6
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans poorly."]<-5
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans somewhat poorly."]<-4
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans somewhat well."]<-3
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans well."]<-2
combined$hardwork[combined$Stereotypes_3=="Describes Black Americans extremely well."]<-1
table(combined$hardwork)


combined$depend<-NA
combined$depend[combined$Stereotypes_4=="Describes Black Americans extremely poorly."]<-6
combined$depend[combined$Stereotypes_4=="Describes Black Americans poorly."]<-5
combined$depend[combined$Stereotypes_4=="Describes Black Americans somewhat poorly."]<-4
combined$depend[combined$Stereotypes_4=="Describes Black Americans somewhat well."]<-3
combined$depend[combined$Stereotypes_4=="Describes Black Americans well."]<-2
combined$depend[combined$Stereotypes_4=="Describes Black Americans extremely well."]<-1
table(combined$depend)

combined$lackd<-NA
combined$lackd[combined$Stereotypes_5=="Describes Black Americans extremely well."]<-6
combined$lackd[combined$Stereotypes_5=="Describes Black Americans well."]<-5
combined$lackd[combined$Stereotypes_5=="Describes Black Americans somewhat well."]<-4
combined$lackd[combined$Stereotypes_5=="Describes Black Americans somewhat poorly."]<-3
combined$lackd[combined$Stereotypes_5=="Describes Black Americans poorly."]<-2
combined$lackd[combined$Stereotypes_5=="Describes Black Americans extremely poorly."]<-1
table(combined$lackd)

combined$aggro<-NA
combined$aggro[combined$Stereotypes_6=="Describes Black Americans extremely well."]<-6
combined$aggro[combined$Stereotypes_6=="Describes Black Americans well."]<-5
combined$aggro[combined$Stereotypes_6=="Describes Black Americans somewhat well."]<-4
combined$aggro[combined$Stereotypes_6=="Describes Black Americans somewhat poorly."]<-3
combined$aggro[combined$Stereotypes_6=="Describes Black Americans poorly."]<-2
combined$aggro[combined$Stereotypes_6=="Describes Black Americans extremely poorly."]<-1
table(combined$aggro)

combined$violent<-NA
combined$violent[combined$Stereotypes_7=="Describes Black Americans extremely well."]<-6
combined$violent[combined$Stereotypes_7=="Describes Black Americans well."]<-5
combined$violent[combined$Stereotypes_7=="Describes Black Americans somewhat well."]<-4
combined$violent[combined$Stereotypes_7=="Describes Black Americans somewhat poorly."]<-3
combined$violent[combined$Stereotypes_7=="Describes Black Americans poorly."]<-2
combined$violent[combined$Stereotypes_7=="Describes Black Americans extremely poorly."]<-1
table(combined$violent
      
      #combined stereotype scale
#higher values = more ascription to neg steroetypes 
      combined$stereotypes<-(combined$lazy+combined$violent+combined$aggro+combined$lackd+combined$depend+combined$hardwork+combined$determined)
      table(combined$stereotypes)
      hist(combined$stereotypes)

      #party ID, 7 category
      table(combined$PartyID)
      combined$republican <- NA
      combined$republican[combined$PartyID == "Strong Democrat"] <- 1
      combined$republican[combined$PartyID == "Democrat"] <- 2
      combined$republican[combined$PartyID == "Weak Democrat"] <- 3
      combined$republican[combined$PartyID == "Independent"] <- 4
      combined$republican[combined$PartyID == "Weak Republican"] <- 5
      combined$republican[combined$PartyID == "Republican"] <- 6
      combined$republican[combined$PartyID == "Strong Republican"] <- 7
      table(combined$republican)
      hist(combined$republican)
      
      #party ID, 3 category 
      #0 is dem, 1 is ind, 2 is rep
      combined$pid3 <- NA
      combined$pid3[combined$PartyID == "Strong Democrat"] <- 0
      combined$pid3[combined$PartyID == "Democrat"] <- 0
      combined$pid3[combined$PartyID == "Weak Democrat"] <- 0
      combined$pid3[combined$PartyID == "Independent"] <- 1
      combined$pid3[combined$PartyID == "Weak Republican"] <- 2
      combined$pid3[combined$PartyID == "Republican"] <- 2
      combined$pid3[combined$PartyID == "Strong Republican"] <- 2
      table(combined$pid3)
      hist(combined$pid3)
      
      combined$pid2 <- NA
      combined$pid2[combined$PartyID == "Strong Democrat"] <- 0
      combined$pid2[combined$PartyID == "Democrat"] <- 0
      combined$pid2[combined$PartyID == "Weak Democrat"] <- 0
      combined$pid2[combined$PartyID == "Weak Republican"] <- 1
      combined$pid2[combined$PartyID == "Republican"] <- 1
      combined$pid2[combined$PartyID == "Strong Republican"] <- 1
      table(combined$pid2)
      
      combined$dem <- NA
      combined$dem[combined$PartyID == "Strong Democrat"] <- 1
      combined$dem[combined$PartyID == "Democrat"] <- 1
      combined$dem[combined$PartyID == "Weak Democrat"] <- 1
      combined$dem[combined$PartyID == "Weak Republican"] <- 0
      combined$dem[combined$PartyID == "Republican"] <- 0
      combined$dem[combined$PartyID == "Strong Republican"] <- 0
      table(combined$dem)
      
      #race, making it where 1 is white and 0 is nonwhite
      table(combined$Race)
      combined$white <- NA
      combined$white[combined$Race == "White"] <- 1
      combined$white[combined$Race == "Other"] <- 0
      combined$white[combined$Race == "Black or African American"] <- 0
      combined$white[combined$Race == "American Indian or Alaska Native"] <- 0
      combined$white[combined$Race == "Asian"] <- 0
      combined$white[combined$Race == "Hispanic or Latino"] <- 0
      combined$white[combined$Race == "Native Hawaiian or Pacific Islander"] <- 0
      table(combined$white)
      
      #gender
      table(combined$Gender)
      combined$female [combined$Gender == "Female"] <- 1
      combined$female [combined$Gender == "Male"] <- 0
      table(combined$female)
      
ttestgender = t.test(combined$whiteidentity~combined$female, mu=0, alt="two.sided", conf=0.95)
ttestgender   
