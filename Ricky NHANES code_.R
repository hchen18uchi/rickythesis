
#good website for nhanes analysis in R 
##https://static-bcrf.biochem.wisc.edu/courses/Tabular-data-analysis-with-R-and-Tidyverse/book/12-usingNHANESweights.html
##https://ehsanx.github.io/SPPH504007SurveyData/docs/importing-nhanes-to-r.html


#Install NHANES packages 
install.packages(c("RNHANES", "nhanesA", "tidyverse", "tableone", "tab"))

#load packages 
library(RNHANES)
library(nhanesA)
library(tidyverse)
library(tableone)
library(tab)
library(survey)
library(dplyr)
library(coefplot)
library(stargazer)




#import metals dataset from 2017
metals2017 <- nhanes('UM_J') %>% tibble()

#import demographic dataset from 2017
demo2017 <- nhanes('DEMO_J') %>% tibble() 

#import depression dataset from 2017
dep <- nhanes('DPQ_J') %>% tibble() 

#see how many observations available phthalates 
ph2017 <- nhanes('PHTHTE_J') %>% tibble()


#merge datasets 
merged.data <- merge(dep, metals2017, 
                      by = c("SEQN"))

merged.data2 <- merge(merged.data, demo2017, 
                     by = c("SEQN"))

summary(merged.data2)

#Table 1 example descriptive characteristics
library(table1)
table1(~ factor(RIAGENDR) + RIDAGEYR + factor(RIDRETH1), data=merged.data2)

#recode some variables
nhanesAnalysis <- merged.data2 %>%
      mutate (gender=recode(RIAGENDR, '1' = 0L,
                            '2' = 1L))
nhanesAnalysis$gender <- as.factor(nhanesAnalysis$gender)
head(nhanesAnalysis$gender)

#Turns Dep score into numbers

nhanesAnalysis <- nhanesAnalysis %>%
  mutate(across(starts_with("DPQ"), ~case_when(
    . == "Not at all" ~ "0",
    . == "Several days" ~ "1",
    . == "More than half the days" ~ "2",
    . == "Nearly every day" ~ "3",
    TRUE ~ NA_character_  # This line handles the 'anything else' case
  )))

#Creates a new variable called DPQsum, which is the total depression score

# Convert DPQ variables to numeric
nhanesAnalysis <- nhanesAnalysis %>%
  mutate(across(starts_with("DPQ"), ~as.numeric(as.character(.)), .names = "numeric_{.col}"))

# Create the DPQsum variable as the sum of all DPQ variables
nhanesAnalysis <- nhanesAnalysis %>%
  mutate(DPQsum = rowSums(select(., starts_with("numeric_DPQ")), na.rm = TRUE))



###weights- need to account for weights in nhanes ,(cov)
nhanesDesign <- svydesign(id=~SDMVPSU,
                           strata = ~SDMVSTRA,
                          weights = ~WTMEC2YR,
                          nest=TRUE,
                          data=nhanesAnalysis)

#simple linear regression analysis 
#insert exposures and outcomes (depression + metals etc), outcome = dep score,
output <- svyglm(outcome ~ exposure +
                   exposure,
                 family = gaussian(), #using a linear model
                 data   = nhanesAnalysis,
                 design = nhanesDesign)

#linear regression for URXUBA(Barium)
outputURXUBA <- svyglm(DPQsum ~ URXUBA +
                   URXUBA,
                 family = gaussian(), #using a linear model
                 data   = nhanesAnalysis,
                 design = nhanesDesign)
#Data Visualization for Barium
coefplot(outputURXUBA)
# Generate a summary table of the model 1
stargazer(outputURXUBA, type = "text")


#example mixtures analysis code
#quantile g-comp#
qfit2 <- qgcomp.noboot(depression~exposure1+exposure2+exposure3+exposure4+cov1+ +cov2 + cov3,
                       data = nhanesDesign,
                       expnms = c("exposure1","exposure2","exposure3","exposure4"),
                       q =4)
qfit2
plot(qfit2)


######################


