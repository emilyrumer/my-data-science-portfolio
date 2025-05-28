library(tidyverse)
library(psych)
library(ggplot2)
library(MASS) 
library(car)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
load("lfs(1).rda")

# GETTING DATA READY----

lfs$HOURPAY[lfs$HOURPAY<1]<-NA

hist(lfs$HOURPAY, breaks=100)
#log-transform hourly pay
lfs$loghrpay<-log(lfs$HOURPAY)

hist(lfs$loghrpay)

# declare missing values for all individuals with a number of hours lower than 1 hour
lfs$TOTHRS[lfs$TOTHRS<1]<-NA
lfs$TOTHRS[lfs$TOTHRS>97]<-NA

# check the basic descriptive statistics:
describe(lfs$TOTHRS)

# recode the part-time/full-time dichotomous variable
table(lfs$FTPTWK)
lfs$parttime[lfs$FTPTWK=="Full-time"]<-1
lfs$parttime[lfs$FTPTWK=="Part-time"]<-2
lfs$parttime<-as.factor(lfs$parttime)
levels(lfs$parttime)<-c("Full-time", "Part-time")
lfs$parttime <- droplevels(lfs$parttime)
table(lfs$parttime)

table(lfs$SEX)
lfs$SEX<-droplevels(lfs$SEX)



lfs$class<-NA

lfs$class[lfs$NSECMJ10=="Higher managerial and professional" | 
            lfs$NSECMJ10== "Lower managerial and professional"] <- 1

lfs$class[lfs$NSECMJ10=="Intermediate occupations" | 
            lfs$NSECMJ10== "Small employers and own account workers" |
            lfs$NSECMJ10=="Lower supervisory and technical"] <- 2

lfs$class[lfs$NSECMJ10=="Semi-routine occupations" |
            lfs$NSECMJ10=="Routine occupations" ] <-3

lfs$class<-as.factor(lfs$class)
levels(lfs$class)<-c("Manager & Professionals", "Intermediate", "Semi-routine & Routine")



#finally drop all invalid levels for variable SEX (don't knows, etc)

lfs$occup<-lfs$NSECMJ10
lfs$occup[lfs$occup=="Never worked, unemployed, and nec"]<-NA
lfs$occup[lfs$occup=="Does not apply"]<-NA

lfs$occup<-droplevels(lfs$occup)
table(lfs$occup)



# UNIVARIATE ANALYSIS----

#Dependent Variables
lfs |> 
  ggplot(aes(x = HOURPAY)) +
  geom_histogram()

lfs |> 
  ggplot(aes(x = loghrpay)) +
  geom_histogram()

#Independent Variable
lfs |> 
  ggplot(aes(x = TOTHRS)) +
  geom_histogram()

  #binary: fulltime partime
lfs |> 
  drop_na(parttime) |> 
  ggplot(aes(x = parttime)) +
  geom_bar(stat = "count")


# BIVARIATE ANALYSIS ----
#as it is bivariate, need to specify more than one variable
lfs |> 
  ggplot(aes(x = TOTHRS, y = loghrpay)) +
  geom_point()

#bivariate regression graphically
lfs |> 
  ggplot(aes(x = TOTHRS, y = loghrpay)) +
  geom_point() +
  geom_smooth(method = "lm")

lfs |>
  drop_na(parttime) |> 
  ggplot(aes(x = parttime, y = loghrpay)) +
  geom_boxplot()

#scatter with diff colours for sex
lfs |> 
  ggplot(aes(x = TOTHRS,
             y = loghrpay,
             colour = SEX)) +
  geom_point()
#shows men as greater pay and more hours. women as fewer hours and less pay


#T-TEST----
#comparison of means across different groups
t.test(lfs$loghrpay~lfs$parttime)
#can conclude p-value is extremely small - 
#     ...probability of us observing a difference in means in our data in a population where null hyp was true... 
#     ...is extrememly small. can reject NH that there is no diff in pay in FT and PT workers

cor.test(lfs$loghrpay, lfs$TOTHRS, use="complete.obs")


lfs<-lfs[complete.cases(lfs$loghrpay,
                        lfs$parttime,
                        lfs$SEX,
                        lfs$TOTHRS,
                        lfs$class), ]

#FITTING REGRESSION MODELS ----

#empty model - model with no IVs - highlights that the mean is also a model

fit0 <- lm(loghrpay~1, data=lfs)
fit0

#check mean
mean(lfs$loghrpay, na.rm=T)

#every time a predictor is added to the model, the mean will change depending on the variables added

fit1 <- lm(loghrpay ~ TOTHRS, data=lfs)
summary(fit1)
#means in any increase in 1 working hours, increase of 0.01 in loghrpay


fit2 <- lm(loghrpay ~ parttime, data=lfs)
summary(fit2)

#model is same as scatterplot with line across it
fit3 <- lm(loghrpay ~ TOTHRS, data=lfs)
summary(fit3)




#Looking at Confounders
#might suspect that association between working hrs and pay may be product of other variables that are correlated with IV and DV
#antecedent confounders (before point in time)

#Control 1: Sex

t.test((lfs$TOTHRS~lfs$SEX))

ftsex <- table(lfs$FTPT, lfs$sex)
prop.table(ftsex, 2)*100

fit3 <- lm(loghrpay ~ parttime + SEX, data=lfs)
summary(fit3)

#Control 2: Age
#Control 3:

#MULTIPLE REGRESSION----

#Model 2: All controls included
#can introduce sex, class and age as controls

fitmodel2<- lm(loghrpay ~ TOTHRS + sex + age + occup, data=lfs)

#converting from log pay / back to normal hours
log(10)
exp(2.302585)
