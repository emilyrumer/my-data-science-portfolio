library(foreign)
library(ggplot2)
library(psych)
library(car)
library(rcompanion)
library(lmtest)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(easystats)

options(digits=3) 

bes<-read.csv("bes_qstep(4).csv", 
              header=TRUE)

options(digits=3) 

#Data Preparation ----

# Whether voted in previous general election
table(bes$vote05)
bes$vote05 <- factor(bes$vote05)
levels(bes$vote05) <- c("Not Voted", "Voted")
table(bes$vote05)

# Gender
table(bes$gender)
bes$gender <- factor(bes$gender)
levels(bes$gender) <- c("Male", "Female")
table(bes$gender)

# Whether owner or renting
table(bes$tenure)
bes$tenure <- factor(bes$tenure)
levels(bes$tenure) <-c ("Owner","Rental")
table(bes$tenure)

# Occupational class 
table(bes$class) 
bes$class<-as.factor(bes$class)
levels(bes$class)<-c("Professional/HigherTechnical",
                     "Manager/Senior Admin",
                     "Clerical",
                     "Sales/services",
                     "Small business owner",
                     "Foreman/supervisor",
                     "Skilled Manual",
                     "Semi/Unskilled Manual")
table(bes$class) 

#Education level
table(bes$education)
bes$education<-factor(bes$education)
levels(bes$education)<-c("Graduate/PG", "A-level/FE/HE",
                         "GCSE A-C", "Below GCSE A-C")
table(bes$education)
bes$education[bes$education==1]<-"Graduate/PG"
bes$education[bes$education==2]<-"A-level/FE/HE"
bes$education[bes$education==3]<-"GCSE A-C"
bes$education[bes$education==4]<-"Below GCSE A-C"
table(bes$education)

describe(bes$age)
hist(bes$age)  


bes$votenum<-NA
bes$votenum[bes$vote05=="Not Voted"] <- 0
bes$votenum[bes$vote05=="Voted"] <- 1

ggplot(data=bes, aes(x=age, y=votenum)) + 
  geom_point(shape=1,  position = position_jitter(w = 0, h = 0.03)) +
  geom_smooth(method = "lm", fullrange=TRUE ) +  xlim(0,130)
 

#Statistical analysis ----

votetab<-table(bes$vote05)
addmargins(votetab)
prop.table(votetab)
#vote table shows more people votes

tenuretab<-table(bes$tenure)
addmargins(tenuretab)
prop.table(tenuretab)
#tenure table shows heavier ownership distribution

tenvote<-table(bes$vote05,bes$tenure)

addmargins(tenvote)
#table shows the 2 categorical data (tenure and vote)


prop.table(tenvote, 2)
#shows owners are more likely to vote than non-owners


ggplot(data=bes[!is.na(bes$vote05) & !is.na(bes$tenure) , ], 
       aes(x=tenure, fill=vote05)) +
  geom_bar(stat="count", aes(fill=vote05), position="fill")


#when working with bivariate categorical, usually use chi-sq test. 
#rearranges the table to make it look as if there is no association between tenure and vote. if the table looks
#... different to the imaginary table with no association, we say the data came from a population where there is an association
#... assuming it comes from a random sample
marg<-addmargins(tenvote)

exp<-matrix(nrow = 2, ncol = 2)

exp[1,1]<-marg[1,3]*marg[3,1]/marg[3,3] 
exp[2,1]<-marg[2,3]*marg[3,1]/marg[3,3]
exp[1,2]<-marg[1,3]*marg[3,2]/marg[3,3]
exp[2,2]<-marg[2,3]*marg[3,2]/marg[3,3]

#this is the expected table - this does not exist. 
#is useful to make a case that something has an effect on something, so need to create world where there is no 
#... effect on each of those things.
exp

#this is the tenvote table
tenvote

#would expect larger amount of tenant voters
#in imaginary world where null hypothesis is try, would have a greater representation of renters who vote
#... and a greater representation of homeowners who do not vote
# relative odds of voting to not voting is same for the 2 groups


# chi-squared test 
chisq.test(tenvote)
# ^ only 1 degree of freedom, means the table can only be reconstructed by choosing 1 value - this is calculated with margins
#(if it is a 3x2 table, there are 2 degrees, can choose any 2 numbers to know the rest due to margins)

chi_tenvote<-chisq.test(tenvote)

objects(chi_tenvote) #the test saves a number of objects in memory that we can retrieve

round(chi_tenvote$observed,2) #observed values in the original table
round(chi_tenvote$expected,2) #expected values under the null = row sum x column sum / grand total

#this is the value of chi-square for the table
chi_value<-sum((chi_tenvote$observed-chi_tenvote$expected)^2/chi_tenvote$expected) 
chi_value

pval<-pchisq(chi_value, 1, lower.tail = FALSE)
pval


# logistic regression ----

#this function gets rid of all missing values across dataset so all variables have same number
#need to make sure: sample only those cases which are complete, (with the variables you want - specify this - is in lin reg WSG)
bes<-bes[complete.cases(bes), ]

#glm = generalised linear model 
model1 <- glm(vote05 ~ tenure, data = bes, family = "binomial")
summary(model1)
#DV is the probability of the second category. 2nd cat is to vote, so probability is of voting

#next model controls for education and age. not gender yet
model2 <- glm(vote05 ~ tenure + education + age, data = bes, family = "binomial")
summary(model2)
#after including controls age & education, the result remains significant 
#can't do side by side comparisons like with linear regression, have to look at sign (+/-) and value

#this compares models 
#more IV you put into regression, the better model fit you will get 
anova(model1,model2, test="Chisq") 


model2gender <- glm(vote05 ~ tenure + education + age + gender, data = bes, family = "binomial")
summary(model2gender)
#gender is insignificant - doesn't change the coefficients much at all, and isn't significant
#to see if a variable is significant, estimate with and without - see if there is much difference in the other values
#if removing/including variable changes estimates, try and find out why (literature)

anova(model2,model2gender, test="Chisq") 

model3 <- glm(vote05 ~ tenure + education + age + class, data = bes, family = "binomial")
summary(model3)

anova(model2,model3, test="Chisq") 


exp(cbind(OR=coef(model2), confint(model2)))


