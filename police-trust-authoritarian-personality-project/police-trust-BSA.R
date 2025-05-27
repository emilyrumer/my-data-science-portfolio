#Package Downloading----
library(tidyverse)
library(knitr)
library(janitor)
library(kableExtra)
library(scales)
library(haven)
library(psych)
library(moments)
library(sjstats)
library(sjPlot)
library(sjlabelled) 
library(sjmisc)
library(car)
library(MASS)
bsa21 <- read_sav("BSA/UKDA-9072-spss/spss/spss25/bsa21.sav")

#Sub-setting data---- 
#...to include complete cases of TrstPlc and libauth
bsa21cc <- bsa21[complete.cases(bsa21$TrstPlc, bsa21$libauth), ]

#Dependent Variable and UNIVARIATE ANALYSIS - TrstPlc---- 
addmargins(table(bsa21$TrstPlc, useNA = "ifany"))

print(bsa21cc$TrstPlc)

bsa21cc$TrstPlc[bsa21cc$TrstPlc<1]<-NA       
bsa21cc$TrstPlc[bsa21cc$TrstPlc>11]<-NA
bsa21cc <- bsa21cc[!is.na(bsa21cc$TrstPlc), ]

table(bsa21cc$TrstPlc)
addmargins(table(bsa21cc$TrstPlc))

#Univariate Analysis DV: TrstPlc
meantrstplc <- mean(bsa21cc$TrstPlc, na.rm = TRUE)
meantrstplc
#mean 6.646091
mediantrstplc <- median(bsa21cc$TrstPlc, na.rm = TRUE)
mediantrstplc
#median 7

#Quartiles
summary(bsa21cc$TrstPlc)
quantile(bsa21cc$TrstPlc, c(.25, .75), na.rm = TRUE)
#5 and 8
IQR(bsa21cc$TrstPlc, na.rm = TRUE)
#3 

describe(bsa21cc$TrstPlc)
#SD: 2.42, meaning on average, trust in police is 2.42 'scored points' away from the mean
skewness(bsa21cc$TrstPlc, na.rm = TRUE)
#skewness very slight left skew (to the left, it is -0.5526472 which is <0)

#Density Plot Visualisation
ggplot(bsa21cc[complete.cases(bsa21cc$TrstPlc), ], aes(x=TrstPlc)) +
  geom_density() +
  xlab("Trust in Police") +
  ylab("Probability Density") +
  geom_vline(xintercept = meantrstplc, colour="blue") +
  geom_vline(xintercept = mediantrstplc, colour="red") +
  ggtitle("Density Curve of Trust in Police") +
  labs(caption = "Source: British Social Attitudes Survey 2021")

#Independent Variable and UNIVARIATE ANALYSIS - Libauth---- 
print(bsa21cc$libauth)

bsa21cc$libauth[bsa21cc$libauth<1]<-NA
bsa21cc$libauth[bsa21cc$libauth>5]<-NA
bsa21cc <- bsa21cc[!is.na(bsa21cc$libauth), ]

table(bsa21cc$libauth)

#Univariate analysis of IV
meanlibauth <- mean(bsa21cc$libauth, na.rm = TRUE)
meanlibauth
#mean 3.176543
medianlibauth <- median(bsa21cc$libauth, na.rm = TRUE)
medianlibauth
#median 3.333

#Quartiles
summary(bsa21cc$libauth)
quantile(bsa21cc$libauth, c(.25, .75), na.rm = TRUE)
#2.67 and 3.83
IQR(bsa21cc$libauth, na.rm = TRUE)
#1.17

describe(bsa21cc$libauth)
#SD: 0.83, meaning on average, libauth is 0.83 units away from the mean
skewness(bsa21cc$libauth, na.rm = TRUE)
#skewness very slight left skew (to the left, it is -0.3629985 which is <0)

#Density Plot Visualisation
ggplot(bsa21cc[complete.cases(bsa21cc$libauth), ], aes(x=libauth)) +
  geom_density() +
  xlab("Libertarian-Authoritarian Personality") +
  ylab("Probability Density") +
  geom_vline(xintercept = meanlibauth, colour="blue") +
  geom_vline(xintercept = medianlibauth, colour="red") +
  ggtitle("Density Curve of Libertarian-Authoritarian Personality") +
  labs(caption = "Source: British Social Attitudes Survey 2021")
#mean and median is very slightly to the right, meaning on average slightly more authoritarian

#Control Variables Univariate Analysis----
#1. AGE----
print(bsa21cc$RespAge_Archive)

bsa21cc$RespAge_Archive[bsa21cc$RespAge_Archive<18]<-NA       
bsa21cc$RespAge_Archive[bsa21cc$RespAge_Archive>80]<-NA
bsa21cc <- bsa21cc[!is.na(bsa21cc$RespAge_Archive), ]

table(bsa21cc$RespAge_Archive)
describe(bsa21cc$RespAge_Archive)
summary(bsa21cc$RespAge_Archive)

meanage <- mean(bsa21cc$RespAge_Archive, na.rm = TRUE)
meanage
#mean 50.66
medianage <- median(bsa21cc$RespAge_Archive, na.rm = TRUE)
medianAge
#median 52

ggplot(bsa21cc[complete.cases(bsa21cc$RespAge_Archive), ], aes(x=RespAge_Archive)) +
  geom_histogram() +
  xlab("Age") +
  ylab("Count") +
  geom_vline(xintercept = meanage, colour="blue") +
  geom_vline(xintercept = medianAge, colour="red") +
  ggtitle("Distribution of Age") +
  labs(caption = "Source: British Social Attitudes Survey 2021")
 
#2. SEX----
bsa21cc$DVSex21[bsa21cc$DVSex21<1]<-NA     
bsa21cc$DVSex21[bsa21cc$DVSex21>2]<-NA

bsa21cc <- bsa21cc[!is.na(bsa21cc$DVSex21), ]

#Frequency table age disribution
frq(bsa21cc$DVSex21, out = "v", show.na = FALSE,
    title = "Table 1: Distribution of Sex",
    file="Sexdistribution.doc")

#3. RACE----
print(bsa21cc$RaceOri4)

bsa21cc$RaceOri4[bsa21cc$RaceOri4<1]<-NA       
bsa21cc$RaceOri4[bsa21cc$RaceOri4>4]<-NA
bsa21cc <- bsa21cc[!is.na(bsa21cc$RaceOri4), ]

tabrace <- table(bsa21cc$RaceOri4)
addmargins(tabrace)
prop.table(tabrace)*100

frq(bsa21cc$RaceOri4, out = "v", show.na = FALSE,
    title = "Table 2: Distribution of Race",
    file="racedistribution.doc")


#BIVARIATE ANALYSIS----
#Bivariate of DV and Main Predictor----
#Correlation Coefficient
cor.test(bsa21cc$libauth, bsa21cc$TrstPlc, use="complete.obs")
#p-value: 0.004615 (is smaller than 0.05 so is statistically significant)

#Scatter plot visualisation
ggplot(data = bsa21cc, aes (x=libauth, y=TrstPlc)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Libertarian-Authoritarian Personality") +
  ylab("Trust in Police") +
  ggtitle("Scatter Plot of Bivariate Association") +
  labs(caption = "Source: British Social Attitudes Survey 2021")

#BIVARIATE OF CONTROL VARIABLES----
#BV Sex----
addmargins(table(bsa21cc$DVSex21, bsa21cc$TrstPlc))
by(bsa21cc$TrstPlc,bsa21cc$DVSex21, describe)

sjt.xtab(bsa21cc$TrstPlc, bsa21cc$DVSex21, show.col.prc = TRUE,
         title="Bivariate Association of Sex and Trust in Police")

cor.test(bsa21cc$DVSex21, bsa21cc$TrstPlc, use="complete.obs")

#BV Age RespAge_Archive----
table(bsa21cc$RespAge_Archive, bsa21cc$TrstPlc)

#Density Plot TrstPlc and Age
ggplot(data=bsa21cc, aes(y=TrstPlc, x=RespAge_Archive)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Age") +
  ylab("Trust in Police") +  
  ggtitle("Bivariate Distribution of Trust in Police and Age") +
  labs(caption = "Source: British Social Attitudes Survey 2021")

cor.test(bsa21cc$RespAge_Archive, bsa21cc$TrstPlc, use="complete.obs")


#BV Race----
sjt.xtab(bsa21cc$TrstPlc, bsa21cc$RaceOri4, show.col.prc = TRUE,
         title="Bivariate Association of Race and Trust in Police",
         file = "Race and TrstPlc.doc")

cor.test(bsa21cc$RaceOri4, bsa21cc$TrstPlc, use="complete.obs")

#EMPTY MODEL----
bsa21cc<-bsa21cc[complete.cases(bsa21cc$TrstPlc, bsa21cc$libauth, bsa21cc$RespAge_Archive, bsa21cc$DVSex21, bsa21cc$RaceOri4), ]

emptymodel <- lm(TrstPlc~1, data=bsa21cc)
summary(emptymodel)

mean(bsa21cc$TrstPlc, na.rm=T)

#SIMPLE LINEAR REGERESSION MODEL----
modelsimple <- lm(TrstPlc ~ libauth, data=bsa21cc)
summary(modelsimple)

tab_model(modelsimple, 
          dv.labels = c("Trust in Police (modelsimple)"),
          pred.labels=c("Intercept", "Libertarian-Authoritarian Personality"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE)

tab_model(modelsimple, 
          dv.labels = c("Trust in Police (modelsimple)"),
          pred.labels=c("Intercept", "Libertarian-Authoritarian Personality"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE,
          file="linearmodelsimple.html")

#MULTIPLE LINEAR REGERESSION MODEL----

#dummy variables for categoricals
bsa21cc$RaceOri4 <- as.factor(bsa21cc$RaceOri4)
bsa21cc$DVSex21 <- as.factor(bsa21cc$DVSex21)

modelmultiple <- lm(TrstPlc ~ libauth + DVSex21 + RespAge_Archive + RaceOri4, data=bsa21cc)
summary(modelmultiple)

tab_model(modelmultiple)

#both together:
tab_model(modelsimple, modelmultiple,
          dv.labels = c("Trust in Police (modelsimple)", "Trust in Police (modelmultiple)"),
          pred.labels=c("Intercept", "Libertarian-Authoritarian Personality","Female", "Age (in years)", "Race - Asian origin", "Race - White origin", "Race - Mixed origin"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE)

#file:
tab_model(modelsimple, modelmultiple,
          dv.labels = c("Trust in Police (modelsimple)", "Trust in Police (modelmultiple)"),
          pred.labels=c("Intercept", "Libertarian-Authoritarian Personality","Female", "Age (in years)", "Race - Asian origin", "Race - White origin", "Race - Mixed origin"),
          show.se=TRUE, show.r2 = TRUE, show.fstat = TRUE, show.aic = TRUE,
          file="linearmodelmultiple.html")

plot_model(modelmultiple, title = "Regression coefficients",
           sort.est=FALSE)


#Model Diagnostics----

#Assessing Outliers - Bonferonni p-value Test
outlierTest(modelmultiple, n.max=100) 

#Leverage test
levmodelmultiple <- hat(model.matrix(modelmultiple))
mean(levmodelmultiple)

plot(levmodelmultiple)

#Cook's distance
cook = cooks.distance(modelmultiple)
plot(cook,ylab="Cook's distances")

#QQ Plot for studentized residuals
qqPlot(modelmultiple, main="QQ Plot for Multiple Regression Model")

#Distribution of studentized residuals
sresid <- studres(modelmultiple)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=1000)
yfit<-dnorm(xfit)
lines(xfit, yfit)

#Plot studentized residuals vs. fitted values
spreadLevelPlot(modelmultiple)

# variance inflation factors
vif(modelmultiple) 

sqrt(vif(modelmultiple)) > 2