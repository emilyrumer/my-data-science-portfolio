#Package Installation----

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(foreign) 
library(psych) 
library(car) 
library(rcompanion) 
library(lmtest) 
library(sjPlot) 
library(sjmisc) 
library(sjlabelled) 
library(easystats) 
library(knitr)
library(janitor)
library(scales)

load("a2022_23_hmip_prisoner_survey_adults_eul.dta")

hmip2223 <- a2022_23_hmip_prisoner_survey_adults_eul



#Cleaning data from women prison responses----

#Cleaning women prison responses out - from 'Functional type of establishment grouped' variable (A_functype_G1)

#Labelling categories
table(hmip2223$A_functype_G1)
hmip2223$functype<-(hmip2223$A_functype_G1)
hmip2223$A_functype_G1 <- factor(hmip2223$A_functype_G1)
levels(hmip2223$A_functype_G1) <- c("High Security - Cat A", "Locals", "Trainers", "Women", "Open - Cat D")
table(hmip2223$A_functype_G1)

hmip2223$functype<-(hmip2223$A_functype_G1)
table(hmip2223$functype)

#THIS GETS RID OF WOMEN PRISON ANSWERS - the sample has now gone from 6403 to 5981
hmip2223$functype[hmip2223$functype=="Women"]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$functype), ]
table(hmip2223$functype)

addmargins(table(hmip2223$functype))


#Univariate analysis of Cell Sharing and Mental Health----

#Independent Variable: Cell Sharing----
tabcellshare<-table(hmip2223$E_cellown_01)
prop.table(tabcellshare)
addmargins(tabcellshare)
tabcellshare

table(hmip2223$E_cellown_01)
hmip2223$E_cellown_01 <- factor(hmip2223$E_cellown_01)
levels(hmip2223$E_cellown_01) <- c("No, in a shared cell", "Yes", "NA")
table(hmip2223$E_cellown_01)

hmip2223$cellown<-NA
hmip2223$cellown[hmip2223$E_cellown_01=="No, in a shared cell"] <- 0
hmip2223$cellown[hmip2223$E_cellown_01=="Yes"] <- 1

hmip2223$cellown <- factor(hmip2223$cellown)
levels(hmip2223$cellown) <- c("No, in a shared cell", "Yes")
tabcellown<-table(hmip2223$cellown)
tabcellown
tabcellownpropround<-prop.table(tabcellown)
round(tabcellownpropround,4)

addmargins(table(hmip2223$cellown))

#Dependent Variable: Mental Health----
tabmnthl<-table(hmip2223$M_mhissues_01)
prop.table(tabmnthl)
addmargins(tabmnthl)
tabmnthl

table(hmip2223$M_mhissues_01)
hmip2223$M_mhissues_01 <- factor(hmip2223$M_mhissues_01)
levels(hmip2223$M_mhissues_01) <- c("No", "Yes", "NA")
addmargins(table(hmip2223$M_mhissues_01))

hmip2223$mnthl<-NA
hmip2223$mnthl[hmip2223$M_mhissues_01=="No"] <- 0
hmip2223$mnthl[hmip2223$M_mhissues_01=="Yes"] <- 1

hmip2223$mnthl <- factor(hmip2223$mnthl)
levels(hmip2223$mnthl) <- c("No", "Yes")
table(hmip2223$mnthl)
addmargins(table(hmip2223$mnthl))

tabmnthl<-table(hmip2223$mnthl)
tabmnthl
tabmnthlpropround<-prop.table(tabmnthl)
round(tabmnthlpropround,6)


#Univariate analysis of DV and IV once data cleaned (graphs of cellown and mnthl)----

#Graph with cell sharing in count (after running all of the above)
ggplot(data=hmip2223[!is.na(hmip2223$cellown),], aes(x = cellown)) + geom_bar(fill = "lightblue2", 
                                                                              color="lightblue3") +
  labs(title = "Cell-sharing in Prison",
       x = "Are you in a cell on your own?",
       y = "Number of responses") +
  scale_x_discrete(labels=c("No, I'm in a shared cell", "Yes")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 


#Graph with cell sharing in count (after running all of the above)
ggplot(data=hmip2223[!is.na(hmip2223$mnthl),], aes(x = mnthl)) + geom_bar(fill = "darkseagreen3", 
                                                                          color="darkseagreen4") +
  labs(title = "Mental Health in Prison",
       x = "Do you have any mental health problems?",
       y = "Number of responses") +
  scale_x_discrete(labels=c("No", "Yes")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 



#Bivariate analysis----

#Cell sharing and mental health----
cellsharemnthl<-table(hmip2223$mnthl, hmip2223$cellown)
cellsharemnthl
addmargins(cellsharemnthl)

prop.table(cellsharemnthl, 4)

#Stacked bar chart
ggplot(data=hmip2223[!is.na(hmip2223$mnthl) & !is.na(hmip2223$cellown) , ],
       aes(x=cellown, fill=mnthl)) +
  geom_bar(stat="count", aes(fill=mnthl), position="fill") +
  labs(title = "Mental Health and Cell Ownership",
       x = "Are you in a cell on your own?",
       y = "Proportion",
       fill = "Do you have any 
       mental health 
       problems?") 

#Chi-squared test
chi_cellsharemnthl<-chisq.test(cellsharemnthl, correct = FALSE)
chi_cellsharemnthl

#Cramer's
cramerV(cellsharemnthl)

sjt.xtab(hmip2223$mnthl, hmip2223$cellown, show.col.prc = TRUE,
         title="Table: Bivariate Distribution of Cell-sharing and Wellbeing")

         (file = "bivarcellownmnthl.docx")

#Bivariate contingency heat map
# Create the contingency table
cellsharemnthl <- table(hmip2223$mnthl, hmip2223$cellown)
cellsharemnthl
addmargins(table(hmip2223$mnthl, hmip2223$cellown))


# Convert the table to a data frame
cellsharemnthl_df <- as.data.frame.table(cellsharemnthl)

# Plot contingency table 

heatmap +
  theme(text = element_text(family = "Times New Roman"))


heatmap <- ggplot(data = cellsharemnthl_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue1", high = "lightblue4") +
  labs(
       x = "Do You Have Mental Health Problems?",
       y = "Are you in your own cell?",
       fill = "Frequency",
       caption = "Source: HMIP Prisoner Survey 2022-23")
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
  
  
  
  
  
# Create the contingency table
cellsharemnthl3 <- table(hmip2223$cellown, hmip2223$mnthl)
cellsharemnthl3
addmargins(table(hmip2223$cellown, hmip2223$mnthl))


# Convert the table to a data frame
cellsharemnthl_df3 <- as.data.frame.table(cellsharemnthl3)
heatmap3
  
heatmap3 <- ggplot(data = cellsharemnthl_df3, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue1", high = "lightblue4") +
  labs(
       x = "Are you in your own cell?",
       y = "Do you have mental health problems?",
       fill = "Frequency",
       caption = "Source: HMIP Prisoner Survey 2022-23")
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #Analysis of Controls ----

#functype----
#Univariate analysis of functype

tabfunctype<-table(hmip2223$functype)
tabfunctype
prop.table(tabfunctype)
addmargins(tabfunctype)

prop.table(tabfunctype)*100


#Bivariate analysis of functype and cellown
tabcellownfunctype<-table(hmip2223$functype, hmip2223$cellown)
tabcellownfunctype
addmargins(tabcellownfunctype)
#Bivariate chi-sq functype and cellown DOESNT WORK
chi_tabcellownfunctype<-chisq.test(tabcellownfunctype, correct = FALSE)
chi_tabcellownfunctype

#Bivariate analysis of functype and mntlhl
tabmntlhlfunctype<-table(hmip2223$functype, hmip2223$mnthl)
tabmntlhlfunctype
addmargins(tabmntlhlfunctype)
#Bivariate chi-sq of functype and mntlhl
chi_tabmntlhlfunctype<-chisq.test(tabmntlhlfunctype, correct = FALSE)
chi_tabmntlhlfunctype


#age----
B_age_05_G1
table(a2022_23_hmip_prisoner_survey_adults_eul$B_age_05_G1)

#Labelling categories
table(hmip2223$B_age_05_G1)
hmip2223$age<-(hmip2223$B_age_05_G1)
hmip2223$age <- factor(hmip2223$B_age_05_G1)
levels(hmip2223$age) <- c("21 to 25", "26 to 49", "50 and over", "70 and over")
table(hmip2223$age)

#Univariate analysis of age
tabage<-table(hmip2223$age)
tabage
prop.table(tabage)
addmargins(tabage)

prop.table(tabage, 4)


ggplot(data=hmip2223[!is.na(hmip2223$age),], aes(x = age)) + geom_bar(fill = "rosybrown3", 
                                                                      color="rosybrown4") +
  labs(title = "Grouped Age Distribution of Respondents",
       x = "Age",
       y = "Number of responses") +
  scale_x_discrete(labels=c("21 to 25", "26 to 49", "50 and over", "70 and over")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of age and cellown
tabcellownage<-table(hmip2223$age, hmip2223$cellown)
tabcellownage
addmargins(tabcellownage)
#Bivariate chi-sq of age and cellown
chi_tabcellownage<-chisq.test(tabcellownage, correct = FALSE)
chi_tabcellownage

#Bivariate analysis of age and mntlhl
tabmntlhlage<-table(hmip2223$age, hmip2223$mnthl)
tabmntlhlage
addmargins(tabmntlhlage)
#Bivariate chi-sq age and mnthl
chi_tabmntlhlage<-chisq.test(tabmntlhlage, correct = FALSE)
chi_tabmntlhlage


#ethnic----
B_ethnic_04_G1
table(a2022_23_hmip_prisoner_survey_adults_eul$B_ethnic_04_G1)

#Labelling categories
table(hmip2223$B_ethnic_04_G1)
hmip2223$ethnic<-(hmip2223$B_ethnic_04_G1)
hmip2223$ethnic <- factor(hmip2223$B_ethnic_04_G1)
levels(hmip2223$ethnic) <- c("Asian", "Black", "Mixed", "White", "Other")
table(hmip2223$ethnic)

#Univariate analysis of ethnic
tabethnic<-table(hmip2223$ethnic)
tabethnic
prop.table(tabethnic)*100
addmargins(tabethnic)

ggplot(data=hmip2223[!is.na(hmip2223$ethnic),], aes(x = ethnic)) + geom_bar(fill = "rosybrown3", 
                                                                            color="rosybrown4") +
  labs(title = "Ethnic Distribution of Respondents",
       x = "Ethnicity",
       y = "Number of responses") +
  scale_x_discrete(labels=c("Asian", "Black", "Mixed", "White", "Other")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

# Bivariate analysis of ethnic and cellown
tabcellownethnic<-table(hmip2223$ethnic, hmip2223$cellown)
tabcellownethnic
addmargins(tabcellownethnic)
#Bivariate chi-sq ethnic and cellown
chi_tabcellownethnic<-chisq.test(tabcellownethnic, correct = FALSE)
chi_tabcellownethnic

# Bivariate analysis of ethnic and mntlhl
tabmntlhlethnic<-table(hmip2223$ethnic, hmip2223$mnthl)
tabmntlhlethnic
addmargins(tabmntlhlethnic)
#Bivariate chi-sq ethnic and mntlhl
chi_tabmntlhlethnic<-chisq.test(tabmntlhlethnic, correct = FALSE)
chi_tabmntlhlethnic


#sentlng----
B_statuslength_05
table(a2022_23_hmip_prisoner_survey_adults_eul$B_statuslength_05)
table(hmip2223$B_statuslength_05)

#Labelling categories
table(hmip2223$B_statuslength_05)
hmip2223$sentlng<-(hmip2223$B_statuslength_05)
hmip2223$sentlng <- factor(hmip2223$B_statuslength_05)

#Drop NAs 
hmip2223$sentlng[hmip2223$sentlng==99]<-NA

hmip2223 <- hmip2223[!is.na(hmip2223$sentlng), ]

hmip2223$sentlng <- factor(hmip2223$sentlng)
levels(hmip2223$sentlng) <- c("Less than 6 months", "6 months - 1 year", "1 - 4 years", "4 - 10 years", "10 + years", "IPP", "Life", "Not currently serving a sentence")
table(hmip2223$sentlng)

#Univariate analysis of sentlng
tabsentlng<-table(hmip2223$sentlng)
tabsentlng
prop.table(tabsentlng)
addmargins(tabsentlng)

ggplot(data=hmip2223[!is.na(hmip2223$sentlng),], aes(x = sentlng)) + geom_bar(fill = "rosybrown3", 
                                                                              color="rosybrown4") +
  labs(title = "Sentence Length of Respondents",
       x = "Sentence Length",
       y = "Number of responses") +
  scale_x_discrete(labels=c("Under 6 months", "6m - 1 yr", "1-4 yr", "4-10 yr", "10+ yr", "IPP", "Life", "Not currently 
  serving a sentence", "Sentence length NA")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") +
  theme(axis.text.x = element_text(angle = 270))

#Bivariate analysis of sntlng and cellown
tabcellownsentlng<-table(hmip2223$sentlng, hmip2223$cellown)
tabcellownsentlng
addmargins(tabcellownsentlng)
#Bivariate chi-sq of sntlng and cellown
chi_tabcellownsentlng<-chisq.test(tabcellownsentlng, correct = FALSE)
chi_tabcellownsentlng

#Bivariate analysis of sntlng and mntlhl
tabmntlhlsentlng<-table(hmip2223$sentlng, hmip2223$mnthl)
tabmntlhlsentlng
addmargins(tabmntlhlsentlng)
#Bivariate chi-sq sntlng and mnthl
chi_tabmntlhlsentlng<-chisq.test(tabmntlhlsentlng, correct = FALSE)
chi_tabmntlhlsentlng


#unsfev----
Q_unsafe_02

#Labelling categories
table(hmip2223$Q_unsafe_02)
hmip2223$unsfev<-(hmip2223$Q_unsafe_02)
hmip2223$unsfev <- factor(hmip2223$Q_unsafe_02)

#Drop NAs - 
hmip2223$unsfev[hmip2223$unsfev==99]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$unsfev), ]


hmip2223$unsfev <- factor(hmip2223$unsfev)
levels(hmip2223$unsfev) <- c("No", "Yes")

hmip2223$unsfev <- factor(hmip2223$unsfev)
table(hmip2223$unsfev)

#Univariate analysis of unsfev
tabunsfev<-table(hmip2223$unsfev)
tabunsfev
prop.table(tabunsfev)
addmargins(tabunsfev)

ggplot(data=hmip2223[!is.na(hmip2223$unsfev),], aes(x = unsfev)) + geom_bar(fill = "rosybrown3", 
                                                                            color="rosybrown4") +
  labs(title = "Responses to 'Have you ever felt unsafe here?'",
       x = "'Have you ever felt unsafe here?'",
       y = "Number of responses") +
  scale_x_discrete(labels=c("No", "Yes")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of unsfev and cellown
tabcellownunsfev<-table(hmip2223$unsfev, hmip2223$cellown)
tabcellownunsfev
addmargins(tabcellownunsfev)
#Bivariate chi-sq of unsfev and cellown
chi_tabcellownunsfev<-chisq.test(tabcellownunsfev, correct = FALSE)
chi_tabcellownunsfev

#Bivariate analysis of unsfev and mntlhl
tabmntlhlunsfev<-table(hmip2223$unsfev, hmip2223$mnthl)
tabmntlhlunsfev
addmargins(tabmntlhlunsfev)
#Bivariate chi-sq unsfev and mnthl
chi_tabmntlhlunsfev<-chisq.test(tabmntlhlunsfev, correct = FALSE)
chi_tabmntlhlunsfev


#disab ----
N_disabled_01
table(a2022_23_hmip_prisoner_survey_adults_eul$N_disabled_01)

#Labelling categories
table(hmip2223$N_disabled_01)
hmip2223$disab<-(hmip2223$N_disabled_01)
hmip2223$disab <- factor(hmip2223$N_disabled_01)
#Drop NAs
hmip2223$disab[hmip2223$disab==99]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$disab), ]
hmip2223$disab <- factor(hmip2223$disab)

levels(hmip2223$disab) <- c("No", "Yes")
table(hmip2223$disab)

#Univariate analysis of disab
tabdisab<-table(hmip2223$disab)
tabdisab
prop.table(tabdisab)
addmargins(tabdisab)

ggplot(data=hmip2223[!is.na(hmip2223$disab),], aes(x = disab)) + geom_bar(fill = "rosybrown3", 
                                                                          color="rosybrown4") +
  labs(title = "Responses to 'Do you consider yourself 
       to have a disability?'",
       x = "'Do you consider yourself to have a disability?'",
       y = "Number of responses") +
  scale_x_discrete(labels=c("No", "Yes")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of disab and cellown
tabcellowndisab<-table(hmip2223$disab, hmip2223$cellown)
tabcellowndisab
addmargins(tabcellowndisab)
#Bivariate chi-sq of disab and cellown
chi_tabcellowndisab<-chisq.test(tabcellowndisab, correct = FALSE)
chi_tabcellowndisab

#Bivariate analysis of disab and mntlhl
tabmntlhldisab<-table(hmip2223$disab, hmip2223$mnthl)
tabmntlhldisab
addmargins(tabmntlhldisab)
#Bivariate chi-sq disab and mnthl
chi_tabmntlhldisab<-chisq.test(tabmntlhldisab, correct = FALSE)
chi_tabmntlhldisab


#drug----
#Have you developed a problem with illicit drugs since you have been in this prison?
P_drugdevprob_01

#Labelling categories
table(hmip2223$P_drugdevprob_01)
hmip2223$drug<-(hmip2223$P_drugdevprob_01)
hmip2223$drug <- factor(hmip2223$P_drugdevprob_01)
#Drop NAs
hmip2223$drug[hmip2223$drug==99]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$drug), ]

hmip2223$drug <- factor(hmip2223$drug)

levels(hmip2223$drug) <- c("No", "Yes")
table(hmip2223$drug)

#Univariate analysis of drug
tabdrug<-table(hmip2223$drug)
tabdrug
prop.table(tabdrug)
addmargins(tabdrug)

ggplot(data=hmip2223[!is.na(hmip2223$drug),], aes(x = drug)) + geom_bar(fill = "rosybrown3", 
                                                                          color="rosybrown4") +
  labs(title = "Responses to 'Have you developed a problem with
       illicit drugs since you have been in this prison?'",
       x = "'Have you developed a problem with illicit drugs since you have been in this prison?'",
       y = "Number of responses") +
  scale_x_discrete(labels=c("No", "Yes")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of drug and cellown
tabcellowndrug<-table(hmip2223$drug, hmip2223$cellown)
tabcellowndrug
addmargins(tabcellowndrug)
#Bivariate chi-sq of drug and cellown
chi_tabcellowndrug<-chisq.test(tabcellowndrug, correct = FALSE)
chi_tabcellowndrug

#Bivariate analysis of drug and mntlhl
tabmntlhldrug<-table(hmip2223$drug, hmip2223$mnthl)
tabmntlhldrug
addmargins(tabmntlhldrug)
#Bivariate chi-sq drug and mnthl
chi_tabmntlhldrug<-chisq.test(tabmntlhldrug, correct = FALSE)
chi_tabmntlhldrug


#visit----
#How often have you been able to see your family and friends in person in the last month?
J_visitoften_04

#Labelling categories
table(hmip2223$J_visitoften_04)
hmip2223$visit<-(hmip2223$J_visitoften_04)
hmip2223$visit <- factor(hmip2223$J_visitoften_04)
#Drop NAs
hmip2223$visit[hmip2223$visit==99]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$visit), ]

hmip2223$visit <- factor(hmip2223$visit)

levels(hmip2223$visit) <- c("Every week", "Two or three times", "Once", "Not at all")
table(hmip2223$visit)

#Univariate analysis of drug
tabvisit<-table(hmip2223$visit)
tabvisit
prop.table(tabvisit)
addmargins(tabvisit)

ggplot(data=hmip2223[!is.na(hmip2223$visit),], aes(x = visit)) + geom_bar(fill = "rosybrown3", 
                                                                        color="rosybrown4") +
  labs(title = "Responses to 'How often have you been able to see 
       your family and friends in person in the last month?'",
       x = "'How often have you been able to see your family and friends in person in the last month?'",
       y = "Number of responses") +
  scale_x_discrete(labels=c("Every week", "Two or three times", "Once", "Not at all")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of visit and cellown
tabcellownvisit<-table(hmip2223$visit, hmip2223$cellown)
tabcellownvisit
addmargins(tabcellownvisit)
#Bivariate chi-sq of visit and cellown
chi_tabcellownvisit<-chisq.test(tabcellownvisit, correct = FALSE)
chi_tabcellownvisit

#Bivariate analysis of visit and mntlhl
tabmntlhlvisit<-table(hmip2223$visit, hmip2223$mnthl)
tabmntlhlvisit
addmargins(tabmntlhlvisit)
#Bivariate chi-sq visit and mnthl
chi_tabmntlhlvisit<-chisq.test(tabmntlhlvisit, correct = FALSE)
chi_tabmntlhlvisit






#educ----
S_edeasy_02
#Is it easy or difficult to get into education?

#Labelling categories
table(hmip2223$S_edeasy_02)
hmip2223$educ<-(hmip2223$S_edeasy_02)
hmip2223$educ <- factor(hmip2223$S_edeasy_02)
#Drop NAs
hmip2223$educ[hmip2223$educ==99]<-NA
hmip2223 <- hmip2223[!is.na(hmip2223$educ), ]

hmip2223$educ <- factor(hmip2223$educ)

levels(hmip2223$educ) <- c("Easy", "Difficult", "Don't know", "Not available here")
table(hmip2223$educ)

#Univariate analysis of educ
tabeduc<-table(hmip2223$educ)
tabeduc
prop.table(tabeduc)
addmargins(tabeduc)

ggplot(data=hmip2223[!is.na(hmip2223$educ),], aes(x = educ)) + geom_bar(fill = "rosybrown3", 
                                                                          color="rosybrown4") +
  labs(title = "Responses to 'Is it easy or difficult to get into education?",
       x = "'Is it easy or difficult to get into education?'",
       y = "Number of responses") +
  scale_x_discrete(labels=c("Easy", "Difficult", "Don't know", "Not available here")) +
  labs(caption = "Source: HMIP Prisoner Survey 2022-23 (Author's Analysis)") 

#Bivariate analysis of educ and cellown
tabcellowneduc<-table(hmip2223$educ, hmip2223$cellown)
tabcellowneduc
addmargins(tabcellowneduc)
#Bivariate chi-sq of educ and cellown
chi_tabcellowneduc<-chisq.test(tabcellowneduc, correct = FALSE)
chi_tabcellowneduc

#Bivariate analysis of educ and mntlhl
tabmntlhleduc<-table(hmip2223$educ, hmip2223$mnthl)
tabmntlhleduc
addmargins(tabmntlhleduc)
#Bivariate chi-sq educ and mnthl
chi_tabmntlhleduc<-chisq.test(tabmntlhleduc, correct = FALSE)
chi_tabmntlhleduc



#Simple Logistic Regression Analysis of IV and DV----
simplemodel1 <- glm(mnthl ~ cellown, data = hmip2223, family = "binomial")
summary(simplemodel1)
tab_model(simplemodel1)

tab_model(simplemodel1, transform = NULL,
          dv.labels = c("Mental Health Problems"),
          pred.labels=c("Intercept","Own Cell"))

#Transform the logit coefficients into odds-ratio 
exp(cbind(OR=coef (simplemodel1), confint(simplemodel1)))

exp(0.23599)

#Testing: Multiple Logistic Regression Model (with NA categories in) ----
modeltestmult <- glm(mnthl ~ cellown + functype + age + ethnic + sentlng + unsfev + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modeltestmult)
tab_model(modeltestmult)
tab_model(modeltestmult, transform = NULL)

exp(cbind(OR=coef(modeltestmult), confint(modeltestmult)))





#Model 1: Multiple Logistic Regression Model (with NA categories out, but not removed from dataset (total is 5981)----
modeltestNAmult <- glm(mnthl ~ cellown + functype + age + ethnic + sentlng + unsfev + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modeltestNAmult)
tab_model(modeltestNAmult)
tab_model(modeltestNAmult, transform = NULL)

exp(cbind(OR=coef(modeltestNAmult), confint(modeltestNAmult)))

exp(-1.15662)


vif(modeltestNAmult)

sqrt(vif(modeltestNAmult)) >2 

exp(0.36784)

exp(0.36784)


plot_model(modeltestNAmult, title = "Odds-Ratio of MHP based on Logistic Regression Model 2",
           sort.est=FALSE)
         

tab_model(simplemodel1, modeltestNAmult, transform = NULL,
          show.se=TRUE, show.r2 = TRUE, show.aic = TRUE,
            dv.labels = c("MHP (Model 1)", 
                          "MHP (Model 2)"),
          pred.labels=c("Intercept","Own Cell", "Local Prison", "Trainers Prison", "Open - Cat D Prison",
                        "Age 26-49", "Age 50+", "Age 70+", "Black Ethnicity", "Mixed Ethnicity", "White Ethnicity",
                        "Other Ethnicity", "6m - 1yr Sentence", "1-4yr Sentence", "4-10yr Sentence", "10+yr Sentence", "IPP Sentence", 
                        "Life Sentence", "Not Currently Serving a Sentence", "Has Felt Unsafe in Prison", 
                        "Illicit Drug Problem Since Prison", "F&F Visit: 2 or 3 times", "F&F Visit: Once", "F&F Visit: No", "Accessing Education: Difficult",
                        "Accessing Educ: Don't Know", "Accessing Educ: Not Available"
                        ))


#Mod 2...
modelmulti2 <- glm(mnthl ~ cellown + age + unsfev + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti2)
tab_model(modelmulti2)
tab_model(modelmulti2, transform = NULL)
#AIC 5999.9

modelmulti3 <- glm(mnthl ~ cellown + functype + age + sentlng + unsfev + visit, data = hmip2223, family = "binomial")
summary(modelmulti3)
tab_model(modelmulti3)
tab_model(modelmulti3, transform = NULL)
#AIC 6231.7

modelmulti4 <- glm(mnthl ~ cellown + age + unsfev + visit, data = hmip2223, family = "binomial")
summary(modelmulti4)
tab_model(modelmulti4)
tab_model(modelmulti4, transform = NULL)
#AIC 6444.3

modelmulti5 <- glm(mnthl ~ cellown + functype + age + ethnic + unsfev + drug + visit, data = hmip2223, family = "binomial")
summary(modelmulti5)
tab_model(modelmulti5)
tab_model(modelmulti5, transform = NULL)
#AIC 6085.4

modelmulti6 <- glm(mnthl ~ cellown + age + sentlng + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti6)
tab_model(modelmulti6)
tab_model(modelmulti6, transform = NULL)
#AIC 6224.3

modelmulti7 <- glm(mnthl ~ cellown + ethnic + unsfev + drug, data = hmip2223, family = "binomial")
summary(modelmulti7)
tab_model(modelmulti7)
tab_model(modelmulti7, transform = NULL)
#AIC 6776.9

modelmulti8 <- glm(mnthl ~ cellown + age + unsfev + drug, data = hmip2223, family = "binomial")
summary(modelmulti8)
tab_model(modelmulti8)
tab_model(modelmulti8, transform = NULL)
#AIC 6885.7

modelmulti9 <- glm(mnthl ~ cellown + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti9)
tab_model(modelmulti9)
tab_model(modelmulti9, transform = NULL)
#AIC 6513.9

modelmulti10 <- glm(mnthl ~ cellown + age + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti10)
tab_model(modelmulti10)
tab_model(modelmulti10, transform = NULL)
#AIC: 6410.8

modelmulti11 <- glm(mnthl ~ cellown + age + sentlng + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti11)
tab_model(modelmulti11)
tab_model(modelmulti11, transform = NULL)
#AIC: 6224.3

modelmulti12 <- glm(mnthl ~ cellown + ethnic + sentlng + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti12)
tab_model(modelmulti12)
tab_model(modelmulti12, transform = NULL)
#AIC: 6145.7

modelmulti13 <- glm(mnthl ~ cellown + ethnic + sentlng + drug + educ, data = hmip2223, family = "binomial")
summary(modelmulti13)
tab_model(modelmulti13)
tab_model(modelmulti13, transform = NULL)
#AIC: 6476.5

modelmulti14 <- glm(mnthl ~ cellown + age + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti14)
tab_model(modelmulti14)
tab_model(modelmulti14, transform = NULL)
#AIC: 6246.7

modelmulti15 <- glm(mnthl ~ cellown + age + sentlng + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti15)
tab_model(modelmulti15)
tab_model(modelmulti15, transform = NULL)
#AIC: 6061.1

modelmulti16 <- glm(mnthl ~ cellown + age + ethnic + unsfev + drug + visit + educ, data = hmip2223, family = "binomial")
summary(modelmulti16)
tab_model(modelmulti16)
tab_model(modelmulti16, transform = NULL, show.se=TRUE, show.r2 = TRUE, show.aic = TRUE)
#AIC: 5821.1

exp(-1.27802)

exp(cbind(OR=coef(modelmulti16), confint(modelmulti16)))

exp(-1.92955)
exp(-0.61036)

tab_model(modelmulti16, transform = NULL,
          show.se=TRUE, show.r2 = TRUE, show.aic = TRUE,
          dv.labels = c("MHP (Model 3)"),
          pred.labels=c("Intercept","Own Cell",
                        "Age 26-49", "Age 50+", "Age 70+", "Black Ethnicity", "Mixed Ethnicity", "White Ethnicity",
                        "Other Ethnicity", "Has Felt Unsafe in Prison", 
                        "Illicit Drug Problem Since Prison", "F&F Visit: 2 or 3 times", "F&F Visit: Once", "F&F Visit: No", "Accessing Education: Difficult",
                        "Accessing Educ: Don't Know", "Accessing Educ: Not Available"
          ))

vif(modelmulti16)

sqrt(vif(modelmulti16)) >2 


exp(cbind(OR=coef(modelmulti16), confint(modeltestNAmult)))



