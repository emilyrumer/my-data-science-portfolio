#Libraries----
library(corrr)
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
library(dplyr)
library(factoextra)
library(lubridate)
library(ggmap)
library(psych)
library(MASS)
library(mice)

install.packages("psych")

options(scipen=999, digits=5)


#individual 
cntry     #country
gndr      #gender
agea      #agea

#job
stfmjob   #how satisfied are you in main job
hinctnta  #household total net income, all sources
wkhtot    #total hours normally worked per week in main job OT included

#manager
mansupp   #line manager gives work-related help, how likely
manhlp    #line manager gives work-related help, how likely

#autonomy
trdawrk   #tired after work
dcsfwrka  #current job can decide start/finish work time
wrkhome   #work from home or place of choice, how often



ESS10 <- read.csv("D:/Users/emilyrumer/~/Documents/UNI/YEAR 3/LAW3287 Quantitative Social Research 2/ASSESSMENT/Q2 R PCA/ESS10/ESS10.csv",
                  header=TRUE, stringsAsFactors=FALSE)
head(ESS10)


library(readr)
ESS10 <- read_csv("ESS10/ESS10.csv")
View(ESS10)


#creating new dataset
ESS10var <- ESS10

#creating new dataset with needed variables
data = c("cntry","agea","gndr",
         "stfmjob","hinctnta","wkhtot","dcsfwrka",
         "wrkhome","trdawrk","mansupp","manhlp")

ESS10var = ESS10var[data]

#shows the start of variables
str(ESS10var)

read.csv('ESS10var.csv', row.names = 1)

setwd('ESS10')

# analysis----

summary(ESS10var)
#checking for any NA
colSums(is.na(ESS10var))


#Cleaning data, removing not applicables----

#country - need to make it numeric

addmargins(table(ESS10imp$cntry))

#cntry to factor
ESS10imp$cntry <- as.numeric(ESS10imp$cntry)
class(ESS10imp$cntry)


#gender
ESS10imp
addmargins(table(ESS10imp$gndr))

table(ESS10imp$gndr, useNA="ifany")


#job satisfaction
addmargins(table(ESS10var$stfmjob))

ESS10var$stfmjob[ESS10var$stfmjob>10]<-NA
table(ESS10var$stfmjob, useNA="ifany")


#age
addmargins(table(ESS10var$agea))

ESS10var$agea[ESS10var$agea>90]<-NA
table(ESS10var$agea, useNA="ifany")


#impute: income
addmargins(table(ESS10var$hinctnta))

ESS10var$hinctnta[ESS10var$hinctnta>10]<-NA
table(ESS10var$hinctnta, useNA="ifany")

addmargins(table(ESS10$hinctnta))



#impute: hrs worked with OT
addmargins(table(ESS10var$wkhtot))

ESS10var$wkhtot


ESS10var$wkhtot[ESS10var$wkhtot>169]<-NA
table(ESS10var$wkhtot, useNA="ifany")

quantile(ESS10var$wkhtot, c(.90, .95, .99, .999), na.rm = TRUE)

#capping at 99th percentile
ESS10var$wkhtot[ESS10var$wkhtot<0]<-NA
ESS10var$wkhtot[ESS10var$wkhtot>84]<-84

ggplot(data=ESS10var, aes(x=wkhtot)) + geom_histogram() +
  xlab("hours worked") +
  ylab("Number of observations")

addmargins(table(ESS10$wkhtot))




#start finish time decide     
addmargins(table(ESS10var$dcsfwrka))

ESS10var$dcsfwrka[ESS10var$dcsfwrka>6]<-NA
table(ESS10var$dcsfwrka, useNA="ifany")


#work from home decide     
addmargins(table(ESS10var$wrkhome))

ESS10var$wrkhome[ESS10var$wrkhome>6]<-NA
table(ESS10var$wrkhome, useNA="ifany")


#tired after work     
addmargins(table(ESS10var$trdawrk))

ESS10var$trdawrk[ESS10var$trdawrk>5]<-NA
table(ESS10var$trdawrk, useNA="ifany")


#manager supports work life balance    
addmargins(table(ESS10var$mansupp))

ESS10var$mansupp[ESS10var$mansupp>65]<-NA
table(ESS10var$mansupp, useNA="ifany")
# 3816 have said I don’t have a line manager and are coded as 55
addmargins(table(ESS10imp$mansupp))


#manager helps with work    
addmargins(table(ESS10var$manhlp))

ESS10var$manhlp[ESS10var$manhlp>5]<-NA
table(ESS10var$manhlp, useNA="ifany")


#getting rid of NAs from all of the above, minus hours worked and income
ESS10var<-ESS10var[complete.cases(ESS10var$agea), ]
ESS10var<-ESS10var[complete.cases(ESS10var$stfmjob), ]
#not doing income
#not doing hours worked
ESS10var<-ESS10var[complete.cases(ESS10var$dcsfwrka), ]
ESS10var<-ESS10var[complete.cases(ESS10var$wrkhome), ]
ESS10var<-ESS10var[complete.cases(ESS10var$trdawrk), ]
ESS10var<-ESS10var[complete.cases(ESS10var$mansupp), ]
ESS10var<-ESS10var[complete.cases(ESS10var$manhlp), ]



#Multiple imputation missing data for income and working hours:

md.pattern(ESS10var, rotate.names=TRUE) #To visualise the missing data patterns.


#The multiple imputation process.
miESS10 = mice(ESS10var,m=5,meth='pmm',seed=7)
miESS10$imp$hinctnta
miESS10$imp$wkhtot
#The following is to check the first 20 cases for the complete first dataset imputed. 
complete(miESS10,1)[1:100,]

#rename - this is new dataset with imputed data
ESS10imp <- complete(miESS10,1)


#check no NAs
colSums(is.na(ESS10imp))



# Renaming new variables - these are as were originally
names(ESS10imp) <- c("cntry", "agea", "gndr", "stfmjob", 
                     "hinctnta", "wkhtot",
                     "dcsfwrka", "wrkhome", 
                     "trdawrk", "mansupp", 
                     "manhlp")


ESS10imp$cntry <- NULL

#Standardising variables

ESS10imp$cntry <- ESS10var$cntry

str(ESS10imp)

summary(ESS10imp)

apply(ESS10imp, 2, var)

ESS_std = scale(ESS10imp)

ESS_std

#checks structure - is a matrix
str(ESS_std)

summary(ESS_std)
sd(ESS_std)

print(ESS_std)

# Convert matrix to data frame
ESS_std_df <- as.data.frame(ESS_std)


summary(ESS_std_df$cntry)


#PCA TESTING----

ESS10imp


colSums(is.na(ESS_std_df))

head(ESS_std_df)

summary(ESS_std_df)


cov_matrix = cov(ESS_std_df)
cov_matrix

ggcorrplot(cov_matrix)






# Load necessary libraries
library(ggcorrplot)

# Assuming cov_matrix is your covariance matrix


# Original variable names (optional step to show the initial state)
print("Original column and row names:")
print(colnames(cov_matrix))
print(rownames(cov_matrix))

# New names for the variables
new_names <- c("Age", "Gender", "Job Satisfaction", "Household Income", "Total Hours Worked Per Week", "Can Decide Start and Finish Time", "Can Decide Place of Work", "Too Tired After Work", "Manager Supports Balancing Work/Life", "Manager Supports in Work")

# Rename the columns and rows of the covariance matrix
colnames(cov_matrix) <- new_names
rownames(cov_matrix) <- new_names

# Print to verify the change
print("Modified column and row names:")
print(colnames(cov_matrix))
print(rownames(cov_matrix))

# Plot the covariance matrix with ggcorrplot
ggcorrplot(cov_matrix, colnames(TRUE), rownames(TRUE))




data_pca = princomp(cov_matrix)
summary(data_pca)

fviz_eig(data_pca, addlabels = TRUE)


data_pca$loadings[, 1:4]

fviz_pca_var(data_pca, col.var = "black")



#checking for multicollinearity
library(car)
multicolmod <- lm(stfmjob ~ agea + gndr + hinctnta + wkhtot + dcsfwrka + wrkhome + trdawrk + mansupp + manhlp, data=ESS_std_df)
vif(multicolmod)




pcak_transform = as.data.frame(-data_pca$x[,1:4])



#K-means----

head(ESS_std_df)

#Plot the quality of clustering
scree <- fviz_nbclust(ESS_std_df, kmeans, method = "wss")

scree



clusters <- 10

TWSS <- numeric(clusters)

scree +
  geom_hline(
    yintercept = TWSS,
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )


4
set.seed(5) #Set the random seed.

km = kmeans(ESS_std_df, centers = 4, nstart = 25) 
#Execute K-means with pre-determined pa km 
#View algorithm output
km

summary(as.factor(km$cluster))


fviz_cluster(km, data = ESS_std_df)


aggregate(ESS10imp, by=list(cluster=km$cluster), mean) #Find the means of each variable


final_data = cbind(ESS10imp, cluster = km$cluster) #Add a variable to the original dataset 
head(final_data)


ESS10imp$cntry <- ESS10var$cntry


#FINAL DATA WITH COUNTRY
final_data_with_cntry = cbind(ESS10imp, ESS10imp$cntry, cluster = km$cluster) #Add a variable to the original dataset 
head(final_data_with_cntry)


addmargins(table(ESS10var$cntry))





#doesnt work with country
final_with_cntry <- cbind(ESS10var$cntry, ESS_std_df, cluster = km$cluster)

final_with_cntry




ESS_std_df$cluster <- km$cluster

cntry_cluster_table <- table(ESS_std_df$cntry, ESS_std_df$cluster)
cntry_cluster_table

country_cluster_df <- as.data.frame(cntry_cluster_table)
names(country_cluster_df) <- c("Country", "Cluster", "Count")

print(country_cluster_df)


ESS_std_df <- as.data.frame(ESS_std)
ESS_std_df$cntry <- df$cntry



#go back and do with row names as countries
read.csv('ESS10imp', row.names = 1)


#Europe map of JS ----

library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(tidyverse)
library(patchwork)
library(foreign)
library(scales)
library(dplyr)

install.packages("dplyr")

#retrieves coordinates and polygons of a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# selects only European countries
europe <-
  world %>%
  filter(world$continent == "Europe")

europe %>%
  select(sovereignt, geometry)



europe %>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)


# calculate country means
ess_cntry <-
  ESS10var %>%
  dplyr::select(cntry, stfmjob) %>%
  group_by(cntry) %>%
  summarize(natjbsf = mean(stfmjob, na.rm = TRUE)) %>%
  mutate(country = recode(cntry,
                          "BE" = "Belgium",
                          "BG" = "Bulgaria",
                          "HR" = "Croatia",
                          "CZ" = "Czechia",
                          "EE" = "Estonia",
                          "FI" = "Finland",
                          "FR" = "France",
                          "GR" = "Greece",
                          "HU" = "Hungary",
                          "IS" = "Iceland",
                          "IE" = "Ireland",
                          "IT" = "Italy",
                          "LT" = "Lithuania",
                          "ME" = "Montenegro",
                          "NL" = "Netherlands",
                          "MK" = "North Macedonia",
                          "NO" = "Norway",
                          "PT" = "Portugal",
                          "SK" = "Slovak Republic",
                          "SI" = "Slovenia",
                          "CH" = "Switzerland",
                          "GB" = "United Kingdom"))
                          
ess_cntry
                          
colnames(ESS10imp)
