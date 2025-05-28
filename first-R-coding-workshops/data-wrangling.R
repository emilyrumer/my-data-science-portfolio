library(tidyverse)
library(psych)

load("bsaw5.Rdata")

#Scenario1
#sub-setting to include data which only fulfills a certain criteria (respondent's age)
table(bsa$RAge)

#includes 18 to 25
#if you don't include comma at end, it will not work (it indicates you are sub-setting the rows)
                                               #(this is because it is in matrix notation with sq brackets)
bsa[bsa$RAge >= 18 & bsa$RAge <=25,]

#shorten the above
bsa1825 <- bsa[bsa$RAge >= 18 & bsa$RAge <=25,] 
#(has the same number of variable's, but diff number of rows to main dataset as is a sub-set)

#run a table again, with the new dataset (bsa1825) to show only these ages
table(bsa1825$RAge)

#using tidyverse to make the age-filtered variable (t at the end to distinguish and not override previous bsa1825 variable)
bsa1825t <- 
  bsa |> 
  filter(bsa$RAge >= 18 &
           bsa$RAge <=25)
  
#different way of sub-setting age (t2 at end of bsa1825)
bsa1825t2 <- 
  bsa |> 
  filter(between(RAge, 18,25))

#note on tidyverse: these functions may change or no longer be maintained, must keep updated to use it


#makes female variable. Needs "" as 'Female' is a string of text
bsa_f <- 
  bsa[bsa$RSex == "Female",]

#comparing an outcome (education) between groups - can be used to emphasise difference
table(bsa$HEdQual)



# Re-coding a categorical variable
bsa$educ4 <- NA #this is redundant because all the new variables are recoded???

bsa$educ4[bsa$HEdQual == "Degree"] <- 1

#run table to check
table(bsa$educ4) #if there are discrepancies between code
# 714 in degree in
table(bsa$HEdQual)


bsa$educ4[bsa$HEdQual == "Higher educ below degree" |
            bsa$HEdQual == "A level or equiv"] <-2

bsa$educ4[bsa$HEdQual == "CSE or equiv" |
            bsa$HEdQual == "Foreign or other" |
            bsa$HEdQual == "O level or equiv"] <- 3
#***in the above be careful with 0 or O for O-level

bsa$educ4[bsa$HEdQual == "No qualification"] <- 4

#check the new values
table(bsa$educ4)

#check the new table
table(bsa$HEdQual, bsa$educ4)

#factors are categorical variables in R. answer is false
is.factor(bsa$educ4)

#need to transform variable to a factor so R can process it as a categorical variable
bsa$educ4 <- as.factor(bsa$educ4)

#answer is true - we have written over variable we just created, it is not adding new variable, just swapping to a factor
is.factor(bsa$educ4)

#telling R that when we print the table, the specific levels assigned will be seen (specify in order of what 1,2,3,4 was)
levels(bsa$educ4) <- c("Degree",
                      "Higher SE",
                      "Lower SE",
                      "No quals")
#check new labels
table(bsa$educ4)







#Tidyverse way of re-coding categorical variable
bsa1 <-
bsa |> 
  mutate(educ4t = recode(HEdQual,
                         "Degree" = "Degree",
                         "Higher educ below degree" = "Higher SE",
                         "A level or equiv" = "Higher SE",
                         "O level or equiv" = "Lower SE",
                         "CSE or equiv" = "Lower SE",
                         "Foreign or other" = "Lower SE",
                         "No qualification" = "No qual")) |> 
  mutate(educ4t = na_if(educ4t, "DK/Refusal/NA")) |>  #na_if means assign missing values if a condition is met
  mutate(educ4t = droplevels(educ4t))
  
  
table(bsa1$educ4t) #still has the DK/Refusal/NA section, but at 0. To get rid of, use 'droplevels' (was added to above)

#mutate function adds columns to the dataset at the end with the new variable educ4 and educ4t



#Scenario2
#Re-coding a continuous value into a factor (such as age, when it is continuous rather than in bands)

#see ages in list
table(bsa$RAge)
#ask if age is a numeric variable
is.numeric(bsa$RAge)

bsa$age4cat <- NA
bsa$age4cat[bsa$RAge<=24] <- 1
bsa$age4cat[bsa$RAge>=25 & bsa$RAge <=39] <- 2
bsa$age4cat[bsa$RAge>=40 & bsa$RAge <=64] <- 3
bsa$age4cat[bsa$RAge>=65 & bsa$RAge <=97] <- 4
#be very clear about where categories begin and end so they dont overlap with the next category - it will create errors


#check new categories
table(bsa$age4cat)

#turn into a factor
bsa$age4cat <- as.factor(bsa$age4cat)

#rename groups (in order of 1,2,3,4)
levels(bsa$age4cat) <- c("Under 25", "25 to 39", "40 to 64", "65+")

#see table with labels and ages
table(bsa$RAge, bsa$age4cat)

table(bsa$age4cat)
table(bsa$RAge, bsa$age4cat)

#recode continuous with tidyverse:
bsa |> 
  mutate(age4cat2 = cut(RAge, 
                        breaks = c(0, 24, 39, 64, 98))) |> 
           count(age4cat2)

