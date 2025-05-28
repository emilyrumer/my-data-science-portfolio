library(tidyverse)
library(psych)

load("lfs(1).rda")

#find mean of hourly pay in dataset
mean(lfs$HOURPAY)

#finds the median
median(lfs$HOURPAY)

#treating all values of negative as missing values
lfs$HOURPAY[lfs$HOURPAY < 0] <- NA

#remove missing values from mean (NA, remove)
mean(lfs$HOURPAY, na.rm = TRUE)

#remove missing values from median
median(lfs$HOURPAY, na.rm = TRUE)


#describe is in psych library - it gives more info of the variable
  #when this code is ran, it shows the sample size for the variable 'hourly pay' as 11,635, 
  #this is less than the total 102.059 obs, as this is how many answered this variable
describe(lfs$HOURPAY)

#make histogram of hourly pay 
hist(lfs$HOURPAY, na.rm = TRUE)

#capping the hourly pay, anything in the top 1% is ignored 
quantile(lfs$HOURPAY, .99, na.rm = TRUE)

#capping the hourly pay, anything in the top 0.01% is ignored 
quantile(lfs$HOURPAY, .999, na.rm = TRUE)

#if salary is greater than £90ph, cap them at £90ph (instead of removing them)
lfs$HOURPAY[lfs$HOURPAY > 90] <- 90

#check max value is now 90, yes it is
describe(lfs$HOURPAY)

#make histogram to see new distribution with capped hourly pay
hist(lfs$HOURPAY)


#describe by the categories in the code (hp and sex) 
by(lfs$HOURPAY, lfs$SEX, describe)
#observations:
#men have higher salaries (higher mean and median)
#greater inequalities of earnings among men than women (shown by SD)

lfs |> 
  drop_na(HOURPAY) |>           #drop missing values for NA
  ggplot(aes(x=HOURPAY)) +      #plot hour pay on X-axis
  geom_histogram()              #run histogram plot

lfs |> 
  drop_na(HOURPAY) |>           #drop missing values for NA
  ggplot(aes(x=HOURPAY)) +      #plot hour pay on X-axis
  geom_density()                #run density plot (smooth histogram)
#notice bump at the end for £90 capped values

#density plot with 2 lines to represent hourpay by gender
lfs |> 
  drop_na(HOURPAY) |>           
  ggplot(aes(x=HOURPAY, colour = SEX)) +       #specify
  geom_density()      
#result: women's peak is broader and more to the left as their earnings are more compressed than men's
#        men's values are across a broader spectrum, more uncertainty in what their wage will be

#boxplot
lfs |> 
  drop_na(HOURPAY) |>           
  ggplot(aes(x=HOURPAY, colour = SEX)) +      
  geom_boxplot()  




