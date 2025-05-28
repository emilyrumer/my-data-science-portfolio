library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(tidyverse)
library(patchwork)
library(foreign)# to import datasets from other software


ess8 <- read.dta("ess_welfare.dta")

# define the welfare scale as 1 to 5 by averaging
ess8$welfare<-(ess8$welfare)/9

# define country as factor
ess8$cntry<-as.factor(ess8$cntry)

# select only 4 countries for simplicity
ess <- ess8[ess8$cntry=="DE" |
              ess8$cntry=="DK" |
              ess8$cntry=="IE" |
              ess8$cntry=="ES",]

#CONTINUOUS VARIABLES----

#Boxplot - geom_boxplot()
#Use to summarise distribution of continuous variable using 5-number summary (minimum, 
# 1st Q (25%), median (50%), 3rd Q (75%) and max). Also good to plot outliers
ggplot(data=ess, aes(y=welfare)) + geom_boxplot()

#Can also use for the distribution of the same continuous variables across the levels of a categorical variable (country)
ggplot(data=ess, aes(y=welfare, x=cntry)) + geom_boxplot()
#by default these are ordered alphabetically, to order by median or cntry check WSGuide 
#FIX
ggplot(data=ess, aes(y=welfare, x=fct_reorder(cntry, welfare, .fun = median, na.rm = TRUE))) + geom_boxplot()


#Histogram - geom_histogram()
#Use to summarise distribution of continuous variable by plotting frequency of observations or data points
#(e.g. respondents) across diff levels of the contin. varia. which are grouped in discrete intervals of fixed width
ggplot(data=ess, aes(x=welfare)) + geom_histogram()

#To present bivariate distributions between a continuous v and categorical predictor, can overlay multiple histograms (might not look clear for more than 2 categories)
ggplot(data=ess, aes(x=welfare, fill= cntry)) + geom_histogram()

#Can plot multiple histograms side by side (one per each cat/level) of the cat.v. Don't forget ~ sign
ggplot(data=ess, aes(x=welfare)) + geom_histogram() + facet_wrap(~cntry)


#Density Plot - geom_density()
#Use to summarise distribution of contin.v by plotting the density or concentration of observations across diff levels (rather than discrete bins) of the contin.v
ggplot(data=ess, aes(x=welfare)) + geom_density()

#Can also plot bivariate density plots by specifying colour line is set to a categ. factor v. 
ggplot(data=ess, aes(x=welfare, colour=cntry)) + geom_density()
#could use facet_wrap again to separate graphs but usually unecessary in density plots as only one line, unless there are so many lines you can't see them all


#Scatterplot
#Plots how a continuous variable varies across the values of anothe continuous variable
#Before building, should think through what dependent and independent variables are
#For instance, you want to know how overall welfare attitudes (depd) vary as age (indepd) increases. Specify agea on x-axis and welfare on y-axis, then ask for point geom to be plotted on space defined by the axis.
ggplot(data = ess, aes (x=agea, y=welfare)) +geom_point()

#Can add a line of best fit or regression line (the predicted values for y based on x from a linear regression model) that summarises the underlying
# pattern of association between age and welfare attitudes. Key is correctly specifying depd (y) and indepd(x) are. After this, just add 'geom_smooth(method = "lm")'
ggplot(data = ess, aes (x=agea, y=welfare)) + geom_point() +
  geom_smooth(method = "lm")

#Can examine how that relationship varies across country AND gender using facet_grid
ggplot(data = ess, aes (x=agea, y=welfare)) + geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(vars(gndr), vars(cntry))


#CATEGORICAL VARIABLES----

#Univariate Bar Charts

#To plot number/count/frequency use following:
ggplot(data=ess, aes(x = sblazy)) + geom_bar()

#by default, ggplot produces bar for NAs. Shouldn't deny they exist, but often don't want to show them. To exclude them, subset data using "data[!is.na(data$variable),]
ggplot(data=ess[!is.na(ess$sblazy),], aes(x = sblazy)) + geom_bar()
#can see NA has gone. from now we introduce this subsetting code in the definition of dataset

#To plot proportion of respondents that choose each category we slightly tweak code. Add option 'y = ..prop.., group = 1' which transforms counts into proportions
ggplot(data=ess[!is.na(ess$sblazy),], aes(x = sblazy, y = ..prop.., group = 1)) + geom_bar()

#Rather than proportions, to show percentages, can specify + scale_y_continuous(labels = scales::percent) to re-scale y-axis
ggplot(data=ess[!is.na(ess$sblazy),],
       aes(x = sblazy, y = (..prop..), group = 1)) +
  geom_bar() +
scale_y_continuous(labels = scales::percent)


#Bivariate Bar Charts
#Looks at how distribution varies across categories of indepd. v

#Stacked Bar Chart
#Plots one bar per each categ of indepd v and each bar will be subdivided in segments representing freqs of proportions of obsvs

#For freq counts:
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) +
  geom_bar()

ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla) + 
         (fill=sblazy)) +
  geom_bar()


#stacked bar chart
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar()


#For proportions/%
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)

#Clustered bar chart
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + geom_bar(position = "dodge") 

#Adding third indepnd variable
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + facet_wrap(~cntry) 

#CHLOROPLETH MAPS----
#Use to represent features that have a geographical or spatial basis

#retrieves coordinates and polygons of a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# selects only European countries
europe <- 
  world %>% 
  filter(world$continent == "Europe") 

#Simple features (sf) object called europe includes geometry variable which defines countries boundaries
europe%>%
  select(sovereignt, geometry)

#Can plot this country boundaries or polygons using ggplot in combo with sf package
europe%>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)
#This crops map to include only part of Russia

#calculate mean welfare scores for each country and merge info with spatial data created above:
  #calculate country means
ess_cntry <-
  ess8 %>%
  select(cntry, welfare) %>%
  group_by(cntry) %>%
  summarize(natwelf = mean(welfare, na.rm = TRUE)) %>%
  mutate(country = recode(cntry, 
                          "IE"="Ireland",
                          "DE"="Germany",
                          "DK"="Denmark",
                          "ES" = "Spain",
                          "FR" = "France",
                          "GB" = "United Kingdom",
                          "GR" = "Greece",
                          "SE" = "Sweden"
                          ))
ess_cntry

  #merge
map_ess<-
  left_join(europe, ess_cntry, by = c("sovereignt"="country"))

map_ess %>%
  select(sovereignt, geometry, natwelf)

#can then map by specifying fill within geom_sf aesthetics
map_ess %>%
  ggplot() +
  geom_sf(aes(fill = natwelf, )) +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)


#CUSTOMISING PLOTS WITH OPTIONS----

#Main Title
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_bar(position = "fill") +
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries (Pooled)")  

#Axis Titles to x and y:      xlab(“text”) and ylab(“text”)
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries") +
  xlab("Educational Qualifications") + ylab("Proportion of Respondents")

#More systematic way of doing this using labs function    
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") +
  labs(title = "Social benefits make people lazy' by Education in 4 EU countries", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents")

#Adding caption to include relevant info (e.g. data source):   labs(caption = “text”)
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") +
  labs(title = "Social benefits make people lazy' by Education in 4 EU countries", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents",
       caption = "Source: ESS (2008) - Author's own analysis")

#Axis tick labels - they might be too long and overlap:     scale_x_discrete(labels=c())
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") +
  labs(title = "Social benefits make people lazy' by Education in 4 EU countries", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6"))

#Rotate axis tick labels:    theme(axis.text.x = element_text(angle = 90))
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  labs(title = "Social benefits make people lazy' by Education in 4 EU countries", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents") +
  theme(axis.text.x = element_text(angle = 90))

#Change or remove legend title (this is the one which fills the graphs with colour codes)

  #change:    guides(fill=guide_legend(title=“New Legend Title”))
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries (Pooled)") +
  xlab("Educational Qualifications") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  guides(fill=guide_legend(title="Social benefits \nmake people lazy"))

  #remove:    theme(legend.title=element_blank())
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries (Pooled)") +
  xlab("Educational Qualifications") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  theme(legend.title=element_blank()) 

#change position of legend: legend.position
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries (Pooled)") +
  xlab("Educational Qualifications") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  theme(legend.title=element_blank(), 
        legend.position = "bottom" ) 

#Switching from colour to black and white using: scale_fill_grey()
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  ggtitle("'Social benefits make people lazy' by Education in 4 EU countries (Pooled)") +
  xlab("Educational Qualifications") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  theme(legend.title=element_blank()) +
  labs(caption = "Source: ESS (2008) - Author's own analysis") +
  scale_fill_grey()

#Using colour-blind scheme for accessibility - viridis
ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  theme(legend.title=element_blank()) +
  labs(caption = "Source: ESS (2008) - Author's own analysis") +
  scale_fill_viridis_d()


#COMBINING PLOTS: PATCHWORK----

#Use to combine multiple plots often different data or variables, into a single file
#Do this by creating 2 separate plots and store them as objects and combine using patchwork
plot1 <-ggplot(data=ess[!is.na(ess$sblazy) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sblazy)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  labs(title = "Make people lazy", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents") +
  theme(legend.title=element_blank()) +
  scale_fill_viridis_d()

plot2 <-ggplot(data=ess[!is.na(ess$sbprvpv) & !is.na(ess$edulvla),], aes(x = edulvla, fill=sbprvpv)) + 
  geom_bar(position = "fill") + 
  labs(title = "Prevent widespread poverty", 
       x = "Educational Qualifications",
       y = "Proportion of Respondents") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  theme(legend.title=element_blank()) +
  scale_fill_viridis_d()

plot1 + plot2 + 
  labs(caption = "Source: ESS (2008) - Author's own analysis") +
  plot_annotation(title = "Attitudes towards social benefits") +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom') 


#Can combine any number of plots and arrange in diff ways, maybe plot bar plots just created alongside a plot representing average (continuous) welfare scores for each occupational group
plot3 <-
  ess %>% 
  filter(!is.na(edulvla)) %>% 
  group_by(edulvla)%>% 
  summarize(
    meanwelf = mean(welfare, na.rm = TRUE),
    sd = sd(welfare, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n)) %>% 
  ggplot(aes(x=edulvla, y = meanwelf)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = meanwelf - 1.96*se, ymax = meanwelf + 1.96*se),
    width = 0.1,
    linetype = "dotted") +
  scale_x_discrete(labels=c("ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4", "ISCED 5-6")) +
  labs(title = "Overall Support for Welfare", 
       x = "Educational Qualifications",
       y = "Pro-welfare scale") 


plot3 + (plot1 / plot2) + 
  labs(caption = "Source: ESS (2008) - Author's own analysis") +
  plot_annotation(title = "Attitudes towards social benefits") +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom') 