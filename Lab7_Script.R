# Lab 7 Script: Diff-in-means & ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn to apply a Difference in Means test (T-test)
# 2. Learn to apply a ANOVA test (F-test)

#---- 1. Learn to apply Difference in Means test (T-test) ----

#Steps:
 ## 1. Check your data meets the T-test assumptions: (independence, normality, equal variance)
 ## 2. Confirm the dependent variable is continuous, and independent variable has two categories only
 ## 3. Establish a H0
 ## 4. Conduct an T-test for independent samples
 ## 5. Interpret results

library(data.table) #loads the pacakge data.table
library(foreign) #loads the package foreign

hts <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T)) #reads the SPSS file as a data frame and puts it in a data table format

# step 1 - Independence: there is no reason to think that the VMT values between these two regions are not independent
hts[,.N,by=region] #counts the number of observations for each region
hts<-hts[region%in%c("Seattle, WA","Kansas City, MO")] #filters the data to include only Seattle and KC

# step 1 - Normality: Histogram plots look pretty normal

library(ggplot2) #loads the ggplot2 package

ggplot(data=hts,aes(x=lnvmt))+
  geom_histogram()+
  facet_grid(region~.) #creates a histogram of values in the column lnvmt for each region in the plots tab


# step 1 - Variance: Box-plot looks pretty normal

ggplot(data=hts,aes(x=lnvmt, y=region))+
  geom_boxplot() #creates a boxplot of lnvmt for each region in the plots tab

 # desc stats
hts[,.(mean=mean(lnvmt,na.rm=T),sd=sd(lnvmt,na.rm=T),Obs=.N),by=.(region)] #calculates the mean, standard deviation, and number of observations for the values in lnvmt for each region and prints it in the console


#Step 2: dependent variable types
class(hts$lnvmt) ; class(hts$region) #checks the datatypes of columns lnvmt and region in the object hts and prints them in the console

# Step 3: H0: t-statistic = 0 (Mean_Seattle - Mean_Kansas = 0)

# Step 4: Conduct t-test
two_tailed_t_test<-hts[,t.test(formula = lnvmt ~ region)] # two-tailed
two_tailed_t_test #runs a two-tailed t test to compare lnvmt in the regions; creates a list object  in environment and then prints results in console

one_tailed_t_test<-hts[,t.test(formula = lnvmt ~ region,alternative = 'greater')] # one-tailed
one_tailed_t_test #runs a one-tailed t test to see if lnvmt in one region is greater than the other; creates a list object in environment and then prints results in console

# Step 5. Interpret results

# Both T-test (one and and two-tailed are) statistically significant (p-value <0.05 & 0.025), which implies that the H0 can be rejected. Hence we can state that the difference in vehicle miles traveled between the two regions exist is not due to chance

# BONUS: Simulated hypothesis testing plot ----
curve(dt(x, df = 5934.7), from = -10, to = 10)
abline(h=0,col='blue')
points(x=two_tailed_t_test$statistic,y=0,col='red')
#plots the distribution of t with 5934.7 degrees of freedom (df value calculated from previous t-tests), drawns a blue line at y=0, and marks in red the point where the t values from the two tailed test are on the plot

##upper value from Chi-Squared Dist (1-alpha) with alpha=0.05
upper975 <- qt(p = .975, df = 5934.7)
abline(v = upper975,y=0,col='red')
#calcs the 97.5% quantile of the distribution and draws a red line there

lower025 <- qt(p = .025, df = 5934.7)
abline(v = lower025,y=0,col='red')
#calcs the bottom 2.5% quantile of the distribution and draws a red line there


#---- 2. Learn to apply a ANOVA test (F-test) ----

# Steps:
 ## 1. Ensure the ANOVA Assumptions are meet
 ## 2. Ensure variable types
 ## 3. Propose H0: no difference between groups
    #  transit passenger miles (lntpm) ~ region
 ## 4. Conduct one-way ANOVA
 ## 5. Interpret results
 ## 6. Conduct post-hoc tests


uza <- data.table(read.spss("datasets/UZA.sav",to.data.frame = TRUE)) #creates object uza from spss file brought in as data table


# Step 1: Independence: There is no reason to think that TPM and region are dependent
  # in fact a quick plot of the sample means reveal great difference between groups

ggplot(data=uza[,.(Mean_lntmp=mean(lntpm)),by=region], aes(x=region,y=Mean_lntmp))+
  geom_point()
#creates plot of the mean lntmp for each region 

# Step 1: too many outliers?: There is no reason to think that TPM and region are dependent
  #box plot shows that there are some outliers in the Midwest and South regions. Although using the LN transformation helps here, lets delete those observations)

ggplot(data=uza, aes(x=region, y= lntpm))+
  geom_boxplot()
#creates a boxplot for lntpm for each region and shows where the outliers are
  
  # deleting outliers 
uza_bp<-boxplot(uza$lntpm~uza$region) #turns the boxplot of lntpm by region into object uza_bp 

outliers <- uza_bp$out #creates object outliers to contain the values in list "out" from usa_bp object

uza[lntpm%in%outliers,] #filters to match the values in uza lntpm to the values in object outliers
uza2<-uza[!lntpm%in%outliers,] #creates new object uza2 and removes the outlier values

boxplot(uza$lntpm~uza$region) #boxplot with outliers
boxplot(uza2$lntpm~uza2$region) #boxplot without outliers (but the low one from the south is still there?)

# Step 1: dependent variable is normal: 

hist(uza2$lntpm) # looks pretty normal to me!
#plots a histogram of the column lntpm in object uza2

# Step 1: variance homogeneity?
bartlett.test(lntpm ~ region, data=uza2) # H0: variances are equal: p-value ==> accepts
#runs a bartlett test in console to check if variances are equal across groups

# Step 4: one-way anova

fit<-aov(uza2$lntpm~uza2$region) #creates object fit containing results of anova test comparing the lntpm values in each region
summary(fit) #prints a summary of the object in the console

#post-hoc test
TukeyHSD(fit) #runs tukey's honest significant difference test to isolate which specific groups have the least/greatest differences

plot(TukeyHSD(fit)) #plots the results of TukeyHSD to compare differences in each pair-up of regions


