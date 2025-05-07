# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions below
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions below

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`
hts <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T))

# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results

library(data.table)
library(ggplot2)

income<-t.test(hhincome ~ sf, data=hts, var.equal=F)
income

income_histogram<-ggplot(hts, aes(x=hhincome))+
  geom_histogram() +
  facet_wrap(~sf)
ggsave("hhincome_histogram.pdf", income_histogram)

curve(dt(x, df=income$parameter), from =-40, to =40)
abline(h=0, col='blue')
points(x= income$statistic, y=0, col='red')
upper975 <- qt(p = .975, df = income$parameter)
abline(v = upper975,col='red')
lower025 <- qt(p = .025, df = income$parameter)
abline(v = lower025,col='red')

# The t-test shows a significant difference in the mean household incomes between single family households and not single family households: t = -36.23, p-value < 2.2e-16. Because the p-value is less than .05 and t is negative, we reject the null hypothesis and conclude that single-family households tend to have significantly higher incomes than those in other types of housing.


# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

sa_hts<-hts[region%in%c("San Antonio, TX")]
median_jobpop <- median(sa_hts$jobpop, na.rm = TRUE)
sa_hts$jobpop_group <- ifelse(sa_hts$jobpop > median_jobpop, "Above Median", "Below Median")

lnvmt_ttest <- t.test(lnvmt ~ jobpop_group, data = sa_hts, var.equal = F)
lnvmt_ttest

#Since lnvmt_ttest calculates a p-value of .02759, which is less than .05, we reject the null hypothesis; since t = -2.2052, which is negative, we conclude that households in neighborhoods with a jobpop index below the city median tend to have significantly higher household vehicle miles traveled. 

# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.

normality_histogram<-ggplot(sa_hts, aes(x=lnvmt)) +
  geom_histogram()
normality_histogram

outliersplot<-ggplot(data=sa_hts, aes(x=income_cat, y=lnvmt)) +
  geom_boxplot()
outliersplot
ggsave("outliersplot.pdf",plot=outliersplot)

sa_hts_bp<-boxplot(sa_hts$lnvmt~sa_hts$income_cat)
outliers<-sa_hts_bp$out
sa_hts[lnvmt%in%outliers,]
sa_hts2<-sa_hts[!lnvmt%in%outliers,]

boxplot(sa_hts$lnvmt~sa_hts$income_cat)
boxplot(sa_hts2$lnvmt~sa_hts2$income_cat)

no_outliersplot<-ggplot(sa_hts2, aes(x=income_cat, y=lnvmt))+
  geom_boxplot()
no_outliersplot
ggsave("no_outliersplot.pdf",plot=no_outliersplot)

normality_histogram2<-ggplot(sa_hts2, aes(x=lnvmt))+
  geom_histogram()
normality_histogram2
ggsave("notquitenormalbutbetterthanbefore.pdf",plot=normality_histogram2)

anova<-aov(lnvmt~income_cat, data=sa_hts2)
summary(anova)

tukey<-TukeyHSD(anova)
tukey_plot<-plot(tukey)

# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.



