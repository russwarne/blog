##Import libraries
library(psych)
library(tidyverse)

##Import unweighted data file
setwd("C:/Users/russw/OneDrive/Documents/UVU archive/Non-Peer Reviewed Docs/Irish IQ")
df1 <- read.csv("IrishData.csv")

##Create scatterplot
ggplot(data = df1) + geom_point(mapping = aes(x = yr, y = mean_IQ, color = Country, size = n)) + scale_color_manual(values = c("#169b62", "#ff883e", "#0a3161"))

##Descriptive statistics (unweighted data)
describe(df1, type = 2)

##Unweighted group means by country
describeBy(df1$mean_IQ, df1$Country)

##Correlation between study year and sample mean IQ (unweighted data)
cor.test(~ yr + mean_IQ,na.action="na.exclude", data = df1)

##Correlation between study year and sample mean IQ (weighted data)
weight_vector <- df1$n
df2 <- subset(df1, , select = c("yr", "mean_IQ"))
weighted_corr <-cov.wt(df2, wt = weight_vector, cor = TRUE)
weighted_corr$cor

##Weighted mean (all data)
dfUSA <- subset(df1, Country == "USA")
dfNoIreland <- subset(df1, Country == "Northern Ireland")
dfIreland <- subset(df1, Country == "Ireland")
weighted_mean_all <-weighted.mean(df1$mean_IQ, df1$n)
weighted_mean_USA <-weighted.mean(dfUSA$mean_IQ, dfUSA$n)
weighted_mean_NI <-weighted.mean(dfNoIreland$mean_IQ, dfNoIreland$n)
weighted_mean_Ireland <-weighted.mean(dfIreland$mean_IQ, dfIreland$n)
weighted_mean_all
weighted_mean_USA
weighted_mean_NI
weighted_mean_Ireland


##Drop outlier samples by changing sample size to zero
df3 <-df1
df3[2,3] = 0
df3[4,3] = 0
df3[20,3] = 0
df3[21,3] = 0
df3[22,3] = 0
df3[23,3] = 0

##Recalculate weighted means for remaining samples
dfUSAreweight <- subset(df3, Country == "USA")
dfNoIrelandreweight <- subset(df3, Country == "Northern Ireland")
reweighted_mean_all <-weighted.mean(df3$mean_IQ, df3$n)
reweighted_mean_USA <-weighted.mean(dfUSAreweight$mean_IQ, dfUSAreweight$n)
reweighted_mean_NI <-weighted.mean(dfNoIrelandreweight$mean_IQ, 
                                   dfNoIrelandreweight$n)
reweighted_mean_all
reweighted_mean_USA
reweighted_mean_NI

##Recalculate weighted means for Ireland without Lynn data
df3[28,3] = 0
df3[46,3] = 0
df3[48,3] = 0
df3[49,3] = 0
reweighted_mean_all <-weighted.mean(df3$mean_IQ, df3$n)
dfIrelandreweight <- subset(df3, Country == "Ireland")
reweighted_mean_Ireland <-weighted.mean(dfIrelandreweight$mean_IQ,
                                        dfIrelandreweight$n)
reweighted_mean_all
reweighted_mean_Ireland