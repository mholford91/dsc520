# Week 3 Assignment 2
# Megan Holford
# 09-19-2020

#importing libraries
library(ggplot2)
library(tidyverse)
library(pastecs)
library(moments)
library(psych)

#pulls the data into the data frame.
read.csv('acs-14-1yr-s0201.csv') -> survey_data
 
# Please provide the output from the following functions: str(); nrow(); ncol()
str(survey_data)

nrow(survey_data)

ncol(survey_data)

# Calculating the mean, median. and standard deviation,
datamean <- mean(survey_data$HSDegree)
datamedian <- median(survey_data$HSDegree)
datasd <- sd(survey_data$HSDegree)

# Create a Histogram of the HSDegree variable using the ggplot2 package.
# Set a bin size for the Histogram.
# Include a Title and appropriate X/Y axis labels on your Histogram Plot.
ggplot(survey_data, aes(HSDegree)) + geom_histogram(bins = 40, color = "black", fill = "white") + ggtitle("High School Degree Data") + xlab("Percentage of High School Degrees") + ylab("Density")

# Include a normal curve to the Histogram that you plotted.
ggplot(data = survey_data, aes(x = HSDegree)) + geom_histogram(bins = 40, color = "black", fill = "white") + ggtitle("High School Degree Data") + xlab("Percentage of High School Degrees") + ylab("Density") + stat_function(fun = function(x) dnorm(x, mean = datamean, sd = datasd) * 175)

# Create a Probability Plot of the HSDegree variable.
ggplot(survey_data, aes(HSDegree)) + geom_density()+ ggtitle("High School Degree Data") + xlab("Percentage of High School Degrees") + ylab("Density")

# Now quantify normality with numbers using the stat.desc() function. 
stat.desc(survey_data)

# Provide an explanation of the result produced for skew, kurtosis, and z-scores.
data_description <- describe(survey_data$HSDegree)

# Skew
data_description$skew

# Kurtosis
data_description$kurtosis

# Z-scores
zscore <- (survey_data$HSDegree - mean(survey_data$HSDegree)) / sd(survey_data$HSDegree)

# This binds the z-score data to the data frame. 
survey_data$zscore <- cbind(survey_data, zscore)




