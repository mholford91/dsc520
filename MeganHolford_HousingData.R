# Megan Holford
# Week 7 Housing Data
# October 17, 2020

# loading libraries for use
library(readxl)
library(ggplot2)
library(lm.beta)
library(car)



# Dataframe loading and output the first few lines and a summary of the data. 
housing_df<- read_excel('data/week-7-housing.xlsx')
head(housing_df)
summary(housing_df)


# a.Explain why you chose to remove data points from your ‘clean’ dataset.
# When working with data it is important to remove any variables that are not pertinent to the outcome. 
# In the case of this data set, variables like sale_warning, ctyname, and addr_full are not useful or redunant 
# and so could be removed and cleaned from the dataframe. 


# b. Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables 
# used from previous assignment on simple regression) and one that will contain Sale Price and several additional
# predictors of your choice. Explain the basis for your additional predictor selections.

# This is the base model using Sale Price and Square Foot of Lot. 
base_lm <- lm(housing_df$`Sale Price` ~ housing_df$sq_ft_lot, housing_df)

# This model includes other variables with the Sale Price like total square feet, bedrooms, bathrooms, and year built
multi_lm <- lm(housing_df$`Sale Price` ~ housing_df$square_feet_total_living + housing_df$bedrooms 
               + housing_df$bath_full_count + housing_df$bath_half_count + housing_df$year_built)

# c. Execute a summary() function on two variables defined in the previous step to compare the model results. 
# What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. Did 
# the inclusion of the additional predictors help explain any large variations found in Sale Price?
summary(base_lm)
summary(multi_lm)

# Rsquared is the correlation between the dependent variable and the independent variable
# base_lm has an adjusted rsquared of 0.01428 while multi_lm has an adjusted rsquared of 0.2192 The addition 
# of the variables improved the rsquared values. This means that to correlation is better for multi_lm than base_lm.
 

# d. Considering the parameters of the multiple regression model you have created. What are the standardized 
# betas for each parameter and what do the values indicate?
lm.beta(base_lm)
lm.beta(multi_lm)


# e. Calculate the confidence intervals for the parameters in your model and explain what the results indicate.
confint(base_lm)
confint(multi_lm)
# The confidence intervals show the ranges that we can expect when using the linear model. This shows the expected
# outcome about 95% of the time.

# f. Assess the improvement of the new model compared to your original model (simple regression model) by testing
# whether this change is significant by performing an analysis of variance.
anova(base_lm, multi_lm)
#The residuals decreased from the original variable to the new created one.
#This means that means there is less of a relationship due to this addition.

# g. Perform casewise diagnostics to identify outliers and/or influential cases, storing each function's output 
# in a dataframe assigned to a unique variable name.

function(base_lm)
function(multi_lm)
base_resid_lm<- resid(base_lm)
multi_resid_lm<-resid(multi_lm)
base_rstandard_lm<-rstandard(base_lm)
multi_rstandard_lm<-rstandard(multi_lm)


# h.Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing 
# the results of large residuals in a variable you create.

housing_large_resid<- multi_rstandard_lm > 2 | multi_rstandard_lm< -2 

# i. Use the appropriate function to show the sum of large residuals.
sum(housing_large_resid)

# j.Which specific variables have large residuals (only cases that evaluate as TRUE)?

housing_df[housing_large_resid, c("Sale Price", "square_feet_total_living", "bedrooms", "bath_full_count", 
                                  "bath_half_count", "year_built")]

# k. nvestigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all 
# cases that are problematics.
hatvalues(base_lm)
hatvalues(multi_lm)
cooks.distance(base_lm)
cooks.distance(multi_lm)
cov(housing_df$`Sale Price`, housing_df$sq_ft_lot)
cov(housing_df$`Sale Price`,housing_df$bedrooms)

# l. Perform the necessary calculations to assess the assumption of independence and state if the condition is
# met or not. 
durbinWatsonTest(multi_lm)


# m. Perform the necessary calculations to assess the assumption of no multicollinearity and state if the 
# condition is met or not.
vif(multi_lm)

# n. Visually check the assumptions related to the residuals using the plot() and hist() functions. Summarize 
# what each graph is informing you of and if any anomalies are present.

plot(multi_lm)


hist(base_rstandard_lm)
hist(multi_resid_lm)


# o. Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about 
# the sample vs. the entire population model?

# Based on the tests and visuals, we can see that there is a bias in this data. While the sample has valuable
# information, there are some outliers that should be addresses to put it toward the entire population. Another
# thing that can be done is to have a test set separate from the building set to see if the data is a good 
# overall representation. 

