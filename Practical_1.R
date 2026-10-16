# SEMINAR ACTIVITY
install.packages("haven")
install.packages("tidyverse")
install.packages("car")
install.packages("stargazer")
install.packages("knitr")
install.packages("mosaic")
install.packages("gmodels")
install.packages("dplyr")
install.packages("forcats")
install.packages("MASS")
install.packages("GGally")

# BEFORE STARTING 
# Load packages
library(tidyverse)
library(car)
library(dplyr)
library(forcats) #load the forcats package to run fct_drop function to drop levels
library(haven)    
library(stargazer) 
library(knitr)     
library(mosaic)
library(gmodels)


# Load datasets
ge24 <- GE2024
aps24 <- APS2024

# Merge datasets
election24aps24 <- merge(
  ge24, 
  aps24, by = "ONS ID")

# Create Labour vote share
election24aps24$lab.per <- election24aps24$Lab / election24aps24$'Valid votes'

# Create Conservative vote share
election24aps24$con.per <- election24aps24$Con / election24aps24$'Valid votes'

# Check the data
names(election24aps24)
summary(election24aps24$lab.per)

# Simple regression (single binary predictor)
library(car)
election24aps24$England <- recode(election24aps24$`Country name`,'"Scotland" = "Other"; "Wales" = "Other"')
table(election24aps24$England)
fit_1 <- lm(lab.per ~ England, data = election24aps24)
fit_2 <- lm(con.per ~ England, data = election24aps24)
summary(fit_1)
summary(fit_2)# two sample t-test; When one predictor the intercept is the mean value of the reference category and the coefficient tells you how much higher (if it is positive) or how much lower (if it is negative) is the mean value for the other category in your factor.

# Regression on region (multi-level)
#levels(election24aps24$'Region name')

# Fitting and interpreting a multiple regression model
fit_4 <- lm(con.per ~ EcoActive+England, data=election24aps24)
summary(fit_4) # 1.check whether all regression coefficients are 0, which is tested by F-test; 
# p-value check to evaluate the null hypothesis
# at least one explanatory variable related to outcome variable 
# interpretation: the coefficient of Ecoactive 
# represents the increase in vote percent for a conservative candidate 
# for every one-unit increase in the working age people percent who are economically active when holding all other variables in the model constant (in this case that refers to holding constant country).

# MODEL BUILDING AND COMPARISON
# conservative vote percentage analysis including ecoactive, age50andover and england into model and verify age50over is significant to the model after the other two are taken into consideration
reduced = lm(con.per ~ EcoActive+England, data=election24aps24) # Reduced model
full = lm(con.per ~ EcoActive+England+Age50andover, data=election24aps24) # Full Model
anova(reduced, full)# Compare the models; The output shows the results of the partial F-test. Since F=215.29 (p-value=0.00000000000000022) we can reject the null hypothesis that the coefficient for aged50over=0

# Automated model selection
library(MASS)
step.model <- lm(con.per ~ EcoActive+England+Age50andover+British+Employment+White, data=election24aps24)
summary(step.model)
step <- stepAIC(step.model, direction="both")
step$anova # display results
final.model <- lm(con.per ~ England+Age50andover+British+Employment, data=election24aps24)
# the final.model explains 41% of the variance in the percent of the vote for a Conservative candidate.

# Multicollinearity
election24aps24_sm <- subset(election24aps24, select=c(con.per, Age50andover,British,Employment))
cor(election24aps24_sm, use="complete.obs")
pairs(election24aps24_sm)
library(GGally)
ggpairs(election24aps24_sm) # no strong correlation between predictors
# other techniques to check up multicollinearity problem between predictors
vif(final.model) # Typically a VIF larger than 5 or 10 indicates serious problems of multicollinearity. 
1/(vif(final.model)) # Field (2012) recommends calculating the tolerance statistic, which is 1/VIF. He suggests values below 0.1 indicate a serious problem, although values below 0.2 are worthy of concern.

# conclusion: The problem with a collinear explanatory variable is that it does NOT add much new to a model-->one predictor too much correlated to another input, too much redundant information added into model
