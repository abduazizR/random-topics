# Predicting the population health impacts of community interventions: the case of alcohol outlets and binge drinking
# Jennifer Ahern, K. Ellicott Colson, Claire Margerison-Zilko, Alan Hubbard, Sandro Galea
# American Journal of Public Health

# Example code for estimating the health effects of changes in community exposures


# Note on sample dataset:
# The data provided for use with this sample code are simulated. The 
# data are designed to be similar to the real data and associations
# examined in the main paper. There are 4000 observations
# representing individuals who are nested in 44 communities 
# (variable name: neighborhood_id). The exposure of interest is
# neighborhood alcohol outlet density (alc_outlet_density), with
# values ranging from 39 to 168. The outcome of interest is a 
# binary indicator of binge drinking (binge_drink), and covariates 
# include gender (male), age (age_categorical), marital status 
# (married), education (education_categorical), and race/ethnicity 
# (race_categorical). Alcohol outlet density and binge drinking
# were simulated as simple linear functions of the covariates. 
# Thus, unlike the applied example, the relation of outlet density
# with binge drinking has a linear shape. 


#############################################################
# Intervention 1: Deterministic intervention
# Population 1: 
#  Estimate the change in binge drinking associated with 
#  alcohol outlet density changes for the city overall
#############################################################

# Clear workspace 
rm(list=ls())

# Read in sample data
setwd('e:/profiles/desktop/username')
data <- read.csv('sample_data.csv')

summary(data)

# Select an upper limit for alcohol outlet density. 
# Individuals in communities that have outlet densities above this
# level will be set to the limit.
limit <- 100

# Step 1: Fit multivariable regression model and store the model predictions.
regression <- glm(binge_drink ~ alc_outlet_density + factor(male) + 
                    factor(age_categorical) + factor(married) + factor(education_categorical) + factor(race_categorical),
                  family = 'binomial', data = data)
summary(regression)
fitted <- fitted(regression)

# Step 2: Alter the exposure values in the dataset to the exposure 
# pattern that is of interest – store the original values.
alternate_data <- data
alternate_data$alc_outlet_density[alternate_data$alc_outlet_density > limit] <- limit

# Step 3: Predict outcomes under the new exposure pattern from the
# fitted regression model
predictions <- predict(regression, newdata = alternate_data, type = 'response')

# Step 4: Estimate the population level of the outcome – in this case, 
# binge drinking – by averaging predicted outcomes under the exposure
# patterns of interest. 
# In this application, we are comparing the observed level of binge 
# drinking to the level estimated if an upper limit were set for 
# alcohol outlet density. 
# Population of interest: NYC overall.
observed <- mean(fitted)
observed
alternate <- mean(predictions)
alternate

# Step 5: Compare estimates of the outcome under different exposure patterns
# We are comparing binge drinking under the observed alcohol outlet density to 
# the level estimated if an upper limit were set for alcohol outlet density.

alternate - observed

# Step 6: Calculate confidence intervals using the bootstrap
number_of_bootstraps <- 500
estimates <- rep(NA, number_of_bootstraps)
set.seed(321) # Set seed to make results replicable
for (i in 1:number_of_bootstraps) {
  print(i)
  bootstrapped_data <- data[sample(row.names(data), nrow(data), replace=TRUE),]
  reg <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
               factor(married) + factor(education_categorical) + factor(race_categorical),
             family = 'binomial', data = bootstrapped_data)
  fitted <- fitted(reg)
  bootstrapped_data$alc_outlet_density[bootstrapped_data$alc_outlet_density > limit] <- limit
  predictions <- predict(reg, newdata = bootstrapped_data, type = 'response')
  estimates[i] <- mean(predictions) - mean(fitted)
}
quantile(estimates, probs = c(0.025, 0.975))


#############################################################
# Intervention 1: Deterministic intervention
# Population 2:
#  Estimate the difference in binge drinking associated with 
#  alcohol outlet density changes for the subset of communities
#  modified by the intervention
#############################################################

# Clear workspace 
rm(list=ls())

# Read in sample data
setwd('e:/profiles/desktop/username')
data <- read.csv('data/sample_data.csv')

summary(data)

# Select an upper limit for alcohol outlet density. 
# Individuals in communities that have outlet densities above this
# level will be set to the limit.
limit <- 100

# Step 1: Fit multivariable regression model and store the model 
# predictions
regression <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
                    factor(married) + factor(education_categorical) + factor(race_categorical),
                  family = 'binomial', data = data)
summary(regression)
fitted <- fitted(regression)

# Step 2: Alter the exposure values in the dataset to the exposure 
# pattern that is of interest – store the original values.
alternate_data <- data
alternate_data$alc_outlet_density[alternate_data$alc_outlet_density > limit] <- limit

# Step 3: Predict outcomes under the new exposure pattern from the
# fitted regression model
predictions <- predict(regression, newdata = alternate_data, type = 'response')

# Step 4: Estimate the population level of the outcome – in this case, 
# binge drinking – by averaging predicted outcomes under the exposure
# patterns of interest. 
# In this application, we are comparing the observed level of binge 
# drinking to the level estimated if an upper limit were set for 
# alcohol outlet density. 
# Population of interest: the subset of communities modified by the 
# intervention.
observed <- mean(fitted[data$alc_outlet_density > limit])
observed
alternate <- mean(predictions[data$alc_outlet_density > limit])
alternate

# Step 5: Compare estimates of the outcome under different exposure patterns
# We are comparing binge drinking under the observed alcohol outlet density to 
# the level estimated if an upper limit were set for alcohol outlet density.
alternate - observed

# Step 6: Calculate confidence intervals using the bootstrap
number_of_bootstraps <- 500
estimates <- rep(NA, number_of_bootstraps)
set.seed(321) # Set seed to make results replicable
for (i in 1:number_of_bootstraps) {
  print(i)
  bootstrapped_data <- data[sample(row.names(data), nrow(data), replace=TRUE),]
  reg <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
               factor(married) + factor(education_categorical) + factor(race_categorical),
             family = 'binomial', data = bootstrapped_data)
  fitted <- fitted(reg)
  bootstrapped_data$treated <- as.numeric(bootstrapped_data$alc_outlet_density > limit)
  bootstrapped_data$alc_outlet_density[bootstrapped_data$alc_outlet_density > limit] <- limit
  predictions <- predict(reg, newdata = bootstrapped_data, type = 'response')
  estimates[i] <- mean(predictions[bootstrapped_data$treated]) – 
  mean(fitted[bootstrapped_data$treated])
}
quantile(estimates, probs = c(0.025, 0.975))


#############################################################
# Intervention 2: Stochastic intervention
# Population 1: 
#  Estimate the difference in binge drinking associated with 
#  alcohol outlet density changes for the city overall
#############################################################

# Clear workspace 
rm(list=ls())

# Read in sample data
setwd('e:/profiles/desktop/username')
data <- read.csv('sample_data.csv')

summary(data)

# Select an upper limit for alcohol outlet density. 
# Individuals in communities that have outlet densities above this
# level will be set approximately to the limit, but with some 
# stochastic variation.
limit <- 100

# Set the level of variation around the limit. 
# Note: the amount of random variation around the limit that is appropriate depends on 
# the context of the study, the exposure of interest, and the goals of the researcher.
# In this example, random variation is introduced by assigning a new alcohol outlet 
# density for each neighborhood with density above the hypothetical limit to the limit plus 
# a randomly drawn value from a normal distribution with mean 0 and a selected standard 
# devation (represented here by the variable "variation.sd"). One approach for selecting 
# variation.sd is to use a fraction of the total observed variation in the exposure. 
# For example, by setting the variable "level" to 8, we assign variation.sd to be one
# eighth of the standard devation of alc_outlet_density overall. In this manner, the 
# amount of variation introduced around the limit is proportional to the amount of 
# overall variation in the exposure in the data. In this study, we set "level" to
# 4, 8, and 16, to reflect "a lot", "a medium amount", and "a little" variation, 
# respectively. We recommend testing a range of levels. Many other approaches are 
# also possible. 

level <- 8
variation.sd <- sd(data$alc_outlet_density)/level

# Step 1: Fit multivariable regression model and store the model 
# predictions
regression <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
                    factor(married) + factor(education_categorical) + factor(race_categorical),
                  family = 'binomial', data = data)
summary(regression)
fitted <- fitted(regression)

# Step 2: Alter the exposure values in the dataset to the exposure 
# pattern that is of interest – store the original values.
# For this intervention, we will need to loop through each 
# neighborhood id and assign a new, unique exposure level if 
# the outlet density in that neighborhood is above the limit.
alternate_data <- data 
alternate_data$treated <- as.numeric(alternate_data$alc_outlet_density > limit)
ids_of_treated_neighborhoods <- unique(alternate_data$neighborhood_id[alternate_data$treated==1])
set.seed(123) # Set seed to make results replicable
for (i in ids_of_treated_neighborhoods) {
  alternate_data$alc_outlet_density[alternate_data$neighborhood_id == i] <- limit + rnorm(1, 0, variation.sd)
}

# Step 3: Predict outcomes under the new exposure pattern from the
# fitted regression model
predictions <- predict(regression, newdata = alternate_data, type = 'response')

# Step 4: Estimate the population level of the outcome – in this case, 
# binge drinking – by averaging predicted outcomes under the exposure
# patterns of interest. 
# In this application, we are comparing the observed level of binge 
# drinking to the level estimated if an upper limit were set for 
# alcohol outlet density. 
# Population of interest: NYC overall.
observed <- mean(fitted)
observed
alternate <- mean(predictions)
alternate

# Step 5: Compare estimates of the outcome under different exposure patterns
# We are comparing binge drinking under the observed alcohol outlet density to 
# the level estimated if an upper limit were set for alcohol outlet density.
alternate - observed

# Step 6: Calculate confidence intervals using the bootstrap
number_of_bootstraps <- 500
estimates <- rep(NA, number_of_bootstraps)
tic()
set.seed(789) # Set seed to make results replicable
for (i in 1:number_of_bootstraps) {
  print(i)
  bootstrapped_data <- data[sample(row.names(data), nrow(data), replace=TRUE),]
  reg <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
               factor(married) + factor(education_categorical) + factor(race_categorical),
             family = 'binomial', data = bootstrapped_data)
  fitted <- fitted(reg)
  alternate_data <- bootstrapped_data 
  alternate_data$treated <- as.numeric(alternate_data$alc_outlet_density > limit)
  ids_of_treated_neighborhoods <- 
    unique(alternate_data$neighborhood_id[alternate_data$treated==1])
  for (j in ids_of_treated_neighborhoods) {
    alternate_data$alc_outlet_density [alternate_data$neighborhood_id == j] <- limit + 
      rnorm(1, 0, variation.sd)
  }
  predictions <- predict(reg, newdata = alternate_data, type = 'response')
  estimates[i] <- mean(predictions) - mean(fitted)
}
quantile(estimates, probs = c(0.025, 0.975))
toc()

#############################################################
# Intervention 2: Stochastic intervention
# Population 2:
#  Estimate the difference in binge drinking associated with 
#  alcohol outlet density changes for the subset of communities
#  modified by the intervention
#############################################################

# Clear workspace 
rm(list=ls())

# Read in sample data
setwd('e:/profiles/desktop/username')
data <- read.csv('sample_data.csv')

summary(data)

# Select an upper limit for alcohol outlet density. 
# Individuals in communities that have outlet densities above this
# level will be set approximately to the limit, but with some 
# stochastic variation.
limit <- 100

# Set the level of variation around the limit. 
# Note: the amount of random variation around the limit that is appropriate depends on 
# the context of the study, the exposure of interest, and the goals of the researcher.
# In this example, random variation is introduced by assigning a new alcohol outlet 
# density for each neighborhood with density above the hypothetical limit to the limit plus 
# a randomly drawn value from a normal distribution with mean 0 and a selected standard 
# devation (represented here by the variable "variation.sd"). One approach for selecting 
# variation.sd is to use a fraction of the total observed variation in the exposure. 
# For example, by setting the variable "level" to 8, we assign variation.sd to be one
# eighth of the standard devation of alc_outlet_density overall. In this manner, the 
# amount of variation introduced around the limit is proportional to the amount of 
# overall variation in the exposure in the data. In this study, we set "level" to
# 4, 8, and 16, to reflect "a lot", "a medium amount", and "a little" variation, 
# respectively. We recommend testing a range of levels. Many other approaches are 
# also possible. 

level <- 8
variation.sd <- sd(data$alc_outlet_density)/level

# Step 1: Fit multivariable regression model and store the model 
# predictions
regression <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
                    factor(married) + factor(education_categorical) + factor(race_categorical),
                  family = 'binomial', data = data)
summary(regression)
fitted <- fitted(regression)

# Step 2: Alter the exposure values in the dataset to the exposure 
# pattern that is of interest – store the original values.
# For this intervention, we will need to loop through each 
# neighborhood id and assign a new, unique exposure level if the 
# outlet density in that neighborhood is above the limit.
alternate_data <- data 
alternate_data$treated <- 
  as.numeric(alternate_data$alc_outlet_density > limit)
ids_of_treated_neighborhoods <- 
  unique(alternate_data$neighborhood_id[alternate_data$treated == 1])
set.seed(123) # Set seed to make results replicable
for (i in ids_of_treated_neighborhoods) {
  alternate_data$alc_outlet_density [alternate_data$neighborhood_id == i] <- limit + 
    rnorm(1, 0, variation.sd)
}

# Step 3: Predict outcomes under the new exposure pattern from the
# fitted regression model
predictions <- predict(regression, newdata = alternate_data, type = 'response')

# Step 4: Estimate the population level of the outcome – in this case, 
# binge drinking – by averaging predicted outcomes under the exposure
# patterns of interest. 
# In this application, we are comparing the observed level of binge 
# drinking to the level estimated if an upper limit were set for 
# alcohol outlet density. 
# Population of interest: the subset of communities modified by the 
# intervention.
observed <- mean(fitted[data$alc_outlet_density > limit])
observed
alternate <- mean(predictions[alternate_data$treated == 1])
alternate

# Step 5: Compare estimates of the outcome under different exposure patterns
# We are comparing binge drinking under the observed alcohol outlet density to 
# the level estimated if an upper limit were set for alcohol outlet density.
alternate - observed

# Step 6: Calculate confidence intervals using the bootstrap
number_of_bootstraps <- 500
estimates <- rep(NA, number_of_bootstraps)
set.seed(789) # Set seed to make results replicable
for (i in 1:number_of_bootstraps) {
  print(i)
  bootstrapped_data <- data[sample(row.names(data), nrow(data), replace=TRUE),]
  reg <- glm(binge_drink ~ alc_outlet_density + factor(male) + factor(age_categorical) +
               factor(married) + factor(education_categorical) + factor(race_categorical),
             family = 'binomial', data = bootstrapped_data)
  fitted <- fitted(reg)[bootstrapped_data$alc_outlet_density > limit]
  alternate_data <- bootstrapped_data 
  alternate_data$treated <- as.numeric(alternate_data$alc_outlet_density > limit)
  ids_of_treated_neighborhoods <- 
    unique(alternate_data$neighborhood_id[alternate_data$treated == 1])
  for (j in ids_of_treated_neighborhoods) {
    alternate_data$alc_outlet_density [alternate_data$neighborhood_id == j] <- limit + 
      rnorm(1, 0, variation.sd)
  }
  predictions <- predict(reg, newdata = alternate_data, 
                         type = 'response')[alternate_data$treated==1]
  estimates[i] <- mean(predictions) - mean(fitted)
}
quantile(estimates, probs = c(0.025, 0.975))

# END
