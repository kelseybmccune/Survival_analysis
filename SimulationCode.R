
library(tidyverse)
library(lme4)
library(rptR)


rm(list=ls())


###############################################################################
#Starting with a single simulation
###############################################################################

# Creating simulation parameters for a situation where you have multiple trials
# on multiple individuals.

nSubjects = 20
nTests = 8
beta0 = log(2000)
betweenSubjectSd = 0.02
betweenSubjectVar = betweenSubjectSd^2

# Generating random effects for each individual

data = data.frame('subject' = as.factor(1:nSubjects),
                  'subjectEffect' = rnorm(nSubjects, 0, betweenSubjectSd))

# Creating a row for each test on each subject

data = merge(data, 1:nTests) %>% 
  rename(test = 'y')

# Adding in the intercept

data$beta0 = beta0

# Now the ln of the expected value (lambda) should be beta0 + the random subject
# effect for each observation, given that we are not simulating any other
# covariate effects right now.

data$logLambda = data$beta0 + data$subjectEffect

# And lambda should be exp(logLambda)

data$lambda = exp(data$logLambda)

# Now we have an expected value for the response variable for each individual on
# each test. So we can use these values to generate a random draw from a Poisson
# distribution that is unique to each individual.

data$obsResponse = rpois(n = nrow(data), lambda=data$lambda)


# No we can fit a random effects Poisson model to these data.

fittedModel = glmer(obsResponse ~ 1 + (1|subject), family=poisson, data=data)

sumFittedModel = summary(fittedModel)

fittedModel@theta

# Let's look at the model-estimated intercept which we want to be close to the
# beta0 value we chose

beta0
fittedModel@beta

# And let's look at the model-estimated between-subject standard deviation which
# we want to be close to the betweenSubjectSd we chose

betweenSubjectSd
fittedModel@theta

# Checking repeatability

residVar = log(1/exp(beta0)+1)

rptTruth = betweenSubjectVar / (betweenSubjectVar + residVar)
rptTruth


###############################################################################
#Looping the code to do lots of simulations and make sure that the model
#generally recovers the correct values
###############################################################################


# That's all well and good for 1 simulation, but how do we know that this model
# tends to get the right answers? Well, we can loop this code over a bunch of
# iterations and record the model-estimated beta0 and between-subject sd on
# each iteration.

nSims = 1000

nSubjects = 20
nTests = 8
beta0 = log(2000)
betweenSubjectSd = 2

beta0Ests = rep(NA, nSims)
sdEsts = rep(NA, nSims)

for(i in 1:nSims){
  
  #Generate random effects for each individual
  data = data.frame('subject' = as.factor(1:nSubjects),
                    'subjectEffect' = rnorm(nSubjects, 0, betweenSubjectSd))
  
  # Creating a row for each test on each subject
  data = merge(data, 1:nTests) %>% 
    rename(test = 'y')
  
  # Adding in the intercept
  
  data$beta0 = beta0
  
  # Calculating lambda
  
  data$logLambda = data$beta0 + data$subjectEffect
  
  data$lambda = exp(data$logLambda)
  
  
  # Drawing random values
  
  data$obsResponse = rpois(n = nrow(data), lambda=data$lambda)
  
  
  # Fitting the model
  
  fittedModel = glmer(obsResponse ~ 1 + (1|subject), family=poisson, data=data)
  
  # Recording beta0 and between-subject SD
  
  beta0Ests[i] = fittedModel@beta
  sdEsts[i] = fittedModel@theta
  
}

# Now we can look at the distribution of the estimates from 1000 simulations
# and how they relate to the simulation parameters.

simResults = data.frame('beta0' = beta0Ests,
                        'sd' = sdEsts)

#First beta0

ggplot(simResults, aes(x = beta0))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=beta0, col='red')

#Now the between-subject SD values

ggplot(simResults, aes(x=sd))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=betweenSubjectSd, col='red')




###############################################################################
#What if we do everything the exact same, but truncate every observed value
#greater than 2000 because we are pretending that our experiments ended at
#2000 time units, so we just set every other value to 2001?
###############################################################################


nSims = 1000

nSubjects = 20
nTests = 8
beta0 = log(2000)
betweenSubjectSd = 2

beta0Ests = rep(NA, nSims)
sdEsts = rep(NA, nSims)

for(i in 1:nSims){
  
  #Generate random effects for each individual
  data = data.frame('subject' = as.factor(1:nSubjects),
                    'subjectEffect' = rnorm(nSubjects, 0, betweenSubjectSd))
  
  # Creating a row for each test on each subject
  data = merge(data, 1:nTests) %>% 
    rename(test = 'y')
  
  # Adding in the intercept
  
  data$beta0 = beta0
  
  # Calculating lambda
  
  data$logLambda = data$beta0 + data$subjectEffect
  
  data$lambda = exp(data$logLambda)
  
  
  # Drawing random values
  
  data$obsResponse = rpois(n = nrow(data), lambda=data$lambda)
  
  # THE ONLY CHANGE TO THE CODE IS THIS NEXT LINE
  data$obsResponse = ifelse(data$obsResponse > 2000, 2001, data$obsResponse)
  
  
  # Fitting the model
  
  fittedModel = glmer(obsResponse ~ 1 + (1|subject), family=poisson, data=data)
  
  # Recording beta0 and between-subject SD
  
  beta0Ests[i] = fittedModel@beta
  sdEsts[i] = fittedModel@theta
  
}

# Now we can look at the distribution of the estimates from 1000 simulations
# and how they relate to the simulation parameters.

simResults = data.frame('beta0' = beta0Ests,
                        'sd' = sdEsts)

#First beta0

ggplot(simResults, aes(x = beta0))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=beta0, col='red')

#Now the between-subject SD values

ggplot(simResults, aes(x=sd))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=betweenSubjectSd, col='red')



