
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

# would want to include range of values here, and some values from actual data when going to publication
# range of values that are common from literature search


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


# Now we can fit a random effects Poisson model to these data.

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

rpt = rpt(obsResponse ~ 1 + (1|subject), grname = "subject", data = data, datatype = "Poisson",
          nboot = 100, npermut = 50)
summary(rpt) #R = 0.48

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
betweenSubjectSd = 0.02

beta0Ests = rep(NA, nSims)
sdEsts = rep(NA, nSims)
rptEsts = rep(NA, nSims)

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
  
  # calculating and recording rpt
  rpt = rpt(obsResponse ~ 1 + (1|subject), grname = "subject", data = data, datatype = "Poisson",
            nboot = 2, npermut = 2)
  rptEsts[i] = rpt$R$subject[2]
  
}

# Now we can look at the distribution of the estimates from 1000 simulations
# and how they relate to the simulation parameters.

simResults = data.frame('beta0' = beta0Ests,
                        'sd' = sdEsts,
                        'rpt' = rptEsts)

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

#And the rpt values
mean(simResults$rpt)
ggplot(simResults, aes(x=rpt))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=0.44, col='red')


###############################################################################
#What if we do everything the exact same, but truncate every observed value
#greater than 2000 because we are pretending that our experiments ended at
#2000 time units, so we just set every other value to 2001?
###############################################################################


nSims = 1000

nSubjects = 20
nTests = 8
beta0 = log(2000)
betweenSubjectSd = 0.02

beta0Ests = rep(NA, nSims)
sdEsts = rep(NA, nSims)
rptEsts = rep(NA,nSims)

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
  
  # calculating and recording rpt
  rpt = rpt(obsResponse ~ 1 + (1|subject), grname = "subject", data = data, datatype = "Poisson",
            nboot = 2, npermut = 2)
  rptEsts[i] = rpt$R$subject[2]
  
}

# Now we can look at the distribution of the estimates from 1000 simulations
# and how they relate to the simulation parameters.

simResults = data.frame('beta0' = beta0Ests,
                        'sd' = sdEsts,
                        'rpt' = rptEsts)

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


ggplot(simResults, aes(x=rpt))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_histogram(fill='white', color='black')+
  geom_vline(xintercept=0.44, col='red')

###############################################################################
#Real time-to-event data, grackle exploration of novel environment measured 
#as latency to come to the ground with the novel environment
#19 individuals, each individual had 2 trials. Trials could be max 50 minutes
#Birds that didn't come too ground (NAs) given ceiling value of 51 minutes
###############################################################################


#Exploration of novel environment
ee <- read.csv("https://raw.githubusercontent.com/corinalogan/grackles/master/Files/Preregistrations/g_exploration_data_exploration.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(ee)  #Check to make sure it looks right


#want only Test=Environment (not Object)
ee <- ee[ee$Test=="Environment",]

#Sum duration across distances to get DurationOnGround variable as in boldness analysis above
ee$DurationOnGround = ee$TotalDurationInTent + ee$TotalDurationFar.20cm + ee$TotalDurationNear.20cm
#changed LatencyFirstLand NAs to a number higher than the assay session length so they show up as 
#data points? yes. Set latency for these at 3060 (51 minutes: the highest assay duration was 50 mins)
ee$LatencyFirstLand[which(is.na(ee$LatencyFirstLand))]<-3060
ee$LatencyFirstLand <- as.numeric(ee$LatencyFirstLand)
ee$DurationOnGround <- as.numeric(ee$DurationOnGround)
#LatencyFirstLand or Duration has the most data so will use one of these variables. Histograms for both look poisson
ee$BirdID <- as.factor(ee$BirdID)

### Covariate for behavioral flexibility manipulated
# add column identifying whether bird was in the flexibility manipulation or control groups
d <- read.csv(url("https://raw.githubusercontent.com/corinalogan/grackles/master/Files/Preregistrations/g_flexmanip_data_reverse.csv"), header=T, sep=",", stringsAsFactors=F)
d$ID <- as.factor(d$ID)
#remove pilot birds
d <- d[!d$ID=="Fajita" & !d$ID=="Empanada",]

#extract just bird name and flex group
flexID = unique(d[,c(4,5)])
colnames(flexID) = c("Name","FlexGroup")
#Taco did initial discrimination and 1 reversal, so he is considered Control group for this analysis
flexID$FlexGroup[which(flexID$Name == "Taco")]<- "Control"

# Add band ID bird names to data frame to merge it with boldness data
biometrics = read.csv(url("https://raw.githubusercontent.com/corinalogan/grackles/master/Files/Data/data_biometricsAZ.csv"), header=T, sep=",", stringsAsFactors=F)
biometrics = unique(biometrics[,c(1,2)])
flexID = merge(flexID, biometrics, by = "Name")
colnames(ee)[1] <- "Name"

exp = merge(ee, flexID, by = "Name", all = T)
# remove NA birds that were in the aviaries but did not complete exploration assays (4 birds)
exp = exp[-which(is.na(exp$Condition)),]

em1 = glmer(round(LatencyFirstLand) ~ Condition + FlexGroup + (1|BirdID), data = exp, family = "poisson")
summary(em1)



#Now we are going to evaluate the fit of this fitted model. To do that, we're going to use
#the fitted model to simulate a bunch of datasets. This gives us a sense of what kinds
#of data this fitted model generates. We're then going to evaluate whether the kinds of
#data this model generates look like the data we actually collected.

#Simulate 500 datasets from the fitted model
nSims = 500
tmp1 = simulate(em1, nsim=nSims)

#We are going to calculate a chi-squared value for each simulated dataset, so we
#are creating a vector in which to save those chi-squared values.
simResults = rep(NA, nSims)


for(i in 1:nSims){
  
  #The "observed" values in each simulated dataset are based on the explanatory variables
  #in the original dataset. So the easiest way to create a full new simulated dataset
  #is just to pull the original data, and then replace the true observed data with the
  #simulated observed data
  tmp2 = exp %>%
    mutate(LatencyFirstLand = tmp1[,i])
  
  #So now that we have a simulated dataset, we have our observed values. To get our
  #expected values, we fit the exact same model again (this time using the simulated
  #response variables).
  tmp3 = glmer(round(LatencyFirstLand) ~ Condition + FlexGroup + (1|BirdID),
               data = tmp2, family = "poisson")
  
  #So we have our "observed" values which are simulated
  o = tmp2$LatencyFirstLand
  
  #And our "expected" values which are generated from the model fit to the
  #simulated observed values
  e = predict(tmp3, type='response')
  
  #And we can caluclate a chi-squared value for this dataset.
  simResults[i] = sum((o-e)^2/e, na.rm=T)
}

#When we do this 500 times, we get a distribution of the kinds of chi-squared values that
#this fitted model would generate IF it is in fact the correct representation of the data



#Now let's calculate a chi-squred value for the original dataset from the original
#fitted model
modChisq = sum((exp$LatencyFirstLand - predict(em1, type='response'))^2/predict(em1, type='response'))


#Lastly, we can plot the distribution of chi-squared values from the simulations and
#compare this distribution to the one we actually observed. If our original fitted model
#is a good representation of our original fitted data, then the original chi-squared value
#should be similar to those generated from our simulations
tmp = data.frame(simResults)

ggplot(tmp, aes(x=simResults))+
  geom_histogram(fill='white', color='black', binwidth=5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=modChisq, color='red')

#And we can calculate a 'p-value' of sorts by asking what proportion of the simulated
#chi-squared values are larger or smaller than chi-squared value observed from the
#original data and model. Here, we want our p-value to be around 0.5 indicating that
#about half of the chi-square values from the simulations are larger than the observed
#value and the other half are smaller. If the observed value falls in the upper or lower
#2.5% of simulated values, we would say there is significant evidence (at the alpha level
#of 0.05) that the model doesn't fit our original dataset very well.


sum(simResults > modChisq)/length(simResults)





