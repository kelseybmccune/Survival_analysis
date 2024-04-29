
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




################### GRACKLE SOCIAL NETWORK CODE HELP ######################
# I am trying to create randomized networks by permuting the IDs of individuals in the edge list. 
# Then from each iteration I need to get the social network metrics, strength and degree, for each individual and save it to a dataframe for output.
# So there will be one dataframe for strength and one for degree with ncols = 10,000 (number of permutations), and nrow = number of unique grackles
# I got most of the for loop to work to permute the IDs and do the data manipulation to get the half-weight index (hwi)
# What I can't figure out is how to merge/combine the output dataframe (strength.perm for the strength metric) for each iteration.


obs.hwi = read.csv("hwiValues.csv")
el = obs.hwi[,c(2,4,8)] # only want ID1, ID2 and hwi
library(igraph)
library(sna)
library(tidyverse)
el = graph_from_data_frame(el, directed = F)
mat = as_adjacency_matrix(el, attr = "hwi", names = T)
el = as.matrix(mat) #symmetrical matrix
obs.strength = data.frame("ID" = names(rowSums(el)),
                          "obs.strength" = rowSums(el)) # strength values for each individual from actual data
els.nb = read.csv("origEdgeList.csv")

#randomly permute the nonFocalBirdID (ID.2)




perm = els.nb
perm$new.id2 = NA

#Subsetting to sites
caPerm = perm %>% 
  filter(Site=='CA')
azPerm = perm %>% 
  filter(Site=='AZ')

# Creating a list of possible combinations for CA
tmp = unique(c(caPerm$ID.1, caPerm$ID.2))
caCombos = expand.grid('ID.1' = tmp, 'ID.2' = tmp) %>% 
  filter(ID.1 != ID.2)
for(j in 1:nrow(caCombos)){
  tmp = sort(as.vector(as.matrix(caCombos[j, c('ID.1', 'ID.2')])))
  caCombos[j, 'ID.1'] = tmp[1]
  caCombos[j, 'ID.2'] = tmp[2]
}
caCombos = unique(caCombos) %>% 
  mutate('Site' = 'CA')


# Creating a list of possible combinations for AZ
tmp = unique(c(azPerm$ID.1, azPerm$ID.2))
azCombos = expand.grid('ID.1' = tmp, 'ID.2' = tmp) %>% 
  filter(ID.1 != ID.2)
for(j in 1:nrow(azCombos)){
  tmp = sort(as.vector(as.matrix(azCombos[j, c('ID.1', 'ID.2')])))
  azCombos[j, 'ID.1'] = tmp[1]
  azCombos[j, 'ID.2'] = tmp[2]
}
azCombos = unique(azCombos) %>% 
  mutate('Site' = 'AZ')


# Randomizing CA
caPerm = caPerm %>% 
  mutate(ID.2 = sample(ID.2, nrow(.), replace=F))


# Randomizing AZ
azPerm = azPerm %>% 
  mutate(ID.2 = sample(ID.2, nrow(.), replace=F))

# Total seen CA
totSeenCa = data.frame('ID' = c(caPerm$ID.1, caPerm$ID.2)) %>% 
  group_by(ID) %>% 
  summarise(totSeen = n())

# Total seen AZ
totSeenAz = data.frame('ID' = c(azPerm$ID.1, azPerm$ID.2)) %>% 
  group_by(ID) %>% 
  summarise(totSeen = n())

# Seen together CA
for (j in 1:nrow(caPerm)) {
  tmp = sort(as.vector(as.matrix(caPerm[j, c('ID.1', 'ID.2')])))
  caPerm[j, 'ID.1'] = tmp[1]
  caPerm[j, 'ID.2'] = tmp[2]
}
caTogether = caPerm %>% 
  group_by(ID.1, ID.2) %>% 
  summarise(seenTogether = n()) %>% 
  filter(ID.1 != ID.2) %>% 
  full_join(caCombos, by=c('ID.1', 'ID.2'))

# Seen together AZ
for (j in 1:nrow(azPerm)) {
  tmp = sort(as.vector(as.matrix(azPerm[j, c('ID.1', 'ID.2')])))
  azPerm[j, 'ID.1'] = tmp[1]
  azPerm[j, 'ID.2'] = tmp[2]
}
azTogether = azPerm %>% 
  group_by(ID.1, ID.2) %>% 
  summarise(seenTogether = n()) %>% 
  filter(ID.1 != ID.2) %>% 
  full_join(azCombos, by=c('ID.1', 'ID.2'))


# THIS IS WHERE I STOPPED - FOR SOME REASON CA COMPLETE AND AZ COMPLETE
# HAVE 1 MORE ROW THAN AZ TOGETHER AND CA TOGETHER.

caComplete = totSeenCa %>% 
  set_names(c('ID.1', 'totSeen1')) %>% 
  full_join(caTogether, by='ID.1')

azComplete = totSeenAz %>% 
  set_names(c('ID.1', 'totSeen1')) %>% 
  full_join(azTogether, by='ID.1')


# 
# test = totCombosCa %>%
#   full_join(caTogether, by=c('ID.1', 'ID.2'))
# 
# 
# 
# 
# 
# perm$count = 1
# seenTogether = aggregate(count ~ ID.1 + new.id2 + Site, data = perm, FUN = "sum")
# colnames(seenTogether)[4]="seenTogether"
# seenTogether = seenTogether %>%
#   filter(ID.1 != new.id2)
# 
# 
# #Merge this back in with all possible combinations of birds that could be seen together
# seenTogether$inST = 1
# totCombos$inTC = 1
# 
# 
# test = merge(totCombos, seenTogether, by=c('ID.1', 'new.id2', 'Site'), all=T)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# perm$new.id2 = sample(perm$ID.2, nrow(perm), replace=F) # permuted edgelist, includes UBs and UNKs
# 
# 
# #THERE IS STILL A PROBLEM HERE WHERE AN INDIVIDUAL CAN BE MATCHED UP WITH ITSELF
# 
# # Now go through exact same process as with original data to calculate half-weight index
# 
# # count number of times each focal bird is seen individually (y)
# id1 = perm[,c(3,5)] #ID.1, Site
# colnames(id1)[1] = "ID"
# id2 = perm[,c(4,5)] #Site, new.id2
# colnames(id2)[1] = "ID"
# totSeen = rbind(id1, id2)
# totSeen$count = 1
# totSeen = aggregate(count ~ ID + Site, data = totSeen, FUN = "sum")
# colnames(totSeen)[3] = "totSeen"
# 
# # #Creating a list of all possible combinations that could be seen together in CA
# tmpCa = totSeen %>% filter(Site == 'CA')
# caCombos = expand.grid('ID.1' = tmpCa$ID, 'new.id2' = tmpCa$ID) %>%
#   filter(ID.1 != new.id2)
# for(j in 1:nrow(caCombos)){
#   tmp = sort(as.vector(as.matrix(caCombos[j, c('ID.1', 'new.id2')])))
#   caCombos[j, 'ID.1'] = tmp[1]
#   caCombos[j, 'new.id2'] = tmp[2]
# }
# caCombos = unique(caCombos) %>%
#   mutate('Site' = 'CA')
# 
# # #Creating a list of all possible combinations that could be seen together in AZ
# tmpAz = totSeen %>% filter(Site == 'AZ')
# azCombos = expand.grid('ID.1' = tmpAz$ID, 'new.id2' = tmpAz$ID) %>%
#   filter(ID.1 != new.id2)
# for(j in 1:nrow(azCombos)){
#   tmp = sort(as.vector(as.matrix(azCombos[j, c('ID.1', 'new.id2')])))
#   azCombos[j, 'ID.1'] = tmp[1]
#   azCombos[j, 'new.id2'] = tmp[2]
# }
# azCombos = unique(azCombos) %>%
#   mutate('Site' = 'AZ')
# 
# totCombos = rbind(caCombos, azCombos)
# 
# rm(caCombos, azCombos, tmpAz, tmpCa)
# 
# 
# 
# #Now reduce to birds with at least 2 focal follows
# # perm = perm[which(perm$ID.1 == "Adobo" | perm$ID.1 == "Burrito" | perm$ID.1 == "Chilaquile" |
# #                     perm$ID.1 == "Chalupa" | perm$ID.1 == "Diablo" | perm$ID.1 == "Fideo" |
# #                     perm$ID.1 == "Taco" | perm$ID.1 == "Taquito" |  perm$ID.1 == "Yuca" |
# #                     perm$ID.1 == "Tembleque" | perm$ID.1 == "Polvorones" | perm$ID.1 == "Camote" |
# #                     perm$ID.1 == "Dulce de Leche" | perm$ID.1 == "Zapote Negro" | perm$ID.1 == "Cuervo" |
# #                     perm$ID.1 == "Xunub" | perm$ID.1 == "Galandra" | perm$ID.1 == "Kel" |
# #                     perm$ID.1 == "Kau" | perm$ID.1 == "Cutuy" | perm$ID.1 == "Tzanatl Preciosa" |
# #                     perm$ID.1 == "Pina"),]
# 
# # And exclude unbanded birds
# # perm <- perm[!perm$new.id2=="NA" & !perm$new.id2=="unbanded adult female" &
# #                     !perm$new.id2=="unbanded adult male" & !perm$new.id2=="unbanded juvenile" &
# #                     !perm$new.id2=="unbanded juvenile female" & !perm$new.id2=="unbanded juvenile male" &
# #                     !perm$new.id2=="unbanded unknown female" & !perm$new.id2=="unbanded unknown male"  &
# #                     !perm$new.id2=="unknown adult female" & !perm$new.id2=="unknown adult male" &
# #                     !perm$new.id2=="unknown banded female" & !perm$new.id2=="unknown female" &
# #                     !perm$new.id2=="unknown grackle"  & !perm$new.id2=="unknown individual" &
# #                     !perm$new.id2=="unknown juvenile" & !perm$new.id2=="unknown juvenile female"  &
# #                     !perm$new.id2=="unknown juvenile male" & !perm$new.id2=="unknown male" &
# #                     !perm$new.id2=="unknown unbanded male",]
# 
# 
# #Create an edgelist, symmetrize the associations so all are in the same order and repeat pairs can be identified
# # perm$ID.1 = as.character(perm$ID.1)
# # perm$new.id2 = as.character(perm$new.id2)
# 
# 
# # I edited this, and now feel good about it
# for (j in 1:nrow(perm)) {
#   tmp = sort(as.vector(as.matrix(perm[j, c('ID.1', 'new.id2')])))
#   perm[j, 'ID.1'] = tmp[1]
#   perm[j, 'new.id2'] = tmp[2]
# }
# 
# 
# 
# #For the half-weight index, calculate the number of times two birds are seen together (x)
# perm$count = 1
# seenTogether = aggregate(count ~ ID.1 + new.id2 + Site, data = perm, FUN = "sum")
# colnames(seenTogether)[4]="seenTogether"
# seenTogether = seenTogether %>%
#   filter(ID.1 != new.id2)
# 
# 
# #Merge this back in with all possible combinations of birds that could be seen together
# seenTogether$inST = 1
# totCombos$inTC = 1
# 
# 
# test = merge(totCombos, seenTogether, by=c('ID.1', 'new.id2', 'Site'), all=T)
# 
# 
# seenTogether$seenTogether = ifelse(is.na(seenTogether$seenTogether), 0, seenTogether$seenTogether)
# 
# 
# colnames(totSeen)[1] = "ID.1"
# colnames(totSeen)[3] = 'totSeen1'
# 
# hwi.perm = merge(seenTogether,totSeen, by = 'ID.1', all=T)
# 
# 
# hwi.perm$saw1not2 = hwi.perm$totSeen1 - hwi.perm$seenTogether
# hwi.perm = hwi.perm[,-5]
# 
# 
# colnames(totSeen)[1] = "new.id2"
# colnames(totSeen)[3] = 'totSeen2'
# hwi.perm = merge(hwi.perm, totSeen, by=c('new.id2', 'Site'), all=T)
# 
# hwi.perm = merge(hwi.perm,y, by = c("new.id2","Site"))
# hwi.perm$Y.2 = hwi.perm$y - hwi.perm$x #the number of times bird 2 was seen without bird 1
# hwi.perm = hwi.perm[,-6]
# #data frame ready for half-weight index equation
# ### Half-weight index is used to weight the edges in the network for the calculation of the social network metric "strength". The equation is:
# hwi.perm$hwi = hwi.perm$x/(0.5*(hwi.perm$Y.1 + hwi.perm$Y.2)+hwi.perm$x)
# 
# ### Strength
# el = hwi.perm[,c(1,3,7)] # only want ID1, ID2 and hwi
# el = graph_from_data_frame(el, directed = F)
# mat = as_adjacency_matrix(el, attr = "hwi", names = T)
# el = as.matrix(mat) #symmetrical matrix
# tmp = data.frame(rowSums(el)) #strength
# tmp$ID = row.names(tmp)
# 
# 
# strength.perm = merge(strength.perm,tmp2, by = "ID", all = T) #### CANNOT FIGURE OUT THIS LINE #####
# #merge(strength.perm,tmp2[,2], by = 'ID', all = T)
# #cbind(strength.perm, tmp[match(rownames(strength.perm), rownames(tmp))])
# #
# #tmp2[,2]
# 
# 
# 
# 
# 
# 



strength.perm = obs.strength
for(i in 1:3){
  perm = els.nb
  perm$new.id2 = NA
  perm$new.id2 = sample(perm$ID.2, nrow(perm), replace=F) # permuted edgelist, includes UBs and UNKs
  
  # Now go through exact same process as with original data to calculate half-weight index  
  
  # count number of times each focal bird is seen individually (y)
  tmp2 = perm[,c(2,4)] #ID.1, Site
  colnames(tmp2)[1] = "ID"
  tmp3 = perm[,c(4,6)] #Site, new.id2
  colnames(tmp3)[2] = "ID"
  y = rbind(tmp2, tmp3)
  y$count = 1
  y = aggregate(count ~ ID + Site, data = y, FUN = "sum") 
  colnames(y)[3] = "y"
  
  #Now reduce to birds with at least 2 focal follows 
  perm = perm[which(perm$ID.1 == "Adobo" | perm$ID.1 == "Burrito" | perm$ID.1 == "Chilaquile" | 
                      perm$ID.1 == "Chalupa" | perm$ID.1 == "Diablo" | perm$ID.1 == "Fideo" | 
                      perm$ID.1 == "Taco" | perm$ID.1 == "Taquito" |  perm$ID.1 == "Yuca" | 
                      perm$ID.1 == "Tembleque" | perm$ID.1 == "Polvorones" | perm$ID.1 == "Camote" | 
                      perm$ID.1 == "Dulce de Leche" | perm$ID.1 == "Zapote Negro" | perm$ID.1 == "Cuervo" | 
                      perm$ID.1 == "Xunub" | perm$ID.1 == "Galandra" | perm$ID.1 == "Kel" | 
                      perm$ID.1 == "Kau" | perm$ID.1 == "Cutuy" | perm$ID.1 == "Tzanatl Preciosa" | 
                      perm$ID.1 == "Pina"),]
  
  # And exclude unbanded birds
  # perm <- perm[!perm$new.id2=="NA" & !perm$new.id2=="unbanded adult female" & 
  #                     !perm$new.id2=="unbanded adult male" & !perm$new.id2=="unbanded juvenile" & 
  #                     !perm$new.id2=="unbanded juvenile female" & !perm$new.id2=="unbanded juvenile male" &
  #                     !perm$new.id2=="unbanded unknown female" & !perm$new.id2=="unbanded unknown male"  & 
  #                     !perm$new.id2=="unknown adult female" & !perm$new.id2=="unknown adult male" & 
  #                     !perm$new.id2=="unknown banded female" & !perm$new.id2=="unknown female" & 
  #                     !perm$new.id2=="unknown grackle"  & !perm$new.id2=="unknown individual" & 
  #                     !perm$new.id2=="unknown juvenile" & !perm$new.id2=="unknown juvenile female"  & 
  #                     !perm$new.id2=="unknown juvenile male" & !perm$new.id2=="unknown male" & 
  #                     !perm$new.id2=="unknown unbanded male",] 
  
  
  #Create an edgelist, symmetrize the associations so all are in the same order and repeat pairs can be identified
  perm$ID.1 = as.character(perm$ID.1)
  perm$new.id2 = as.character(perm$new.id2)
  for (j in 1:nrow(perm)) {
    el.perm = perm[j, c("ID.1", "new.id2")]
    el.perm = el.perm[,sort.list(el.perm)] #produces warning messages, but works
    perm[j, "ID.1"] = el.perm[,1]
    perm[j, "new.id2"] = el.perm[,2]
  }
  #For the half-weight index, calculate the number of times two birds are seen together (x)
  perm$count = 1
  x = aggregate(count ~ ID.1 + new.id2 + Site, data = perm, FUN = "sum")
  colnames(x)[4]="x"
  
  colnames(y)[1] = "ID.1" 
  hwi.perm = merge(x,y, by = c("ID.1","Site"))
  hwi.perm$Y.1 = hwi.perm$y - hwi.perm$x #the number of times bird 1 was seen without bird 2
  hwi.perm = hwi.perm[,-5]
  colnames(y)[1] = "new.id2"
  hwi.perm = merge(hwi.perm,y, by = c("new.id2","Site"))
  hwi.perm$Y.2 = hwi.perm$y - hwi.perm$x #the number of times bird 2 was seen without bird 1
  hwi.perm = hwi.perm[,-6]
  #data frame ready for half-weight index equation
  ### Half-weight index is used to weight the edges in the network for the calculation of the social network metric "strength". The equation is:
  hwi.perm$hwi = hwi.perm$x/(0.5*(hwi.perm$Y.1 + hwi.perm$Y.2)+hwi.perm$x)
  
  ### Strength
  el = hwi.perm[,c(1,3,7)] # only want ID1, ID2 and hwi
  el = graph_from_data_frame(el, directed = F)
  mat = as_adjacency_matrix(el, attr = "hwi", names = T)
  el = as.matrix(mat) #symmetrical matrix
  tmp = data.frame(rowSums(el)) #strength
  tmp2 = data.frame("ID" = names(tmp),
                    i=tmp)
  strength.perm = merge(strength.perm,tmp2, by = "ID", all = T) #### CANNOT FIGURE OUT THIS LINE #####
  #merge(strength.perm,tmp2[,2], by = 'ID', all = T)
  #cbind(strength.perm, tmp[match(rownames(strength.perm), rownames(tmp))])
  #
  #tmp2[,2]
}