rm(list=ls())

library(tidyverse)


x = runif(100, -3, 3)
beta0 = 1
beta1 = 2
e = rnorm(100, 0, 3)

y = beta0 + beta1*x + e


fitModel = lm(y ~ x)
summary(fitModel)


#Simple simulation of survival times

timeIntervals = rpois(100, 200)

hist(timeIntervals)

eventTimes = timeIntervals

for(i in 2:100){
  eventTimes[i] = eventTimes[i] + eventTimes[i-1]
}
eventTimes[1] = timeIntervals[1]

tmp = data.frame('events' = 1, 'times' = eventTimes)

ggplot(tmp, aes(x=times, y=events))+
  geom_point()





# Testing

n = 100
