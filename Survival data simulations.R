library(coxed)
simdata = sim.survdata(N = 1000, T = 100, num.data.frames = 1) 
# simulating 1000 observations of TtoE data ranging from 1-100. Just one dataset simulated.
hist(simdata$data[4]) # y variable
head(simdata$data, 10) # x covariates, y variable, and censored (T/F)

survsim.plot(simdata,df=1,type = "baseline") # to see the baseline hazard function
# df argument indicates which of the simulated data sets you want to look at.
survsim.plot(simdata,df=1,type = "both")

# can define betas 
simdata <- sim.survdata(N=1000, T=100, num.data.frames=1,
                        beta = c(2, .1, .1))
# can define your own hazard function with argument hazard.fun =

# can generate time-varying covariates & coefficients
simdata <- sim.survdata(N=1000, T=100, type="tvc", num.data.frames=1)
head(simdata$data, 20)
simdata <- sim.survdata(N=1000, T=100, type="tvbeta", num.data.frames = 1)
head(simdata$betas, 10) # matrix

# includes simulation of random effects? No

library(simsurv)

