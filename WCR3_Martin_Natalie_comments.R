### my comments preceded by `CPK:`
#Load all libraries
library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)


#CPK: only needed 2 of these packages


#1 Load the data 
fishdat <- read_csv("fish_data.csv")

#2 Regression scatter plot
#CPK: Yes! Log transform. I didn't ask for it, but this is spot on!
fishdat.log <- fishdat %>% 
  mutate_at(c("eye_size", "body_length"), log)
fishdat %>% 
  ggplot(aes(y=eye_size, x=body_length, color=fish)) + geom_point() + geom_smooth(method = "lm")

#3 Compute the residuals
#CPK: this plot is clearly copied from the project. This is OK, but it has to be modified. Maybe this was included, but meant to be removed. [-2]
fishdat.log %>% 
  ggplot(aes(y=eye_size, x=body_length, col=Ecomorph2)) + geom_point() + geom_smooth(method="lm")
fishdat.log.eco.lm <- lm(body_size~eye_size*Ecomorph2, fishdat.log)

Summary(fishdat.log.eco.lm)
 
fishdat.log.eco.lm <-   lm(eye_size ~ body_length, data = fishdat)

#3 Display residuals as a boxplot 
fishdat.log <- fishdat.log %>% 
  mutate(res=residuals(fishdat.log.lm))
fishdat.log %>% 
  ggplot(aes(x=Ecomorph2,y=res)) + geom_boxplot()

#4 Two linear models 
fishdat.lm <- lm(eye_size~body_length, fishdat)

#Without interactions
#CPK: This is plotting, no modeling [-2]
lm1 <- fishdat %>% 
  ggplot(aes(body_length,eye_size)) + geom_point() + geom_abline(slope=coef(fishdat.lm)[2], intercept=coef(fishdat.lm)[1], col="blue")
#With interactions 
lm2 <- fishdat %>% 
  ggplot(aes(body_length,eye_size)) + geom_point() + geom_abline(slope=coef(fishdat.lm)[2], intercept=coef(fishdat.lm)[1], col="blue")

#5 AIC Scores
lm1.aic <- aicw(lm1$AICc)
lm2.aic <- aicw(lm2$AICc)


###CPK: Seems you're a little unsure. Have a look at the solution below to see how this could have been done.


#Total points: 6

#Solution...

dat <- read_csv("https://bcorgbio.github.io/class/data/fish_data.csv")
dat %>% 
  ggplot(aes(body_length,eye_size,col=fish))+geom_point()+geom_smooth(method="lm")


dat %>% 
  mutate(res=residuals(lm(eye_size~body_length))) %>% 
  ggplot(aes(fish,res))+geom_boxplot()

lm1 <- lm(eye_size~body_length+fish,dat)
lm2 <- lm(eye_size~body_length*fish,dat)

AIC(lm1,lm2)

