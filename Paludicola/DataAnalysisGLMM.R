library(dplyr)
library(tidyr)
library(lme4)
library(MASS)
library(tree)
library(sjPlot)
library(MuMIn)


#First, we are going to work with the dataframe with 6 VAR. and  79 obs.including Tarso
#to see if it's a relevant variable or not. I mark the model with 5 or 6 to know if it
#includes Tarso (6 variables) or not (5 variables)

attach(paludicolatarso)

model6Coord <- lmer(CoordDec~P3+Peso+Ala+Musculo+Grasa+Tarso+(1|Anyo))
model6Coord1 <- lmer(CoordDec~P3+Peso+Ala+Musculo+Tarso+(1|Anyo))

model6Dia <- lmer(Dia~P3+Peso+Ala+Musculo+Grasa+Tarso+(1|Anyo))
model6Dia1 <- lmer(Dia~P3+Peso+Ala+Musculo+Tarso+(1|Anyo))

detach(paludicolatarso)


# Automatic model selection -----------------------------------------------

attach(paludicolatarso)

#Here we see that the variable Tarso is not relevant when we are studying
#the latitud, we need to pick a deltaAIC > 5 to see Tarso in 1 model
options(na.action = "na.fail")
dredge(model6Coord, evaluate = TRUE,  rank = "AICc")
tab_model(model6Coord, transform=NULL, show.ci=FALSE)

options(na.action = "na.fail")
dredge(model6Coord1, evaluate = TRUE,  rank = "AICc")
tab_model(model6Coord1, transform=NULL, show.ci=FALSE)

options(na.action = "na.fail")
dredge(model6Dia, evaluate = TRUE,  rank = "AICc")
tab_model(model6Dia, transform=NULL, show.ci=FALSE)

options(na.action = "na.fail")
dredge(model6Dia1, evaluate = TRUE,  rank = "AICc")
tab_model(model6Dia1, transform=NULL, show.ci=FALSE)
