


install.packages("psych")
install.packages("nlme")
install.packages("lme4")
install.packages("lmerTest")
install.packages("effects")
install.packages("ggplot2")
install.packages("easyGgplot2")
install.packages("car")
install.packages("dplyr")
install.packages("lattice")
install.packages("rmcorr")
install.packages("stargazer")
install.packages("lattice")
install.packages("Hmisc")
install.packages("cooccur")
install.packages("brms")
install.packages("psych")
install.packages("sjstats")
install.packages("corrplot")
install.packages("DescTools")
install.packages("sjPlot")
install.packages("RColorBrewer")
install.packages("EMAtools")
install.packages("simr")
install.packages("effectsize")
remove.packages("rlang")
remotes::install_github("marklhc/bootmlm")

library(nlme)
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(car)
library(lattice)
library(dplyr)
library(ggpubr)
library(easyGgplot2)
library(rmcorr)
library(stargazer)
library(lattice)
library(Hmisc)
library(cooccur)
library(brms)
library(psych)
library(sjstats)
library(corrplot)
library(DescTools)
library(sjPlot)
library(RColorBrewer)
library(tidyverse)
library(EMAtools)
library(simr)
library(effectsize)

rm(list=ls())


#here and elsewhere: set directory to your working directory
#setwd ("")

setwd("")

##load the data
  
d <- readRDS("Multilevel regression analyses.RDS")

options(scipen = 999)
names(d)
########################################################################################################
#################################### Descriptives and Frequencies ######################################
########################################################################################################



### Sample description in methods part

## Overall Participants

# n

n_distinct((d$participant), na.rm=TRUE)


## age, sex, job 

#age

#only retain rows unique to two criteria
real_age=distinct(d,participant,age)


mean(real_age$age,na.rm=T)
sd(real_age$age,na.rm = T)


#sex

prop.table(table(d$sex))


#################################################################################################################################################
#################################################### MAIN ANALYSES ##############################################################################
#################################################################################################################################################

dm <- subset(d, select = c(participant, c_result.rc, c_domain))

dm <- na.omit(dm)


# standardization of DV
dm$c_result.rc.z <-scale(dm$c_result.rc, center=TRUE, scale=TRUE)

# dummy coding: domain "morality" vs others


dm$morality_cat[dm$c_domain == "Moral/ethical behavior"] = "morality"

dm$morality_cat[dm$c_domain != "Moral/ethical behavior" ] = "non-morality"

dm$morality_cat = factor(dm$morality_cat, levels = c("morality","non-morality"))

dm <- dm %>%
  mutate(morality_cat = relevel(morality_cat, ref = "non-morality"))

contrasts(dm$morality_cat)



############### HYPOTHESIS 1 (preregistered) ################################
## People are more likely to compare downward compared to upward in morality than in other domains.



# set dummy score to numeric to allow effect size calculation

dm$morality_cat_d <- as.numeric(dm$morality_cat)

#calculate model


m1.d <- lmer(c_result.rc.z ~ morality_cat_d + 
             (1 | participant), data = dm, REML="false")
summary(m1.d)
anova(m1.d)




# effect size d

lme.dscore(m1.d,data = dm,type = "lme4")

fixef(m1.d)["morality_cat_d"]


# confidence interval
# Covariance preserving residual bootstrap

mySumm <- function(x) 
{
  c(getME(x, "beta"), sigma(x))
}


boo01 <- bootstrap_mer(m1.d, mySumm, type = "residual", nsim = 100)
# Get confidence interval
boot.ci(boo01, index = 2, type = c("norm", "basic", "perc"))


#### with domain importance as covariate


dm_i <- subset(d, select = c(participant, c_result.rc, c_domain, c_importance.cnt))

dm_i <- na.omit(dm_i)


# standardization of DV
dm_i$c_result.rc.z <-scale(dm_i$c_result.rc, center=TRUE, scale=TRUE)

# dummy coding: domain "morality" vs others


dm_i$morality_cat[dm_i$c_domain == "Moral/ethical behavior"] = "morality"

dm_i$morality_cat[dm_i$c_domain != "Moral/ethical behavior" ] = "non-morality"

dm_i$morality_cat = factor(dm_i$morality_cat, levels = c("morality","non-morality"))

dm_i <- dm_i %>%
  mutate(morality_cat = relevel(morality_cat, ref = "non-morality"))

contrasts(dm_i$morality_cat)


# set dummy score to numeric to allow effect size calculation

dm_i$morality_cat_d <- as.numeric(dm_i$morality_cat)


#calculate model

m2co.d <- lmer(c_result.rc.z ~ morality_cat_d + c_importance.cnt +
               (1 | participant), data = dm_i, REML="false")
summary(m2co.d)
anova(m2co.d)


# effect size d

lme.dscore(m2co.d,data = dm_i,type = "lme4")

fixef(m2co.d)["morality_cat_d"]


# confidence interval
# Covariance preserving residual bootstrap


boo02 <- bootstrap_mer(m2co.d, mySumm, type = "residual", nsim = 100)
# Get confidence interval
boot.ci(boo02, index = 2, type = c("norm", "basic", "perc"))


## exploratory: comparing "morality" to the domains "smoking" and "alcohol"

## Smoking

dm_smoke <- dm

dm_smoke$morality_smoke[dm_smoke$c_domain == "Moral/ethical behavior"] = "morality"
dm_smoke$morality_smoke[dm_smoke$c_domain == "Smoking"] = "smoke"


dm_smoke$morality_smoke=factor(dm_smoke$morality_smoke, levels = c("morality","smoke"))

dm_smoke <- dm_smoke %>%
  mutate(morality_smoke = relevel(morality_smoke, ref = "smoke"))



# set dummy score to numeric for effect size calculation

dm_smoke$morality_smoke_d <- as.numeric(dm_smoke$morality_smoke)


#take subset
dm_smoke <- na.omit(dm_smoke)

#calculate model


m3.d <- lmer(c_result.rc.z ~ morality_smoke_d + 
               (1 | participant), data = dm_smoke, REML="false")
summary(m3.d)

# effect size d
anova(m3.d)
lme.dscore(m3.d,data = dm_smoke,type = "lme4")

fixef(m3.d)["morality_smoke_d"]




# confidence interval
# Covariance preserving residual bootstrap



boo03 <- bootstrap_mer(m3.d, mySumm, type = "residual", nsim = 100)

# Get confidence interval
boot.ci(boo03, index = 2, type = c("norm", "basic", "perc"))




## Alcohol

dm_alc = dm

dm_alc$morality_alc[dm_alc$c_domain == "Moral/ethical behavior"] = "morality"
dm_alc$morality_alc[dm_alc$c_domain == "Alcohol"] = "alc"



dm_alc$morality_alc=factor(dm_alc$morality_alc, levels = c("morality","alc"))


dm_alc <- dm_alc %>%
  mutate(morality_alc = relevel(morality_alc, ref = "alc"))

#take subset
dm_alc <- na.omit(dm_alc)





# set dummy score to numeric for effect size calculation

dm_alc$morality_alc_d <- as.numeric(dm_alc$morality_alc)



#calculate model

m4.d <- lmer(c_result.rc.z ~ morality_alc_d + 
               (1 | participant), data = dm_alc, REML="false")
summary(m4.d)
anova(m4.d)

# effect size d
lme.dscore(m4.d,data = dm_alc,type = "lme4")

fixef(m4.d)["morality_alc_d"]



# confidence interval
# Covariance preserving residual bootstrap



boo04 <- bootstrap_mer(m4.d, mySumm, type = "residual", nsim = 100)

# Get confidence interval
boot.ci(boo04, index = 2, type = c("norm", "basic", "perc"))


#power analysis



###simr


###change effect size
fixef(m1.d)["morality_cat_d"]



###change to desired value: .20
fixef(m1.d)['morality_cat_d'] <- 0.20

powerSim(m1.d, test = fixed("morality_cat_d"), nsim= 100)




###change to desired value: .50

fixef(m1.d)['morality_cat_d'] <- 0.50

powerSim(m1.d, test = fixed("morality_cat_d"), nsim= 100)



###change to desired value: .80

fixef(m1.d)['morality_cat_d'] <- 0.80

powerSim(m1.d, test = fixed("morality_cat_d"), nsim= 100)





####################################################### End of Analyses ###################################################################


