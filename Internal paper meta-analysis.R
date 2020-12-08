install.packages("readr")
install.packages("bootES")
install.packages('metafor')
library(readr)
library(bootES)
library(metafor)

#put data into dataframes with the names
#if csv files are in drive C, works automatic, if not, change destination

Study1 <- read_csv("C:/Study1 meta.csv")
Study1b <- read_csv("C:/Study1b meta.csv")
Study2a <- read_csv("C:/Study2a meta.csv")
Study2b <- read_csv("C:/Study2b meta.csv")
Study3 <- read_csv("C:/Study3 meta.csv")
Study5 <- read_csv("C:/Study5 Indians meta.csv")
Study6a <- read_csv("C:/Study6 US meta.csv")
Study6i <- read_csv("C:/Study6 Indians meta.csv")
Study7 <- read_csv("C:/Study7 meta.csv")

#compute the difference of the ratings to four (mean of scale)

Study1$diff = Study1$`Study 1` -4
Study1b$diff = Study1b$`Study 1b` -4
Study2a$diff = Study2a$`Study 2a` -4
Study2b$diff = Study2b$`Study 2b` -4
Study3$diff = Study3$Study3 -4
Study5$diff = Study5$Study5_I -4
Study6a$diff = Study6a$Study6A -4
Study6i$diff = Study6i$Study6I -4
Study7$diff = Study7$`Study7` -4

#compute effect sizes and standard errors of effect sizes for each study

bootES(data = Study1, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study1b, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study2a, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study2b, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study3, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study5, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study6a, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study6i, data.col = "diff", effect.type = "cohens.d")
bootES(data = Study7, data.col = "diff", effect.type = "cohens.d")

#put effect sizes, standard errors of effect sizes, and country into a dataframe

Cohens_d = c(0.295, 0.368, 0.296, 0.191, 0.257, -0.136, 0.260, -0.031, 0.136)
SE = c( 0.071,0.072, 0.065, 0.054, 0.100,0.072,0.074,0.071, 0.071)
country = c(1, 1, 1, 1, 1, 2, 1, 2,1)
data = data.frame(Cohens_d, SE, country)

#make two subsets: subset a for U.S. sample, subset i for Indian sample

data_a = subset(data, country == 1)
data_i = subset(data, country == 2)

#compute meta-analysis for each subsample and test whether country is a moderator on whole sample

meta_a = rma(yi = Cohens_d, sei = SE, data = data_a)
meta_i = rma(yi = Cohens_d, sei = SE, data = data_i)
meta_a
meta_i
meta = rma(yi = Cohens_d, sei = SE, data = data, mods = country)
meta
