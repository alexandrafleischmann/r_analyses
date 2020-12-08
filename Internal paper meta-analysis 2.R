#install.packages("readr")
#install.packages("bootES")
#install.packages("metafor")
#install.packages("esc")
#install.packages("compute.es")
  
  
library(readr)
library(bootES)
library(metafor)
library(esc)
library(compute.es)


#import csv

#meta-analysis of main effect

Study2 <- read_csv("C:/Meta Study 2.csv")
Study3 <- read_csv("C:/Meta Study 3.csv")
Study4 <- read_csv("C:/Meta Study 4.csv")
Study5 <-read_csv("C:/Meta Study 5.csv")

bootES(Study2, data.col = "approval", group.col = "condition", effect.type = "cohens.d", contrast = c("1" = -1, "2" = 1))
bootES(Study3, data.col = "approval", group.col = "condition", effect.type = "cohens.d", contrast = c("1" = -1, "2" = 1))
bootES(Study4, data.col = "approval", group.col = "condition", effect.type = "cohens.d", contrast = c("0" = -1, "1" = 1))
bootES(Study5, data.col = "approval", group.col = "condition", effect.type = "cohens.d", contrast = c("0" = -1, "1" = 1))


Cohensd1 = c(-0.041, 0.39, 0.34, 0.40)
SE1 = c(0.15, 0.12, 0.15, 0.09)
data1 <- data.frame(Cohensd1, SE1)
meta1 <- rma(yi = Cohensd1, sei = SE1, data = data1)
meta1

#meta-analysis of moderation

esc_t(p = .015, totaln = 201, es.type = "d")
esc_t(p = .176, totaln = 292, es.type = "d")
fes(f = 0.54, n.1 = 102, n.2 = 99, dig = 3)
sqrt(0.02)
fes(f = 1.29, n.1 = 264, n.2 = 251, dig = 3)
sqrt(0.008)


Cohensd2 = c(0.346, 0.159, 0.104, 0.100)
SE2 = c(0.142, 0.117, 0.141, 0.089)
data2 <- data.frame(Cohensd2, SE2)
meta2 <- rma(yi = Cohensd2, sei = SE2, data = data2)
meta2
