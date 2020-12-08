#put in the correlations and n

r <- c(0.087, 0.094, 0.163, 0.037)
n <- c(535, 535, 535, 535)

#calculate error

library(metafor)

corr <- escalc ("COR", ri = r, ni = n)
SE <- sqrt (corr$vi)

SE

#calculate CI

install.packages("psychometric")
library(psychometric) 


CIr(r=0.087, n = 535, level = .95)
CIr(r=0.094, n = 535, level = .95)
CIr(r=0.163, n = 535, level = .95)
CIr(r=0.037, n = 535, level = .95)