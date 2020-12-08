#install.packages("psychometric")
#install.packages('MBESS')

library(psychometric) 
library(MBESS)



#calculate confidence intervals for correlations in Study 1

CIr(r=0.063, n =251, level = .95)
CIr(r=0.138, n =251, level = .95)
CIr(r=0.088, n =251, level = .95)
CIr(r=0.072, n =251, level = .95)
CIr(r=0.080, n =251, level = .95)
CIr(r=0.819, n =251, level = .95)

#calculate confidence intervals for eta squared Supplementary Study 4

ci.pvaf(F.value=819.59, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=3.82, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=6.09, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=0.54, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=2.95, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=5.07, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=11.09, df.1=1, df.2=197, N=201, conf.level=.90)
ci.pvaf(F.value=0.02, df.1=1, df.2=197, N=201, conf.level=.90)


#calculate confidence intervals for eta squared Study 5

ci.pvaf(F.value=3.84, df.1=1, df.2=511, N=515, conf.level=.90)
ci.pvaf(F.value=21.59, df.1=1, df.2=511, N=515, conf.level=.90)
ci.pvaf(F.value=1.29, df.1=1, df.2=511, N=515, conf.level=.90)