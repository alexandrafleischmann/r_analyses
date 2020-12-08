#install.packages('metafor')


library(metafor)


#Meta-Analysis

#input correlations with deontology and conventional analysis (utilitarianism vs. deontology), N per study and study names

corr1 <- c(.220, .165, .144, .113)
numb1 <- c(252, 252, 247, 338)
study_name1 <- c("Study 1", "Study 2", "Study 3", "Study 4")


#input correlations of deontology and utilitarianism with process dissociation, N per study, study names, study as variable, and which parameter (deontology vs. utilitarianism)

corr2 <- c(-.238, .006, -.106, .062, -.108, -.009)
numb2 <- c(252, 252, 247, 247, 338, 338)
study_name2 <- c("Study 2", "Study 2", "Study 3", "Study 3", "Study 4", "Study 4")
study2 <- c(1, 1, 2, 2, 3, 3)
parameter <- factor(c("deontological","utilitarian","deontological", "utilitarian", "deontological", "utilitarian"))


#input correlations of deontology with ability and opinion subscales, N per study, study names, study as variable, and which subscale

corr3 <- c(-.215, -.217, -.167, -0.0005, -.150, -.034)
numb3 <-c(252, 252, 247, 247, 338, 338) 
study_name3 <- c("Study 2", "Study 2", "Study 3", "Study 3", "Study 4", "Study 4")
study3<-c(1, 1, 2, 2, 3, 3)
mod3 <- c(1, 2, 1, 2, 1, 2)
subscale <- factor(c("ability", "opinion", "ability", "opinion", "ability", "opinion"))


data1 <- data.frame(corr1, numb1, study_name1)
data2 <- data.frame(corr2, numb2, study_name2, study2, parameter)
data3 <- data.frame(corr3, numb3, study_name3, study3, subscale, mod3)

#calculate variance for effect sizes and save in dataframe

ve1 <- escalc ("COR", ri = corr1, ni = numb1)
ve2 <- escalc ("COR", ri = corr2, ni = numb2)
ve3 <- escalc("COR", ri = corr3, ni = numb3)


data1$ve <- ve1$vi
data2$ve <- ve2$vi
data3$ve <- ve3$vi


#calculate covariances of correlations
#calculation following Olkin & Finn, 1989, "Testing Correlated Correlations", Psychological Bulletin


covariances <- function(r12, r13, r23, n){
cov <-(1/n)*((r23-(1/2)*r12*r13)*(1-(r12*r12)-(r13*r13))+(1/2)*r12*r13*r23*r23)
  return(cov)
}

#calculate covariances of correlations of SCO with deontological and utilitarian decisions
#Variable 1: SCO, variable 2: deontological decisions, variable 3: utilitarian decisions

cov1.1 <- covariances(-.238, .006, .007, 252)
cov1.2 <- covariances(-0.097, 0.062, 0.139, 247)
cov1.3 <- covariances(-0.108, -0.009, 0.244, 338)


#calculate covariances of correlations of deontological decisions with SCO ability and SCO opinion
#Variable 1: deontological decisions, variable 2: ability comparison subscale, variable 3: opinion comparison subscale

cov2.1<-covariances(-.215, -.217, .643, 252)
cov2.2<-covariances(-.167, -0.0005, .695, 247)
cov2.3<-covariances(-.150, -.034, .717, 338)


#make matrix of variances and covariances and add to dataframe

varcovar1.1 <- c(ve2$vi[1], cov1.1, ve2$vi[3], cov1.2, ve2$vi[5], cov1.3)
varcovar1.2 <- c(cov1.1, ve2$vi[2], cov1.2, ve2$vi[4], cov1.3, ve2$vi[6])

varcovar2.1 <- c(ve3$vi[1],cov2.1, ve3$vi[3], cov2.2, ve3$vi[5], cov2.3)
varcovar2.2 <- c(cov2.1, ve3$vi[2], cov2.2, ve3$vi[4], cov2.3, ve3$vi[6])

data2$var1 <-varcovar1.1
data2$var2 <-varcovar1.2

data3$var1 <- varcovar2.1
data3$var2 <- varcovar2.2

#construct full (blockdiagonal) variance-covariance

V2 <- bldiag(lapply(split(data2[,c("var1", "var2")], data2$study2), as.matrix))
V3 <- bldiag(lapply(split(data3[,c("var1", "var2")], data3$study3), as.matrix))


#calculate meta-analysis for effect on conventional deontology vs. utilitarianism


meta1 <- rma.uni(yi = corr1, vi = ve, data = data1, slab = study_name1, method="REML", digits = 3)
summary(meta1)


forest (meta1, xlab="Pearson's r", mlab="RE Model Conventional Analysis", digits = 3)
par(font=2)
text(-0.34, 5.5, "Studies")
text(0.6, 5.5, "Pearson's r [95% CI]")


#calculate meta-analysis for process dissociation analysis: different parameters as moderator
meta2 <- rma.mv(yi= corr2, V = V2, mods = ~ parameter, random = ~ parameter | study2, data = data2, slab = study_name2, method="ML", digits = 3)
summary(meta2)
meta2.2 <- rma.mv(yi= corr2, V = V2, mods = ~ parameter - 1, data = data2, method="ML", digits = 3)
summary(meta2.1)

#make forest plot of process dissociation meta-analyses

forest(x = data2$corr2, vi =data2$ve, xlim = c(-1.5, 1), xlab="Pearson's r", slab = data2$study_name2, rows=c(10, 4, 9, 3, 8, 2), 
       ylim = c(1, 13.5), digits = 3)


forest(x = data2$corr2, vi =data2$ve, xlim = c(-1.5, 1), xlab="Pearson's r", slab = data2$study_name2, rows=c(10, 4, 9, 3, 8, 2), ylim = c(1, 13.5), digits = 3)
addpoly(x = meta2.2$beta[1], sei = meta2.2$se[1], row=7, mlab="RE Model Process Dissociation", digits = 3)
addpoly(x = meta2.2$beta[2], sei = meta2.2$se[2], row=1, mlab="RE Model Process Dissociation", digits = 3)
par(font=2)
text(-1.5, 11, pos=4, "Deontological PD Parameter")
text(-1.5, 5, pos=4,"Utilitarian PD Parameter")
text(-1.5, 12.5, pos=4,"Studies")
text(1, 12.5, pos=2, "Pearson's r [95% CI]")

#calculate meta-analysis for subscales and deontological decisions with subscales as moderator

meta3 <- rma.mv(yi= corr3, V = V3, mods = ~ subscale, random =  ~ subscale | study3, data = data3, slab = study_name3, method="ML", digits = 3)
summary(meta3)
meta3.2 <- rma.mv(yi= corr3, V = V3, mods = ~ subscale - 1, data = data3, method="ML", digits = 3)
summary(meta3.2)


#make forest plot of subscale meta-analyses

forest(x = data3$corr3, vi =data3$ve, xlim = c(-1.5, 1), xlab="Pearson's r", slab = data3$study_name3, rows=c(10, 4, 9, 3, 8, 2), 
       ylim = c(1, 13.5), digits = 3)
addpoly(x = meta3.2$beta[1], sei = meta3.2$se[1], row=7, mlab="RE Model Process Dissociation", digits = 3)
addpoly(x = meta3.2$beta[2], sei = meta3.2$se[2], row=1, mlab="RE Model Process Dissociation", digits = 3)

par(font=2)
text(-1.5, 11, pos=4, "Ability Subscale")
text(-1.5, 5, pos=4,"Opinion Subscale")
text(-1.53, 12.5, pos=4,"Studies")
text(1, 12.5, pos=2, "Pearson's r [95% CI]")


