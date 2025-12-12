#  READING CREDSCO.TXT

dd <- read.table("D:/karina/docencia/areferenciesPPT/0DadesPractiques/CREDSCO/credscoClean.csv",header=T, sep=";")

dim(dd)

names(dd)
attach(dd)
sapply(dd, class)

summary(dd)

summary(Dictamen)
table(Dictamen,useNA="ifany" )


#Build the response binary according to bernoulli coding: 1=Target event
#DictamenBern<-Dictamen
#DictamenBern[DictamenBern == 0 ] <- NA
#DictamenBern[DictamenBern == 2 ] <- 0

#table(DictamenBern)

#build logistic regression
model1<- glm(Dictamen~ Edad+RatiFin+Tipo.trabajo, family = binomial, data = dd)
class(model1)

#model1= glm(DictamenBern~ Edad+Rati.Fin+Tipo.trabajo, family = binomial, data = dd)
model1

#more information
summary(model1)


#relevel permet decidir quin nivell volem de referència pel terme independent


#install.packages("dummies")
library(dummies)

typeWork<-dummy(Tipo.trabajo)
typeWork
head(typeWork)
head(Tipo.trabajo)

class(typeWork)

barplot(table(Tipo.trabajo))
summary(typeWork)

barplot(typeWork)

barplot(typeWork, cex.names=0.9, las=2)
x<-barplot(typeWork, cex.names=0.9, xaxt="n")
text( x=x, y=0, labels=dimnames(typeWork)[[2]], adj=1, xpd=TRUE, srt=20)


model2= glm(Dictamen~ Edad+RatiFin+typeWork, family = binomial, data = dd)
summary(model2)
head(typeWork)

model2= glm(Dictamen~ Edad+RatiFin+typeWork[,-5], family = binomial, data = dd)
summary(model2)

model2= glm(Dictamen~ Edad+RatiFin+typeWork[,2:4], family = binomial, data = dd)
summary(model2)


model2$rank
model2$family
model2$method

# -2MaxLogLikelihood
model2$deviance
model2$null.deviance
deltadev<-model2$null.deviance -model2$deviance
deltadev
#1-pchisq()

model2$aic

#confidence intervals for coefficients, based on the profiles log-likelihood function
confint(model2)
#requires special package wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)



#stepwise using AIC (Akaike Information Criterion)
step(model2)


n<-dim(dd)[1]

learn <- sample(1:n, round(0.67*n))


length(Edad)
length(RatiFin)
length(Tipo.trabajo)

dim(dd[learn,])

length(Edad[learn])
length(RatiFin[learn])
length(Tipo.trabajo[learn])

model2= glm(Dictamen[learn] ~ Edad[learn]+RatiFin[learn]+typeWork[learn,2:4], family = binomial, data = dd[learn,])
summary(model2)

#Compare deviances

#return to complete model
model2= glm(Dictamen~ Edad+Rati.Fin+typeWork[,-5], family = binomial, data = dd)
summary(model2)

#interpret coefficients
attributes(model2)
model2$coefficients

exp(model2$coefficients)
#relevel(typeWork,"temporal")
#head(typeWork)
#relevel(typeWork,3)

indexos<-c(2,4)
model2= glm(DictamenBern~ Edad+RatiFin+typeWork[,indexos], family = binomial, data = dd)
summary(model2)
exp(model2$coefficients)


plot(model2$linear.predictors,model2$fitted.values)
plot(model2)


#outliers
anova(model2, test="Chisq")

#compare a sequence of models
anova(model1, model2,  test="Chisq")
anova(step(model2),  test="Chisq")


#Built dummies for all qualitative variables, 
#Built a complete model for Dictament, with all variables as explanatories
#stepwise the resulting model and see which variables should be finally retained


#YOUR WORK.......

# Analyze errors en train, test i plots

#si hi ha molts regressors
#regressors<-c(1:nCol)
#exclosos<-c(1,5:7, 8,10,19:21,24:26,28:30,32:34,36:39,41:42,44:46,60:63,65:66,68,81:82,93,96:100,102:104,112,114,118:121,124:125,130,132,136:140,142,147:148)
#regressorsActius<- regressors[-exclosos]
#var<-paste0("Train[,",regressorsActius,"]")
#formula<-as.formula(paste("Train[,1]~ ",paste(var,collapse="+")))
#model= glm(formula , family = binomial, data = Train)
#step(model)
