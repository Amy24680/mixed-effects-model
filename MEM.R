library(lme4)
library(lmerTest)
library(rms)
library(carData)
library(languageR)
library(MuMIn)
library(caret)
library(ggplot2)
library(rlang)
library(vctrs)
library(glue)
library(tibble)
library(car)

data<-read.csv("data.csv")
head(data)
str(data)
data$cxn<-as.factor(data$cxn)
data$num_attr<-as.factor(data$num_attr)
data$position<-as.factor(data$position)
data$syn_comp<-as.factor(data$syn_comp)
data$sem_rel<-as.factor(data$sem_rel)
data$person<-as.factor(data$person)
data$num_pp<-as.factor(data$num_pp)
data$gender<-as.factor(data$gender)
data$style<-as.factor(data$style)
data$Noun<-as.factor(data$Noun)
str(data)
str(data$Noun)
levels(data$cxn)
levels(data$Noun)
levels(data$syn_comp)
data$syn_comp<-factor(data$syn_comp, levels = c("s","o","na")) 
levels(data$num_pp)
data$num_pp<-factor(data$num_pp, levels = c("sing","pl")) 
levels(data$sem_rel)
data$sem_rel<-factor(data$sem_rel, levels = c("kinsh","attr","whlpt","own","assoc","nomi")) 
summary(data$cxn)

#first model
model1<-glmer(cxn~num_attr+position+syn_comp+sem_rel+person+num_pp+gender+style+(1|Noun),data=data,family =binomial)
summary(model1)

#final model
model2<-glmer(cxn~num_attr+syn_comp+sem_rel+person+num_pp+style+(1|Noun),data=data,family =binomial)
summary(model2)

#test Singular
isSingular(model2)
model2@optinfo$conv$lme4$messages

#model fit to the data
probs = 1/(1+exp(-fitted(model2)))
somers2(probs, as.numeric(data$cxn)-1)
probs = binomial()$linkinv(fitted(model2))
somers2(probs, as.numeric(data$cxn)-1)

#classification of accuracy
predictions<-fitted(model2)
predictions.cat<-ifelse(predictions>=0.5,"wo","w")
table(data$cxn,predictions.cat)
(1937+910)/(1937+910+791+412)
2349/4050
ranef(model2)$Noun

#model validation
for (i in 1:100){
Train <- createDataPartition(data$cxn,p=0.75,list=FALSE)
training <- data[Train, ]
testing <- data[-Train, ]
model3 <- glmer(cxn~num_attr+syn_comp+sem_rel+person+num_pp+style+(1|Noun),data=training,family =binomial)
pred = predict(model3, newdata=testing,allow.new.levels = TRUE)
predictions.cat=ifelse(pred>0.5,"wo","w")
predictions.cat=as.factor(predictions.cat)
result=confusionMatrix(data=predictions.cat, testing$cxn)
print(result$overall[1])}

#test for variance inflation factor(VIF)
car::vif(model2)

#validate the importance of explanatory variable for fixed effects
Anova(model2)
