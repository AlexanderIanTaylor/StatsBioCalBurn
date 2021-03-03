library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)
muscle.incomplete <- read.table(file = '~/muscle-incomplete.txt', header = TRUE)
muscle.incomplete
range(muscle.incomplete$weight)
range(muscle.incomplete$calhour)
range(muscle.incomplete$calories,na.rm = TRUE)
#We see 24 observations, weight varies from 43.7 to 66.7, calhour varies from 13.0 to 56.0, calories varies from 212 to 352 with some missing variables.
muscle.incomplete.aggr=aggr(muscle.incomplete,numbers=TRUE,prop=FALSE,ylab=c("Histogram of missing data","Pattern"))
#from this histogram we see 8 missing values in calories (0 in the other two variables). 
muscle.incomplete.aggr
#as we said.
aggr(muscle.incomplete, combined=TRUE, numbers = TRUE,prop = TRUE, cex.numbers=0.87, varheight = FALSE)
#Easily showing us that 33% of the calories data is missing (which we could kind of deduce ourselves by dividing 8/24)
barMiss(muscle.incomplete[,c("weight","calories")])
#no visible connection between missing values in calories with weight
barMiss(muscle.incomplete[,c("calhour","calories")])
#All missing calorie values occur between 10-20 calhour. We can work with this.
#A possible explanation is that the calorie measurements do not measure below a certain thresshold.
muscle.model.omit <- lm(calories ~ calhour + weight, data=muscle.incomplete)
muscle.model.omit2 <- glm(calories ~ calhour + weight, data=muscle.incomplete)
summary(muscle.model.omit)
summary(muscle.model.omit2)
#they produce the same model but the summary of the two different ones provide some different info. Also, it's useful for future stuff.
wald.test(b=coef(muscle.model.omit2), Sigma=vcov(muscle.model.omit2),Terms=3)
wald.test(b=coef(muscle.model.omit2), Sigma=vcov(muscle.model.omit2),Terms=3)
wald.test(b=coef(muscle.model.omit2), Sigma=vcov(muscle.model.omit2),Terms=2:3)
#don't really need to do these, they just tell us more of what we already know
imp <- mice(muscle.incomplete,m=100)
#imputing with 100
summary(imp)
imp$imp$calories
final.dataset <- complete(imp,"long",inc=T)
##col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$calories))],101) ##still got to look at this
##stripplot(calories~.imp,data=final.dataset,jit=TRUE,fac=0.8,col=col,pch=20,cex=1.4,xlab="Imputation number") and this
fit <- with(data=imp, exp=lm(calories ~ calhour + weight))
fit
MI.matrix<-matrix(0,100,3)
for(k in 1:100) MI.matrix[k,]<- coefficients(fit$analyses[[k]])
MI.results=data.frame(Intercept=MI.matrix[,1],calhour=MI.matrix[,2],weight=MI.matrix[,3])
MI.results
est <- pool(fit)
summary(est)
#P value of weight very high so we'll make a model without it.
fit2 <- with(data=imp, exp=lm(calories ~ calhour))
fit2
MI.matrix2 <- matrix(0,100,2)
for(k in 1:100) MI.matrix2[k,]<- coefficients(fit2$analyses[[k]])
MI.results2=data.frame(Intercept=MI.matrix2[,1],calhour=MI.matrix[,2])
MI.results2
est2 <- pool(fit2)
summary(est2)
