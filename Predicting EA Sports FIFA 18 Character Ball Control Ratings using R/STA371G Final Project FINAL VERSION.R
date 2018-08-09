#STA 371G Project
#Jeff Nguyen, ErnestoFrausto

#dataframe
#-------------------------
data <- complete

install.packages("data.table")
library(data.table)

#creates random sample of n = 200
set.seed(12)#changing this value will get you a different sample size of n
prelim_sample <- data.table(data) #assign full data set into a temporary dataset
rs <- prelim_sample[sample(.N, 200)]#create random sample variable

#exports random sample to a .csv so we can actually see which 200 were sampled
write.csv(rs, file = "random_sample_of_complete_data.csv")
attach(rs)

#Model
#-------------------------
model1 <- lm(ball_control~ preferred_foot + acceleration + reactions + 
               jumping+ strength+ shot_power +interceptions + age, data = rs)
summary(model1)

#checks for correlation between explainatory variables
cor(data[,c(44,47,49,50,52,55,7)])


#linearity check
#-------------------------
#everything checks out, if something doesn't change the number on line 13 and re-run the model until you get something you like
residual<-residuals(model1)

plot(acceleration, residuals(model1),main='Acceleration vs. Residuals')# 
abline(0,0, col='red')
plot(acceleration, ball_control) 

plot(reactions, residuals(model1), main='Reactions vs. Residuals')#
abline(0,0, col='red')
plot(reactions, ball_control)

plot(shot_power, residuals(model1), main='Shot Power vs. Residuals')
abline(0,0, col='red')
plot(shot_power, ball_control)

plot(jumping, residuals(model1), main = 'Jumping vs. Residuals')
abline(0,0, col='red')
plot(jumping, ball_control)

plot(strength, residuals(model1), main = 'Strength vs. Residuals')
abline(0,0, col='red')
plot(strength, ball_control)

plot(interceptions, residuals(model1), main = 'Interceptions vs. Residuals')
abline(0,0, col='red')
plot(interceptions, ball_control)

plot(age, ball_control)
plot(age, residuals(model1), main = 'Age vs. Residuals')
abline(0,0, col='red')

#varience check
#-------------------------
#outliers need to removed
#equal varience condition is met

plot(residuals(model1), main = 'Model Residual Plot',ylab='Model Residuals')
abline(0,0, col = 'red')

#Normailty Check
#-------------------------
#model is approx. normal
hist(residuals(model1), main = "Model Residual Distribution",
     xlab="Model 1 Residuals", col = 'green')
qqnorm(residuals(model1), main = "Model QQ Plot")
abline(mean(residuals(model1)),sd(residuals(model1)), col='red')


#Skewness Check
#-------------------------
#everything checks out
library(moments)
skewness(residuals(model1)) # no skewness, what was the critical value?  Is it greater than__??
kurtosis(residuals(model1)) #less than 10, no s


#-------------------------------------------------------------------
#-------------------------------------------------------------------
#building Model
install.packages("car")  # Companion to Applied Regression
library(car)
vif(model1)


full<- model1
null<- lm(ball_control~1)

# Forward Selection
# removed jumping and strength after forward selection, p-values for both were still not significant
step(null, scope=list(lower=null, upper=full), direction="forward")
modelForward <- lm(ball_control ~ strength+ jumping+shot_power + acceleration + interceptions + reactions, data = rs)
summary(modelForward)#better model
vif(modelForward)

confint(modelForward, level = 0.95)
#Forward Model Correlation Check
cor(rs[,c(49,44,55,47)])

#Outlier Check
hist(cooks.distance(modelForward), main = "Model Forward Cooks Distance Distribution",
     xlab='Cooks Distance', col = 'blue')
plot(cooks.distance(modelForward), main = 'Cooks Distance', ylab='Model Forward Cooks Distance')

#2SE method better since distribution is approx normal
mfResiduals <- residuals(modelForward)
meanError<- 2*sd(mfResiduals)
meanError
rs$residual<-mfResiduals
rs$standError<-sd(mfResiduals)
rs$Outliers<-ifelse(abs(mfResiduals)>meanError, 1, 0)
write.csv(rs, file = "rs_with_outliers_marked.csv")

#Normality Check
qqnorm(residuals(modelForward), main ='Model Forward QQ-Plot')
abline(mean(residuals(modelForward)),sd(residuals(modelForward)), col='red')

hist(residuals(modelForward),main = 'Model Forward Residual Distribution', 
     xlab='Model Forward Residual', col = 'green')

#model Residual Plot     
plot(residuals(modelForward),,ylab='Model Forward Residuals',
     main = 'Model Forward Residual Distribution')
abline(0,0, col = 'red')


plot(residuals(modelForward),predict(modelForward))



#----------------------------------------------------------------------------------------------
#outliers removed
fm <- rs_with_outliers_removed
attach(fm)

modelFM <- lm(ball_control~ preferred_foot + acceleration + reactions + 
               jumping+ strength+ shot_power +interceptions + age, data = fm)
full3<- modelFM
null3<- lm(ball_control~1)

# Forward Selection
# removed jumping and strength after forward selection, p-values for both were still not significant
step(null3, scope=list(lower=null3, upper=full3), direction="forward")
modelForward2 <- lm(ball_control ~ shot_power + acceleration + interceptions + reactions, data = fm)
summary(modelForward2)#better model
vif(modelForward2)

#Forward Model Correlation Check
cor(fm[,c(49,44,55,47)])

#Outlier Check
hist(cooks.distance(modelForward2), main = "Model Forward Cooks Distance Distribution",
     xlab='Cooks Distance', col = 'blue')
plot(cooks.distance(modelForward2), main = 'Cooks Distance', ylab='Model Forward Cooks Distance')

cooksD2<-cooks.distance(modelForward2)
rs$CookD<-ifelse(cooksD2>0.04, 1, 0)
write.csv(rs, file = "rs_with_outliers_marked2.csv")

#Normality Check
qqnorm(residuals(modelForward2), main ='Model Forward QQ-Plot')
abline(mean(residuals(modelForward2)),sd(residuals(modelForward2)), col='red')

hist(residuals(modelForward2),main = 'Model Forward Residual Distribution', 
     xlab='Model Forward Residual', col = 'green')

#model Residual Plot     
plot(residuals(modelForward2),,ylab='Model Forward Residuals',
     main = 'Model Forward Residual Distribution')
abline(0,0, col = 'red')


plot(residuals(modelForward),predict(modelForward))
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

fm2 <- rs_with_outliers_removed2
attach(fm2)

modelFM2 <- lm(ball_control~ preferred_foot + acceleration + reactions + 
                jumping+ strength+ shot_power +interceptions + age, data = fm2)
full4<- modelFM2
null4<- lm(ball_control~1)

# Forward Selection
# removed jumping and strength after forward selection, p-values for both were still not significant
step(null4, scope=list(lower=null4, upper=full4), direction="forward")
modelForward3 <- lm(ball_control ~ shot_power + acceleration + interceptions + reactions, data = fm2)
summary(modelForward3)#better model
vif(modelForward3)

#Forward Model Correlation Check
cor(fm2[,c(50,45,51,49)])

#Outlier Check
hist(cooks.distance(modelForward3), main = "Model Forward Cooks Distance Distribution",
     xlab='Cooks Distance', col = 'blue')
plot(cooks.distance(modelForward3), main = 'Cooks Distance', ylab='Model Forward Cooks Distance')

cooksD3<-cooks.distance(modelForward3)
rs$CookD<-ifelse(cooksD2>0.04, 1, 0)
write.csv(rs, file = "rs_with_outliers_marked3.csv")


#Normality Check
qqnorm(residuals(modelForward3), main ='Model Forward QQ-Plot')
abline(mean(residuals(modelForward3)),sd(residuals(modelForward3)), col='red')

hist(residuals(modelForward3),main = 'Model Forward Residual Distribution', 
     xlab='Model Forward Residual', col = 'green')

#model Residual Plot     
plot(residuals(modelForward3),,ylab='Model Forward Residuals',
     main = 'Model Forward Residual Distribution')
abline(0,0, col = 'red')


plot(residuals(modelForward3),predict(modelForward3))

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

fm3 <- rs_with_outliers_removed3
attach(fm3)

modelFM3 <- lm(ball_control~ preferred_foot + acceleration + reactions + 
                 jumping+ strength+ shot_power +interceptions + age, data = fm3)
full5<- modelFM3
null5<- lm(ball_control~1)

# Forward Selection
# removed jumping and strength after forward selection, p-values for both were still not significant
step(null5, scope=list(lower=null5, upper=full5), direction="forward")
modelForward4 <- lm(ball_control ~ shot_power + acceleration + interceptions + reactions, data = fm3)
summary(modelForward4)#better model
vif(modelForward4)

#Forward Model Correlation Check
cor(fm2[,c(50,45,56)])

#Outlier Check
hist(cooks.distance(modelForward4), main = "Model Forward Cooks Distance Distribution",
     xlab='Cooks Distance', col = 'blue')
plot(cooks.distance(modelForward4), main = 'Cooks Distance', ylab='Model Forward Cooks Distance')

cooksD3<-cooks.distance(modelForward4)
rs$CookD<-ifelse(cooksD2>0.04, 1, 0)
write.csv(rs, file = "rs_with_outliers_marked3.csv")


#Normality Check
qqnorm(residuals(modelForward4), main ='Model Forward QQ-Plot')
abline(mean(residuals(modelForward4)),sd(residuals(modelForward4)), col='red')

hist(residuals(modelForward4),main = 'Model Forward Residual Distribution', 
     xlab='Model Forward Residual', col = 'green')

#model Residual Plot     
plot(residuals(modelForward4),,ylab='Model Forward Residuals',
     main = 'Model Forward Residual Distribution')
abline(0,0, col = 'red')


plot(residuals(modelForward4),predict(modelForward4))
