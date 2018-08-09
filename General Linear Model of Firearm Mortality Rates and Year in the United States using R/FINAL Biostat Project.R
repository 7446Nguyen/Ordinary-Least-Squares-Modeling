setwd('C:/Users/Jeffrey/Desktop')
prelim <- read.csv("Prelim Dataset.csv")

library(car)

#interactions
install.packages("data.table")
library(data.table)

set.seed(11)
prelim_sample <- data.table(prelim)
rs <- prelim_sample[sample(.N, 100)]

rs$Year <- relevel(rs$Year, "year2016")
rs$HR_Centered <- rs$Log.Transform.Homicide.Mortality.Rate - mean(rs$Log.Transform.Homicide.Mortality.Rate)

int_model <- lm(Firearm.Mortality.Rate ~ HR_Centered *Year, data = rs)
summary(int_model)



#conclusion for each main efect and interaction effect, 3 research questions hypothesis(2 main effects, 1 interaction)
#interaction plot
year_2014 <- rs[rs$Year == "year2014", ]
year_2015 <- rs[rs$Year == "year2015", ]
year_2016 <- rs[rs$Year == "year2016", ]

plot(year_2014$Log.Transform.Homicide.Mortality.Rate,year_2014$Firearm.Mortality.Rate, col = "red", cex = 0.8, 
     main = "Firearm Mortality Rate by 
     Log Transformed Homicide Mortality Rate and Year", xlab = "Centered Log Transformed Homicide Mortality Rate", 
     ylab = "Firearm Mortality Rate", xlim = c(0, 13), ylim = c(2, 20))
points(year_2015$Log.Transform.Homicide.Mortality.Rate, year_2015$Firearm.Mortality.Rate, pch = 17,col = "blue", cex = 0.8)
points(year_2016$Log.Transform.Homicide.Mortality.Rate,year_2016$Firearm.Mortality.Rate, pch = 16,col = "green", cex = 0.8)


abline(lm(Firearm.Mortality.Rate ~ Log.Transform.Homicide.Mortality.Rate, data = year_2014), col = "red")
abline(lm(Firearm.Mortality.Rate ~ Log.Transform.Homicide.Mortality.Rate, data = year_2015), col = "blue", lty = 2)
abline(lm(Firearm.Mortality.Rate ~ Log.Transform.Homicide.Mortality.Rate, data = year_2016), col = "green", lty = 3)

legend("bottomright", title = "Year", c("2014", "2015", "2016"), col = c("red", "blue","green"), pch = c(17, 16), inset = 0.0001)


#homicde rate is significant in regualr GLM
#homicide rate is not significant in interaction becasue as year passes particularly 2014 and 2015 where there is no 
#interaction; 2016 has interaction with 2014 and 2015, but is not significant enough to lower p-value in GLM
# with interaction
#spike in 2016, but not enough to shift model


#---------------------------------------------------------------------------------------------------------------------
#GLM w/o Interaction
homicide_MR_LT <-prelim$Log.Transform.Homicide.Mortality.Rate
firearm_MR <- prelim$Firearm.Mortality.Rate

multi_model <- lm(firearm_MR ~ homicide_MR_LT + prelim$Year, data = prelim)
plot(homicide_MR_LT, firearm_MR, xlab = "Homicide Mortality Rate", ylab = "Firearm Mortality Rate", 
     main = "Homicide vs. Firarm Mortality Rates", pch = 20)

boxplot(firearm_MR ~ prelim$Year, ylab = "Firearm Mortality Rate", 
        main = "Figure 7: Firearm Mortality Rate Distribution by Year", 
        col = c("light blue", "light green", "light pink"))

hist(multi_model$residuals, main = "Model Residuals", xlab = "Residual", 
     col = "light grey", right = F)

plot(multi_model$fitted.values, multi_model$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residual Plot", pch = 20)
abline(h = 0, col = "red")
summary(multi_model)
#assumptions, if not enough space for assumptions graph, then describe graphs in detail

