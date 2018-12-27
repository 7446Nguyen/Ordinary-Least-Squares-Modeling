#Jeff Nguyen
# Final Project

library(SDSRegressionR)
library(rms)
telco <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn-1.csv")
telco <- as_tibble(telco)

#Variable Check--------------------------------------------------------------------------------------------------
names(telco)
#TotalCharges: Quantitative, Continuous
hist(telco$TotalCharges)
fivenum(telco$TotalCharges)
3794.975-401.4

telco$StreamingMovies %>% table() #Categorical, factorize(No, Yes, No Internet Services)
telco$TechSupport %>% table() #Categorical, factorize(No, Yes, No Internet Services)
telco$PhoneService %>% table() # Categorical; Yes/No
telco$tenure %>% table() #Quantitative, Discrete
hist(telco$tenure, breaks = 20)
median(telco$tenure)
fivenum(telco$tenure)
55-9



telco$gender %>% table() #Categorical, Female/Male 
telco$MultipleLines %>% table()#Categorical, factorize(No, Yes, No Internet Services)
telco$InternetService %>% table()#Categorical, factorize(DSL, Fiber Optic, No)



#Data Cleaning----------------------------------------------------------------------------------------------------

#Remove NA's
telco_complete <-na.omit(telco)

#Dataset with variables of interest
g_telco <- telco_complete %>%  select(customerID,Churn,TotalCharges,StreamingMovies, TechSupport, PhoneService, tenure, gender,MultipleLines, InternetService )

#Filter out below values due to colinearity
data <- subset(g_telco,StreamingMovies != "No internet service"&
                       TechSupport != "No internet service"&
                       InternetService !="No")

View(data)

#Take sample n = 385 to avoid all coefficenets being significant b/c of CLT----------------------------------------
sample <- sample_n(data, 400, replace = FALSE)

#THIS FILE TO BE USED FOR THE MODEL
#write.csv(sample,"Telco Sampled Data.csv")
#View(sample)


#Factorization of Sampled Data--------------------------------------------------------------------------------------
model_data = read_csv("Telco Sampled Data.csv")


#Variable Check for Sampled Data
#Variable Check--------------------------------------------------------------------------------------------------

#TotalCharges: Quantitative, Continuous
hist(model_data$TotalCharges)
fivenum(model_data$TotalCharges)
4799.225-843.525

model_data$StreamingMovies %>% table() #Categorical, factorize(No, Yes, No Internet Services)
model_data$TechSupport %>% table() #Categorical, factorize(No, Yes, No Internet Services)
model_data$PhoneService %>% table() # Categorical; Yes/No
model_data$tenure %>% table() #Quantitative, Discrete
hist(model_data$tenure, breaks = 20)
median(model_data$tenure)
fivenum(model_data$tenure)
59.5-12.5

model_data$gender %>% table() #Categorical, Female/Male 
model_data$MultipleLines %>% table()#Categorical, factorize(No, Yes, No Internet Services)
model_data$InternetService %>% table()#Categorical, factorize(DSL, Fiber Optic, No)

model_data$Churn %>% table()
#Check for Correlation between quantitative variables
cor.test(model_data$TotalCharges,model_data$tenure)
plot(model_data$TotalCharges,model_data$tenure, 
     main = "Correlation Check for Total Charges and Tenure", xlab = " Total Charges ($)",
     ylab = "Tenure (Months)")#high correlation, remove tenure

#Set levels for model
model_data <- model_data  %>% mutate(Churn = ifelse(Churn == "Yes",1,0),
                                     StreamingMovies = factor(StreamingMovies, levels=c("No", "Yes")),
                                     TechSupport = factor(TechSupport, levels=c("No", "Yes")),
                                     PhoneService = factor(PhoneService, levels=c("No", "Yes")), 
                                     gender = factor(gender, levels=c("Female", "Male")),
                                     MultipleLines = factor(MultipleLines, levels = c("No","Yes")),
                                     InternetService = factor(InternetService, levels=c("DSL", "Fiber optic")))

str(model_data)

#Initial Model------------------------------------------------------------------------------------------------------
percent_churn <- glm(Churn ~ TotalCharges + StreamingMovies + TechSupport + PhoneService + 
                       tenure +gender  + InternetService, data = model_data, family = 'binomial')
vif(percent_churn)

summary(percent_churn)
#Remove TotalCharges because of multicolinearity with tenure and low significance, 
#remove multiple lines becuase of multicolinearity with phone services

#MultipleLines, 
#Outlier Check and removal
cooksPlot(percent_churn, key.variable = "customerID", print.obs = TRUE, sort.obs = TRUE,save.cutoff = TRUE)
cooksCutOff * 2

g_model_data <- model_data %>% 
  filter(customerID %not in% c("7721-JXEAW"," 2091-MJTFX", "6313-GIDIT","9053-JZFKV","4355-HBJHH","0117-LFRMW","4398-HSCJH","3521-SYVOR"))

g_percent_churn <- glm(Churn ~ StreamingMovies + TechSupport + PhoneService + 
                       tenure +gender  + InternetService, data = g_model_data, family = 'binomial')
summary(g_percent_churn)
lrm(Churn ~ StreamingMovies + TechSupport + PhoneService + 
    tenure +gender  + InternetService, data = g_model_data)

exp(g_percent_churn$coef)
exp(confint.default(g_percent_churn))
View(g_model_data)

library(car)
Anova(g_percent_churn, type="III")

#Final Model Construction--------------------------------------------------------------------------------------------
step(g_percent_churn)

pc_final = glm(Churn ~ StreamingMovies + TechSupport + tenure + InternetService, data = g_model_data, family = 'binomial')

lrm(Churn ~ StreamingMovies + TechSupport + tenure + InternetService +TotalCharges, data = g_model_data)

exp(pc_final$coef)
exp(confint.default(pc_final))

#Plots---------------------------------------------------------------------------------------------------------------

library(emmeans)
library(ggplot2)
library(skimr)

g_model_data %>% skim(tenure)
g_model_data %>% skim(TotalCharges)


#Tenure
pro_mns <- summary(emmeans(g_percent_churn, "tenure",at=list(tenure = seq(0, 72, .01)), type="response"))

View(pro_mns)
g <- simpleScatter(g_model_data, tenure, Churn, title="Probability of Churn given Tenure", 
                   xlab="Tenure", ylab="Churn probability")

g + 
  geom_line(data=pro_mns, aes(x=tenure, y=prob), color="red") + 
  geom_line(data=pro_mns, aes(x=tenure, y=asymp.LCL), linetype="dashed") +
  geom_line(data=pro_mns, aes(x=tenure, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = 6.21, col="blue",size = 1)




#CI Marks-------------------------------------
pro_mns2 <- summary(emmeans(g_percent_churn, "tenure",at=list(tenure = seq(0, 72, .01)), type="response"))
mark_prob <- 0.5
ci_marks <- pro_mns2 %>% 
  filter(abs(asymp.LCL - mark_prob) == min(abs(asymp.LCL - mark_prob)) |
           abs(asymp.UCL - mark_prob) == min(abs(asymp.UCL - mark_prob))) %>% 
  pull(tenure)
ci_marks#gives location of x values for a given y-hat
g + 
  geom_line(data=pro_mns2, aes(x=tenure, y=prob), color="red") + 
  geom_line(data=pro_mns2, aes(x=tenure, y=asymp.LCL), linetype="dashed") +
  geom_line(data=pro_mns2, aes(x=tenure, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = ci_marks)

