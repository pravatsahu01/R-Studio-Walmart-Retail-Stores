
#Walmart Sales Prediction with R Programming
#Introduction
#Problem: To predict sales
#Solution: To Build Multiple linear regression model  

#Step 1: Identify dependent and Independent variables
#Dependent Variable: Weekly Sales
#Independent variable: All other variables

#Step2: Data Understanding

#There are 6435 observations and 8 variables in the data. There are 6435 observations and 8 variables in the data. The variables are Store number, Date, Weekly Sales, Holiday flag which are marked as 0 if date falling on non-holiday and 1 if the date falling on holiday, temperature for week, fuel price for the week, prevailing consumer price index for the week, and lastly the unemployment rate for the week.

#Step3: Import data to Lab

walmart_data <- read.csv("Walmart_Store_sales.csv", header= TRUE,sep=",")
View(walmart_data)

#Step4: Data exploration
str(walmart_data)
head(walmart_data)
tail(walmart_data)
nrow(walmart_data)
ncol(walmart_data)
dim(walmart_data)
class(walmart_data)
table(walmart_data$Store)
summary(walmart_data)

#To change the data variable to Date Format from Character Format.

walmart_data$Date <- as.Date(walmart_data$Date, format = "%d-%m-%Y")
str(walmart_data)


#Step5: Dividing data into Training (70%) and Testing (30%) ratio.

library(caTools) #load package

#set the seed to freeze the sample 
set.seed(1)

sample <- sample.split(walmart_data$Weekly_Sales, SplitRatio=0.7)
sample
train_data <- subset(walmart_data, sample== TRUE )
test_data<- subset(walmart_data,sample==FALSE)

#Step6: Build the Model using Training data

model <- lm(Weekly_Sales ~ ., data= train_data)

summary(model)

#Step 7: Look at the Coefficient Table to determine the significant variables.

#Step 8: Rerun the model using significant independent variables: 

model1 <- lm(Weekly_Sales ~ Store + Holiday_Flag + Temperature + CPI + Unemployment , data= train_data)

summary(model1)


#Step 9: Rerun the model using significant independent variables: 

model2 <- lm(Weekly_Sales ~ Store + Holiday_Flag + CPI + Unemployment , data= train_data)

summary(model2)


#Step 10: prediction  on testing data set 

predtest<- predict(model2,test_data)

predtest

#attach it with the dataframe
predtest1<-data.frame(predtest)

#to bind the predicted data set with test data set by cbind function
final_data<- cbind(test_data,predtest1)
View(final_data)


#cal rmse 
sqrt(mean((final_data$Weekly_Sales - final_data$predtest)^2))


write.csv(final_data,"walmart_sale_prediction_output.csv")
View(walmart_sale_prediction_output)

sales_prediction_10 <- head(walmart_sale_prediction_output,10)
View(sales_prediction_10)














