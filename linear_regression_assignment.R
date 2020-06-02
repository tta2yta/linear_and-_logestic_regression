install.packages("e1071")
install.packages("DAAG")
library("HSAUR")
data_heptathlon<- heptathlon
head(data_heptathlon)

# scatterplot
scatter.smooth(x=data_heptathlon$run200m, y=data_heptathlon$longjump, main="longjump~run200m") 

#BoxPlot - Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(data_heptathlon$run200m, main="run200m", sub=paste("Outlier rows: ", boxplot.stats(data_heptathlon$run200m)$out))  # box plot for 'run200m'
boxplot(data_heptathlon$longjump, main="longjump", sub=paste("Outlier rows: ", boxplot.stats(data_heptathlon$longjump)$out))  # box plot for 'longjump'

#Density plot - Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(data_heptathlon$run200m), main="Density Plot: run200m", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data_heptathlon$run200m), 2)))  # density plot for 'run200m'
polygon(density(data_heptathlon$run200m), col="red")
plot(density(data_heptathlon$longjump), main="Density Plot: longjump", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data_heptathlon$longjump), 2)))  # density plot for 'longjump'
polygon(density(data_heptathlon$longjump), col="red")


#Correlation

cor(data_heptathlon$run200m, data_heptathlon$longjump)  # calculate correlation between run200m and longjump 

#Build Linear Model
linearMod <- lm(longjump~run200m, data=data_heptathlon)  # build linear regression model on full data
print(linearMod)


#Linear Regression Diagnostics
summary(linearMod)  # model summary


#How to calculate the t Statistic and p-Values?
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["run200m", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["run200m", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(data_heptathlon)-ncol(data_heptathlon))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

#AIC and BIC

AIC(linearMod) 
BIC(linearMod) 

#Predicting Linear Models
#Step 1: Create the training (development) and test (validation) data samples from original data.

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(data_heptathlon), 0.8*nrow(data_heptathlon))  # row indices for training data
trainingData <- data_heptathlon[trainingRowIndex, ]  # model training data
testData  <- data_heptathlon[-trainingRowIndex, ]   # test data

#Step 2: Develop the model on the training data and use it to predict the distance on test data

# Build the model on training data -
lmMod <- lm(longjump~run200m, data=trainingData)  # build the model
longjumpPred <- predict(lmMod, testData)  # predict distance

#Step 3: Review diagnostic measures.
summary (lmMod)  # model summary

#Step 4: Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$longjump, predicteds=longjumpPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(min_max_accuracy)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
print((mape))

#k- Fold Cross validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=data_heptathlon, form.lm=longjump~run200m, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  
