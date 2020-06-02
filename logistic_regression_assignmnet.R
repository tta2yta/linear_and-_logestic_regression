install.packages("rms")
library(foreign)       
library(ggplot2) # USed for plotting data
library(dplyr) # Used to extract columns in the data
library(rms) # Used to extract p-value from logistic model


#Load dataset
data_auto<- read.dta("http://statistics.ats.ucla.edu/stat/data/auto.dta")
head(data_auto)

# Limit the dataset to the two columns of interest
df <- select(data_auto, foreign, price )

#Compute summary statistics by groups:
library(dplyr)
group_by(df, foreign) %>%
  summarise(
    count = n(),
    mean = mean(price, na.rm = TRUE),
    sd = sd(price, na.rm = TRUE)
  )

#Visualize your data using box plots-----t-test statistic
library("ggpubr")
ggboxplot(df, x = "foreign", y = "price", 
          color = "foreign", palette = c("#00AFBB", "#E7B800"),
          ylab = "price", xlab = "foreign")

#The logistic regression model
M1 <- glm(foreign ~ price, df, family = binomial)
M1

# Create a range of price values (we'll cover a wider range then the dataset)
# The range of values must be saved in a data frame and must have the same column
# name as that given in the original dataset
M.df<- data.frame(price = seq(-50000, 100000, 1000))

#Predict the foreign values (as a probability) using the above data
M.df$foreign <- predict(M1, newdata=M.df, type="response")

# Plot the modeled probability values
ggplot(M.df, aes(x=price, y=foreign)) + geom_line()


# model with the use of ggplot's stat_smooth function
ggplot(df, aes(x=price, y=as.numeric(df$foreign) - 1)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  ylab("foreign") + xlim(1000, 20000)

#Assessing the fit with a pseudo R2
M1$null.deviance
M1$deviance

#The difference between both log-likelihood values is referred to as the model Chi-square.
modelChi <- M1$null.deviance - M1$deviance
modelChi

#pseudo R-square
pseudo.R2 <- modelChi / M1$null.deviance
pseudo.R2

#Alternative pseudo R2
lrm(foreign ~ price, df)

#likelihood ratio p-value.
Chidf <- M1$df.null - M1$df.residual
chisq.prob <- 1 - pchisq(modelChi, Chidf)
chisq.prob

#Parameter significance
summary(M1)

#Multi-variable model
# Grab variables of interest
df2 <- select(data_auto, foreign, price, weight)

# Run regression model
M2 <- glm(foreign ~ price + weight, df2, family = binomial)

# Compute pseudo R-square
modelChi <- M2$null.deviance - M2$deviance
pseudo.R2 <- modelChi / M2$null.deviance
pseudo.R2


# Compute the pseudo p-value
Chidf <- M2$df.null - M2$df.residual
modelChi <- M2$null.deviance - M2$deviance
1 - pchisq(modelChi, Chidf)

# Assess each parameter's significance
summary(M2)
