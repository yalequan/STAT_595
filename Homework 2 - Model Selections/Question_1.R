# Load Packages

library("ggplot2")
library("GGally")
library("olsrr")
library("MASS")

# Setup data

set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3 * x2 + rnorm(100)

# Find Correlation Between X1 and X2

question_1 = data.frame("x1" = c(x1), "x2" = c(x2), "y" = c(y))

ggcorr(question_1, palette = "RdBu", label = TRUE, label_size = 3, label_round = 2, hjust = 1)
cor(question_1)
ggpairs(question_1, title = "Correlation Matrix for Question 1")

# Fit regression model to predict y using x1 and x2

model_1 = lm(y ~ x1 + x2, data = question_1)
summary(model_1)

# Fit regression model to predict y using x1

model_2 = lm(y ~ x1, data = question_1)
summary(model_2)

# Fit regression model to predict y using x2

model_3 = lm(y ~ x2, data = question_1)
summary(model_3)

# Add new observations

x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)

question_1 = data.frame("x1" = c(x1), "x2" = c(x2), "y" = c(y))

# Fit new regression model to predict y using x1 and x2

model_4 = lm(y ~ x1 + x2, data = question_1)
summary(model_4)

# Check for outliers

ols_plot_cooksd_bar(model_4)

leverage_4 = (hatvalues(model_4))

ggplot(data = NULL, aes(x = seq(1, length(leverage_4)), y = leverage_4)) + geom_point() + labs(title="Leverage Plot for y ~ x1 + x2") + xlab("Index") + ylab("Leverage")

sresid_4 = studres(model_4)

plot(sresid_4, ylab = "Studentized Residuals", main = ("Plot of Studentized Residuals for y ~ x1 + x2"))


# Fit new regression model to predict y using x1

model_5 = lm(y ~ x1, data = question_1)
summary(model_5)

# Check for outliers

ols_plot_cooksd_bar(model_5)

leverage_5 = (hatvalues(model_5))

ggplot(data = NULL, aes(x = seq(1, length(leverage_5)), y = leverage_5)) + geom_point() + labs(title="Leverage Plot for y ~ x1") + xlab("Index") + ylab("Leverage")

sresid_5 = studres(model_5)

plot(sresid_5, ylab = "Studentized Residuals", main = ("Plot of Studentized Residuals for y ~ x1"))

# Fit new regression model to predict y using x2

model_6 = lm(y ~ x2, data = question_1)
summary(model_6)

# Check for outliers

ols_plot_cooksd_bar(model_6)

leverage_6 = (hatvalues(model_6))

ggplot(data = NULL, aes(x = seq(1, length(leverage_6)), y = leverage_6)) + geom_point() + labs(title="Leverage Plot for y ~ x2") + xlab("Index") + ylab("Leverage")

sresid_6 = studres(model_6)

plot(sresid_6, ylab = "Studentized Residuals",main = ("Plot of Studentized Residuals for y ~ x2"))
