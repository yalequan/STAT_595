# Import Packages

library("MASS")
library("leaps")
library("dplyr")
library("fastDummies")
library("ggplot2")
library("GGally")

# Check integrity of the data and its structure

head(GASTURBINE)

summary(GASTURBINE)

sum(is.na(GASTURBINE))

str(GASTURBINE)

# Change Engine to Factor

GASTURBINE$ENGINE = as.factor(GASTURBINE$ENGINE)

str(GASTURBINE)

# Create Dummy variables

GASTURBINE_2 = dummy_cols(GASTURBINE, select_columns = c("ENGINE"))
str(GASTURBINE_2)
View(GASTURBINE_2)

# Clean Data. Remove one of the dummy variables and the ENGINE variable

GASTURBINE_2 = GASTURBINE_2 %>% select(c(HEATRATE, everything()))

GASTURBINE_2 = GASTURBINE_2 %>% select(-c(ENGINE, ENGINE_Aeroderiv))
str(GASTURBINE_2)

# Setup full model

full_model = lm(GASTURBINE_2$HEATRATE ~ ., data = GASTURBINE_2)
summary(full_model)

n = nrow(GASTURBINE_2)

# Stepwise Selection

Stepwise_model = stepAIC(full_model, direction = "both", trace = TRUE)
summary(Stepwise_model)

# Backward Selction

Backward_Model_AIC = stepAIC(full_model, direction = "backward", trace = FALSE)
summary(Backward_Model_AIC)

Backward_Model_BIC = stepAIC(full_model, direction = "backward", trace = FALSE, k = log(n))
summary(Backward_Model_BIC)

# All Possible Subsets

mycp = leaps(x = GASTURBINE_2[,2:10], y = GASTURBINE_2$HEATRATE, names = names(GASTURBINE_2)[2:10], method = "Cp")
myr2 = leaps(x = GASTURBINE_2[,2:10], y = GASTURBINE_2$HEATRATE, names = names(GASTURBINE_2)[2:10], method = "r2")
myadjr2 = leaps(x = GASTURBINE_2[,2:10], y = GASTURBINE_2$HEATRATE, names = names(GASTURBINE_2)[2:10], method = "adjr2")

# Select best CP, R^2 and AdjR^2
best_cp = mycp$which[which((mycp$Cp == min(mycp$Cp))),]
best_adjr2 = myadjr2$which[which((myadjr2$adjr2 == max(myadjr2$adjr2))),]
best_r2 = myr2$which[which((myr2$r2 == max(myr2$r2))),]

# Create dataframe of model
options(warn = -1)
model_1 = data.frame(best_cp, best_r2, best_adjr2)

# View Model
model_1

