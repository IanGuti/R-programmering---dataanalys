
# Load libraries 
library("readxl")
library(leaps)
library(ggplot2)
library(caret)
library(tidyverse)
library(tibble)
library(car)
library(corrplot)


# Load the excel file
file_path <- "C:/Users/ian_g/Desktop/Plugg/R Programmering/kunskapskontroll/Bilar.xlsx"
car_data <- read_excel(file_path)

# Inspect data
View(car_data)
names(car_data)
dim(car_data)
class(car_data)

# Inspecting classes of columns
column_classes <- sapply(car_data, class)
print(column_classes)               

# Transform

car_data$Bränsle <- as.factor(car_data$Bränsle)
car_data$Växellåda <- as.factor(car_data$Växellåda)
car_data$Biltyp <- as.factor(car_data$Biltyp)
car_data$Drivning <- as.factor(car_data$Drivning)
car_data$Märke <- as.factor(car_data$Märke)

column_classes <- sapply(car_data, class)
print(column_classes)   

# Remove duplicates

duplicates <- duplicated(car_data)
car_data[duplicates, ]
car_data_unique <- car_data[!duplicated(car_data), ]

# Check how many rows there were before and after

cat("Antal rader före borttagning:", nrow(car_data), "\n")
cat("Antal rader efter borttagning:", nrow(car_data_unique), "\n")

# Remove those variables I don't wish to have, using logic

columns_to_remove <- c("Drivning")
indicies_to_remove <- which(colnames(car_data_unique) %in% columns_to_remove)
car_data_filter_1 <- car_data_unique[, -indicies_to_remove]

# Create dummy variables and turn car_data_filter_1 into a dataframe

car_data_filter_1 <- model.matrix(~ . + 0, data = car_data_filter_1)
car_data_filter_1 <- as_tibble(car_data_filter_1)
class(car_data_filter_1)

# Look for the best parameters

regfit <- regsubsets(Pris ~ . , data = car_data_filter_1, nvmax = 19, method = "forward")
regfit.summary <- summary(regfit)
names(regfit.summary)

par(mfrow = c(2, 2))

plot(regfit.summary$cp, xlab = "Number of variables", ylab = "Cp")
which.min(regfit.summary$cp)
points(15, regfit.summary$cp[16], pch = 20, col = "red")

plot(regfit.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2")
which.max(regfit.summary$adjr2)
points(15, regfit.summary$adjr2[17], pch = 20, col = "red")

plot(regfit.summary$bic, xlab = "Number of variables", ylab = "BIC")
which.min(regfit.summary$bic)
points(14, regfit.summary$bic[15], pch = 20, col = "red")

par(mfrow = c(2, 2))
plot(regfit, scale = "Cp")
plot(regfit, scale = "adjr2")
plot(regfit, scale = "bic")

coef(regfit, 17)

par(mfrow = c(1, 1))

# Remove unnecessary variables

columns_to_remove <- c("Bränslemiljöbränsle/hybrid", "Märkevolkswagen", "Växellådamanuell", "Bränslebensin")
indicies_to_remove <- which(colnames(car_data_filter_1) %in% columns_to_remove)
car_data_filter_2 <- car_data_filter_1[, -indicies_to_remove]

# Create traing and test data
dim(car_data_filter_2)
set.seed(123)
2378 * 0.7

train_samples <- sample(seq(2378), 1665, replace = FALSE)
train_set <- car_data_filter_2[train_samples, ]
test_set <- car_data_filter_2[-train_samples, ]

regfit.subset <- regsubsets(Pris ~ . , data = car_data_filter_2[train_samples,], nvmax = 18)

# Check RMSE for training and test data

val.error <- rep(NA, 14)
x_test <- model.matrix(Pris ~ . , data = car_data_filter_2[-train_samples,])
for(i in 1:14){
  coefi = coef(regfit.subset, id = i)
  pred = x_test[, names(coefi)]%*%coefi
  val.error[i] <- mean((car_data_filter_2$Pris[-train_samples]-pred)^2)
}
plot(sqrt(val.error), ylab = "RMSE", ylim = c(35000, 155000), pch = 18, type = "b")
points(sqrt(regfit.subset$rss[-1]/603), col = "blue", pch = 18, type = "b")
legend("topright", legend = c("Training", "Test"), col = c("blue", "black"), pch = 18)

# See how a linear model with standard values preforms

lin_mod <- lm(Pris ~ ., data = car_data_filter_2)
predictor_1 <- predict(lin_mod, test_set)

modell_summary <- summary(lin_mod)
adjusted_R2 <- modell_summary$adj.r.squared
bic_val <- BIC(lin_mod)

data.frame( Adjusted_R2 = adjusted_R2,
            BIC = bic_val,
            RMSE = RMSE(predictor_1, test_set$Pris),
            MAE = MAE(predictor_1, test_set$Pris))

print(lin_mod)
summary(lin_mod)

# Find if there is a non-linear relationship between the independent variables 
# and the dependent variable

ggplot(car_data_filter_2, aes(x = Miltal, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Miltal", y = "Pris") +
  ggtitle("Scatterplot of miltal vs. pris")

ggplot(car_data_filter_2, aes(x = Modellår, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Modellår", y = "Pris") +
  ggtitle("Scatterplot of modellår vs. pris")

ggplot(car_data_filter_2, aes(x = Hästkrafter, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Hästkrafter", y = "Pris") +
  ggtitle("Scatterplot of hästkrafter vs. pris")

# Check if the data is correlated

par(mfrow = c(2, 2))
plot(lin_mod)
view(car_data_filter_2)

# Cook's distance anomalies

leverage <- hatvalues(lin_mod)
indices_within_range <- which(leverage >= 0.05 & leverage)
indices_within_range_2 <- which(leverage >= 0.03 & leverage <= 0.05)
data_within_range <- car_data_filter_2[indices_within_range, ]
data_within_range_2 <- car_data_filter_2[indices_within_range_2, ]
view(data_within_range)
view(data_within_range_2)

# Check if the data is Heteroskedastic

residual <- residuals(lin_mod)
par(mfrow = c(1, 1))
plot(fitted(lin_mod), residual, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")

# Check if the residuals are normal distributed

hist(residual, col = 4, breaks = 15)

# Remove outliers

outlier_remover <- c(9, 31, 99)
car_data_filter_2 <- car_data_filter_2[-outlier_remover, ]

# Check for multicollinear

vif_value <- vif(lin_mod)
print(vif_value)

# Remove variables

columns_to_remove <- c("Biltyphalvkombi", "Biltypkombi", "Biltypsedan", "Biltypsuv")
indicies_to_remove <- which(colnames(car_data_filter_2) %in% columns_to_remove)
car_data_filter_2 <- car_data_filter_2[, -indicies_to_remove]

# Train a new model 
dim(car_data_filter_2)
set.seed(123)
2375 * 0.7

train_samples <- sample(seq(2375), 1662, replace = FALSE)
train_set <- car_data_filter_2[train_samples, ]
test_set <- car_data_filter_2[-train_samples, ]

lin_mod_2 <- lm(Pris ~ ., data = car_data_filter_2)
predictor_2 <- predict(lin_mod_2, test_set)
predictor_train <- predict(lin_mod_2, train_set)

# Evaluate the model

modell_summary <- summary(lin_mod_2)
adjusted_R2 <- modell_summary$adj.r.squared
bic_val <- BIC(lin_mod_2)

data.frame( Adjusted_R2 = adjusted_R2,
            BIC = bic_val,
            RMSE = RMSE(predictor_2, test_set$Pris),
            MAE = MAE(predictor_2, test_set$Pris))

result <- summary(lin_mod_2)
coeff <- abs(result$coefficients[, 1])
sorted_index <- order(coeff, decreasing = TRUE)
top_3_variables <- rownames(result$coeff)[-1][sorted_index[1:3]]
print(top_3_variables)

coef(lin_mod_2)
par(mfrow = c(2, 2))
plot(lin_mod_2)

# Look at the confidence interval and prediction interval

confidence_intervals <- predict(lin_mod_2, newdata = test_set, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lin_mod_2, newdata = test_set, interval = "prediction", level = 0.95)

confint(lin_mod_2)

view(confidence_intervals)
prediction_intervals

pred_intervals <- matrix(c(prediction_intervals[,2], prediction_intervals[,3]), ncol = 2, byrow = FALSE)
mean(pred_intervals)

# Check if data is non linear

ggplot(car_data_filter_2, aes(x = Miltal, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Miltal", y = "Pris") +
  ggtitle("Scatterplot of miltal vs. pris")

ggplot(car_data_filter_2, aes(x = Modellår, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Modellår", y = "Pris") +
  ggtitle("Scatterplot of modellår vs. pris")

ggplot(car_data_filter_2, aes(x = Hästkrafter, y = Pris)) +
  geom_point(color = "blue") +
  labs(x = "Hästkrafter", y = "Pris") +
  ggtitle("Scatterplot of hästkrafter vs. pris")

# Check if new model is correlated or heteroscedastic

par(mfrow = c(2, 2))
plot(lin_mod_2)

# Look at normaldistribution

par(mfrow = c(1, 1))
residuals <- residuals(lin_mod_2)
hist(residual, col = 4, breaks = 15)

# Check if my data in multicollinear
vif_value <- vif(lin_mod_2)
print(vif_value)
