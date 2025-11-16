
################################## Libraries ###################################

library(lubridate)
library(caret)
library(dplyr)
library(MLmetrics)
library(pROC)
library(ggplot2)
library(ROSE)
library(themis)
library(rpart)
library(rpart.plot)
library(randomForest)
library(car)
library(glmnet)
library(e1071)
library(doParallel)
library(tidyr)
library(xtable)
library(vcd)

################################### Data #######################################

# Load the data
data <- read.csv("race_results_final.csv")

data$race_date <- as.Date(data$Date, format = "%d/%m/%Y")  # Convert race date
data$birth_date <- as.Date(data$BornDate, format = "%d-%m-%Y")  # Convert birth date

# Calculate age in months
data$age_in_months <- interval(data$birth_date, data$race_date) %/% months(1)

# Convert Hour of race to numeric (hour of the day)
data$Hour_numeric <- as.numeric(format(as.POSIXct(data$Hour, format = "%H:%M"), "%H")) * 60 +
  as.numeric(format(as.POSIXct(data$Hour, format = "%H:%M"), "%M"))

# Define position_1 =1 if horse won, 0 otherwise
data$position_1 <- ifelse(data$Position == 1, 1, 0)

# Define month of the race
data$Month <- format(data$race_date, "%m")
data$Day <- format(data$race_date, "%j")
data$Day <- as.numeric(data$Day)

# Filter out erroneous data (e.g., JockeyWeight < 40 or HorseWeight < 100)
data <- data %>% filter(JockeyWeight >= 40, HorseWeight >= 100)

# Recode Jockeys with fewer than 100 races as "Other"
jockey_counts <- data %>%
  group_by(Jockey) %>%
  summarize(race_count = n())  # Count the number of races for each jockey

# Merge the count data with the main data
data <- data %>%
  left_join(jockey_counts, by = "Jockey") %>%
  mutate(Jockey = ifelse(race_count < 100, "Other", Jockey)) %>%
  select(-race_count)  # Remove the temporary race_count column

# Recode Breeder with fewer than 50 races as "Other"
Breeder_counts <- data %>%
  group_by(Breeder) %>%
  summarize(race_count = n())  # Count the number of races for each breeder

# Merge the count data with the main data
data <- data %>%
  left_join(Breeder_counts, by = "Breeder") %>%
  mutate(Breeder = ifelse(race_count < 50, "Other", Breeder)) %>%
  select(-race_count)  # Remove the temporary race_count column

# Recode Father with fewer than 100 races as "Other"
Father_counts <- data %>%
  group_by(Father) %>%
  summarize(race_count = n())  # Count the number of races for each breeder

# Merge the count data with the main data
data <- data %>%
  left_join(Father_counts, by = "Father") %>%
  mutate(Father = ifelse(race_count < 100, "Other", Father)) %>%
  select(-race_count)  # Remove the temporary race_count column

# Recode Grandfather with fewer than 100 races as "Other"
Grandfather_counts <- data %>%
  group_by(Grandfather) %>%
  summarize(race_count = n())  # Count the number of races for each breeder

# Merge the count data with the main data
data <- data %>%
  left_join(Grandfather_counts, by = "Grandfather") %>%
  mutate(Grandfather = ifelse(race_count < 100, "Other", Grandfather)) %>%
  select(-race_count)  # Remove the temporary race_count column

# Recode Cuidador with fewer than 50 races as "Other"
Cuidador_counts <- data %>%
  group_by(Cuidador) %>%
  summarize(race_count = n())  # Count the number of races for each breeder

# Merge the count data with the main data
data <- data %>%
  left_join(Cuidador_counts, by = "Cuidador") %>%
  mutate(Cuidador = ifelse(race_count < 50, "Other", Cuidador)) %>%
  select(-race_count)  # Remove the temporary race_count column

# Drop unnecessary variables
data <- data %>% select(-Date, -Hour, -BornDate, -Horse, -Caballeriza,
                        -Mother, -race_date,
                        -birth_date, -Position)

# Convert to factor and num
data <- data %>%
  mutate_if(is.character, as.factor)
data$position_1 <- factor(data$position_1, levels = c(0, 1))
data <- data %>%
  mutate_if(is.integer, as.numeric)

# Normalize numeric variables
numeric_vars <- c("Distance", "HorseNumber", "HorseWeight", "JockeyWeight", "age_in_months",
                  "Hour_numeric", "Day") # Sin Day
preprocess_params <- preProcess(data[, numeric_vars], method = c("center", "scale"))  # Center and scale
data_normalized <- predict(preprocess_params, data[, numeric_vars])  # Apply normalization to the data

# Replace the original numeric columns with the normalized ones
data[numeric_vars] <- data_normalized

# Extract the numeric variables
numeric_data <- data[, numeric_vars]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Convert the correlation matrix to an xtable object
correlation_xtable <- xtable(correlation_matrix, caption = "Correlation Matrix of Numeric Variables")

# Print the LaTeX code to the console
print(correlation_xtable, type = "latex", file = "correlation_matrix.tex", include.rownames = TRUE)

# Convert 'position_1' to numeric
position_1_numeric <- as.numeric(data$position_1)

# Compute correlations between 'position_1' (now numeric) and each column in 'numeric_data'
correlations <- sapply(numeric_data, function(column) cor(position_1_numeric, column, use = "complete.obs"))

# Print the correlations
print(correlations)

# Identify categorical variables
categorical_vars <- names(data)[sapply(data, is.factor)]

# Create a function to calculate Cramér's V for two variables
calculate_cramers_v <- function(var1, var2) {
  tbl <- table(var1, var2)  # Create a contingency table
  cramers_v <- assocstats(tbl)$cramer  # Calculate Cramér's V
  return(cramers_v)
}

# Calculate pairwise Cramér's V for all categorical variables
cramers_v_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                           dimnames = list(categorical_vars, categorical_vars))

for (i in seq_along(categorical_vars)) {
  for (j in seq_along(categorical_vars)) {
    if (i != j) {
      cramers_v_matrix[i, j] <- calculate_cramers_v(data[[categorical_vars[i]]], data[[categorical_vars[j]]])
    }
  }
}

# Print the Cramér's V matrix
print(cramers_v_matrix)

# Convert the Cramér's V matrix to a data frame for better formatting
cramers_v_df <- as.data.frame(cramers_v_matrix)

# Print the table in LaTeX format
latex_table <- xtable(cramers_v_df, caption = "Cramér's V Table for Categorical Variables")
print(latex_table, type = "latex", include.rownames = TRUE, include.colnames = TRUE)


############################### One-hot encoding ###############################

# Create a one-hot encoding model
dummy_model <- dummyVars("~ .", data = data[, -which(names(data) == "position_1")])

# Apply the transformation to the dataset
data_encoded <- as.data.frame(predict(dummy_model, newdata = data))
data_encoded$position_1 <- data$position_1
data_encoded <- data_encoded[, !grepl("Jockey.Other", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Breeder.Other", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Father.Other", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Grandfather.Other", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Cuidador.Other", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Month.01", names(data_encoded))]
data_encoded <- data_encoded[, !grepl("Sex.HEMBRA", names(data_encoded))]
data_encoded <- data_encoded[, names(data_encoded) != "PistaCondition.ARENA | NORMAL"]
data_encoded <- data_encoded[, !grepl("Pelaje.ALAZAN TOSTADO", names(data_encoded))]
train_control <- trainControl(method = "cv", number = 10, sampling = "up")
data$race_id <- paste(data$Day, data$Hour_numeric, sep = "_")

# Create a partition based on the unique race identifier
set.seed(123)  # Set seed for reproducibility
unique_races <- unique(data$race_id)
train_races <- sample(unique_races, size = 0.8 * length(unique_races))
train_data <- data_encoded[data$race_id %in% train_races, ]
test_data <- data_encoded[!data$race_id %in% train_races, ]

# Rename levels of the target variable
train_data$position_1 <- factor(train_data$position_1, levels = c("0", "1"), labels = c("Class0", "Class1"))
test_data$position_1 <- factor(test_data$position_1, levels = c("0", "1"), labels = c("Class0", "Class1"))

f1_metric <- function(data, lev = NULL, model = NULL) {
  print("Observed values:")
  print(data$obs)
  print("Predicted values:")
  print(data$pred)
  f1 <- F1_Score(y_true = data$obs, y_pred = data$pred, positive = "Class1")
  c(F1 = f1)
}

train_control <- trainControl(
  method = "cv",                     # Cross-validation
  number = 5,                        # Number of folds
  classProbs = TRUE,                 # Needed for probability-based predictions
  summaryFunction = f1_metric,       # Use custom F1 metric
  savePredictions = "final",          # Save predictions for final evaluation
  sampling = "up"
)


##################################### GLM ######################################

# Define the grid of alpha and lambda values
grid <- expand.grid(
  alpha = seq(0.3, 0.7, by = 0.1),      # Cross-validation for alpha
  lambda = seq(0.001, 0.01, by = 0.001)
)

# Train the model using Elastic Net with the grid search for alpha and lambda, with upsampling
elastic_net_model <- train(
  position_1 ~ .,   # The formula
  data = train_data, # Training data
  method = "glmnet", # Elastic Net method
  family = "binomial", # Logistic regression (binary outcome)
  metric = "F1",      # Optimize for F1 score 
  trControl = train_control,  # Cross-validation settings
  tuneGrid = grid       # The grid of hyperparameters to tune
)


# Predict probabilities using the trained model
predictions_prob <- predict(elastic_net_model, newdata = test_data, type = "prob")

# Add Date and Hour_numeric columns to predictions
predictions_prob$Date <- test_data$Day
predictions_prob$Hour_numeric <- test_data$Hour_numeric

# Identify the winner (highest probability per race)
predictions_prob <- predictions_prob %>%
  group_by(Date, Hour_numeric) %>%
  mutate(winner = ifelse(Class1 == max(Class1), 1, 0)) %>%
  ungroup()
predictions_prob$winner <- ifelse(predictions_prob$winner == 0, "Class0", "Class1")
predictions_prob$winner <- factor(predictions_prob$winner, levels = c("Class0", "Class1"))

# Confusion Matrix to evaluate performance
conf_matrix <- confusionMatrix(as.factor(predictions_prob$winner), as.factor(test_data$position_1))
print(conf_matrix)

# Calculate the ROC and AUC for the RF model
roc_curve <- roc(test_data$position_1, predictions_prob$Class1)
print(roc_curve)
print(paste("AUC: ", auc(roc_curve)))

# Plot the ROC curve
roc_data <- data.frame(
  fpr = rev(roc_curve$specificities),
  tpr = rev(roc_curve$sensitivities)
)
ggplot(roc_data, aes(x = 1 - fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = paste("ROC Curve (AUC = ", round(auc(roc_curve), 2), ")", sep = ""),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

best_params <- elastic_net_model$bestTune
print(best_params)


###################################### RF ######################################

cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)
clusterExport(cl, c("F1_Score"))

# Train the Random Forest model with trainControl
model_rf <- train(
  position_1 ~ .,         # Formula for model
  data = train_data,      # Training data
  method = "rf",          # Random Forest model
  trControl = train_control,  # Using the trainControl object with AUC optimization
  metric = "F1",
  tuneGrid = expand.grid(.mtry = c(2,3,4,5,6,7,8, 9, 10, 11, 12, 13, 14, 15)),  # Range for mtry hyperparameter
  ntree = 500,             # Number of trees in the Random Forest
  importance=T,   # Computar importancia de c/ covariable.
  proximity = FALSE,    #computa la matriz de proximidad entre observaciones.
)

stopCluster(cl)  # Stop the cluster after training
registerDoSEQ()  # Reset to sequential processing
print(model_rf)

# Predict probabilities using the trained model
predictions_prob_rf <- predict(model_rf, newdata = test_data, type = "prob")

# Add Date and Hour_numeric columns to predictions (just like before)
predictions_prob_rf$Date <- test_data$Day
predictions_prob_rf$Hour_numeric <- test_data$Hour_numeric

# Identify the winner (highest probability per race)
predictions_prob_rf <- predictions_prob_rf %>%
  group_by(Date, Hour_numeric) %>%
  mutate(winner = ifelse(Class1 == max(Class1), 1, 0)) %>%
  ungroup()
predictions_prob_rf$winner <- ifelse(predictions_prob_rf$winner == 0, "Class0", "Class1")
predictions_prob_rf$winner <- factor(predictions_prob_rf$winner, levels = c("Class0", "Class1"))

# Confusion Matrix to evaluate performance
conf_matrix_rf <- confusionMatrix(as.factor(predictions_prob_rf$winner), as.factor(test_data$position_1))
print(conf_matrix_rf)

# Calculate the ROC and AUC for the RF model
roc_curve_rf <- roc(test_data$position_1, predictions_prob_rf$Class1)
print(roc_curve_rf)
print(paste("AUC: ", auc(roc_curve_rf)))

# Plot the ROC curve
roc_data_rf <- data.frame(
  fpr = rev(roc_curve_rf$specificities),
  tpr = rev(roc_curve_rf$sensitivities)
)
ggplot(roc_data_rf, aes(x = 1 - fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = paste("ROC Curve (AUC = ", round(auc(roc_curve_rf), 2), ")", sep = ""),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

####################################### XGB ####################################

# Define the tuning grid
tune_grid <- expand.grid(
  n.trees = c(100, 200, 300),       # Number of trees
  interaction.depth = c(3, 4, 5),    # Depth of each tree
  shrinkage = c(0.01, 0.05, 0.1),   # Learning rate
  n.minobsinnode = c(10, 20)        # Minimum number of observations in terminal nodes
)

# Train the GBM model
model_gbm <- train(position_1 ~ ., 
                   data = train_data, 
                   method = "gbm", 
                   trControl = train_control, 
                   metric = "F1", 
                   tuneGrid = tune_grid, 
                   verbose = FALSE)

# Print the model summary
print(model_gbm)

# Predict probabilities using the trained model
predictions_prob_gbm <- predict(model_gbm, newdata = test_data, type = "prob")

# Add Date and Hour_numeric columns to predictions (just like before)
predictions_prob_gbm$Date <- test_data$Day
predictions_prob_gbm$Hour_numeric <- test_data$Hour_numeric

# Identify the winner (highest probability per race)
predictions_prob_gbm <- predictions_prob_gbm %>%
  group_by(Date, Hour_numeric) %>%
  mutate(winner = ifelse(Class1 == max(Class1), 1, 0)) %>%
  ungroup()
predictions_prob_gbm$winner <- ifelse(predictions_prob_gbm$winner == 0, "Class0", "Class1")
predictions_prob_gbm$winner <- factor(predictions_prob_gbm$winner, levels = c("Class0", "Class1"))

# Confusion Matrix to evaluate performance
conf_matrix_gbm <- confusionMatrix(as.factor(predictions_prob_gbm$winner), as.factor(test_data$position_1))
print(conf_matrix_gbm)

# Calculate the ROC and AUC for the RF model
roc_curve_gbm <- roc(test_data$position_1, predictions_prob_gbm$Class1)
print(roc_curve_gbm)
print(paste("AUC: ", auc(roc_curve_gbm)))

# Plot the ROC curve
roc_data_gbm <- data.frame(
  fpr = rev(roc_curve_gbm$specificities),
  tpr = rev(roc_curve_gbm$sensitivities)
)
ggplot(roc_data_gbm, aes(x = 1 - fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = paste("ROC Curve (AUC = ", round(auc(roc_curve_gbm), 2), ")", sep = ""),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

####################################### NN #####################################

# Grid for tuning parameters
grid_nn <- expand.grid(
  size = c(2, 3, 4),               # Number of hidden neurons
  decay = c(0.1, 0.01)        # Weight decay (regularization)
)

# Train the neural network
model_nn <- train(
  position_1 ~ .,                    # Target variable
  data = train_data,                 # Training data
  method = "nnet",                   # Neural network method
  metric = "F1",                     # Optimize for F1 score
  trControl = train_control,      # Cross-validation settings
  tuneGrid = grid_nn,                # Grid of hyperparameters
  linout = FALSE,                     # Classification (not regression)
)

# Predict probabilities on the test data
predictions_prob_nn <- predict(model_nn, newdata = test_data, type = "prob")

# Print the model summary
print(model_nn)

# Do it again with PCA
grid_nn2 <- expand.grid(
  size = c(2:15),          # Reduce number of neurons
  decay = c(0.1, 0.01, 0.001)     # Weight decay
)

# Use PCA for dimensionality reduction
pca <- prcomp(train_data[, -ncol(train_data)], scale. = TRUE)
train_data_pca <- data.frame(pca$x[, 1:50], position_1 = train_data$position_1)

# Train the model with reduced features
model_nn2 <- train(
  position_1 ~ ., 
  data = train_data_pca, 
  method = "nnet", 
  metric = "F1", 
  trControl = train_control, 
  tuneGrid = grid_nn2, 
  linout = FALSE
)

print(model_nn2)

# Apply the same PCA transformation to the test data
test_data_pca <- data.frame(predict(pca, newdata = test_data[, -ncol(test_data)]), position_1 = test_data$position_1)
test_data_pca <- test_data_pca[, 1:50]  # Select the same number of components used for training

# Predict probabilities on the test data
predictions_prob_nn <- predict(model_nn2, newdata = test_data_pca, type = "prob")

# Add Date and Hour_numeric columns to predictions
predictions_prob_nn$Date <- test_data$Day
predictions_prob_nn$Hour_numeric <- test_data$Hour_numeric

# Identify the winner (highest probability per race) with explicit tie-breaking
predictions_prob_nn <- predictions_prob_nn %>%
  group_by(Date, Hour_numeric) %>%
  mutate(
    rank = rank(-Class1, ties.method = "random")  # Rank probabilities (randomly break ties)
  ) %>%
  ungroup() %>%
  mutate(
    winner_flag = ifelse(rank == 1, 1, 0)  # Assign the winner flag to the top-ranked horse
  )

# Assign the winner label
predictions_prob_nn$winner <- ifelse(predictions_prob_nn$winner_flag == 1, "Class1", "Class0")

# Confusion Matrix to evaluate performance
conf_matrix_nn <- confusionMatrix(as.factor(predictions_prob_nn$winner), as.factor(test_data$position_1))
print(conf_matrix_nn)

# Calculate the ROC and AUC for the RF model
roc_curve_nn <- roc(test_data$position_1, predictions_prob_nn$Class1)
print(roc_curve_nn)
print(paste("AUC: ", auc(roc_curve_nn)))
# Plot the ROC curve
roc_data_nn <- data.frame(
  fpr = rev(roc_curve_nn$specificities),
  tpr = rev(roc_curve_nn$sensitivities)
)
ggplot(roc_data_nn, aes(x = 1 - fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = paste("ROC Curve (AUC = ", round(auc(roc_curve_nn), 2), ")", sep = ""),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

