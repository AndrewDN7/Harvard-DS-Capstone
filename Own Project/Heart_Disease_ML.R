###
### Installing packages
###

required_packages <- c(
  "readr",
  "dplyr",
  "janitor",
  "caret"
)

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))



###
### Load Data
###

data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

# Local path to save
data_path <- "data/processed_cleveland.csv"

# Create data folder if needed
if (!dir.exists("data")) dir.create("data")

# Download only if not present (reproducible, avoids repeated downloads)
if (!file.exists(data_path)) {
  download.file(data_url, destfile = data_path, mode = "wb")
}

# Load the dataset
heart_raw <- read_csv(
  data_path,
  col_names = FALSE,
  na = "?"
)

# Clean up column names
colnames(heart_raw) <- c(
  "age", "sex", "cp", "trestbps", "chol",
  "fbs", "restecg", "thalach", "exang", "oldpeak",
  "slope", "ca", "thal", "target"
)

heart <- heart_raw %>% 
  clean_names() %>% 
  mutate(target = ifelse(target > 0, 1, 0))  # Convert to binary classification

# Convert columns to factor
heart <- heart %>%
  mutate(
    sex     = factor(sex, levels = c(0,1), labels = c("female","male")),
    cp      = factor(cp),
    fbs     = factor(fbs),
    restecg = factor(restecg),
    exang   = factor(exang),
    slope   = factor(slope),
    ca      = factor(ca),
    thal    = factor(thal),
    target_numeric = target,
    target  = factor(target, levels = c(0,1), labels = c("no_disease","disease"))
  )



###
### EDA
###

# Inspect structure and basic statistics
glimpse(heart)
summary(heart)

# Consistent fill colors for heart disease status
disease_colors <- c("no_disease" = "steelblue", "disease" = "firebrick")

# Histogram of age filled by disease
ggplot(heart, aes(x = age, fill = target)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.8) +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Distribution of Age by Heart Disease",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Histogram of maximum heart rate filled by disease
ggplot(heart, aes(x = thalach, fill = target)) +
  geom_histogram(binwidth = 10, color = "white", alpha = 0.8) +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Distribution of Max Heart Rate by Heart Disease",
       x = "Max Heart Rate (thalach)",
       y = "Count") +
  theme_minimal()

# Proportion of heart disease cases by sex
ggplot(heart, aes(x = sex, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Frequency by Sex",
       x = "Sex",
       y = "Proportion") +
  theme_minimal()

# Chest pain type vs heart disease outcome
ggplot(heart, aes(x = cp, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Chest Pain Type by Heart Disease Outcome",
       x = "Chest Pain Type",
       y = "Proportion") +
  theme_minimal()

# Cholesterol levels by heart disease status
ggplot(heart, aes(x = target, y = chol, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Cholesterol Levels vs Heart Disease",
       x = "Disease",
       y = "Cholesterol") +
  theme_minimal()

# Fasting blood sugar vs heart disease
ggplot(heart, aes(x = fbs, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by Fasting Blood Sugar",
       x = "FBS (0 = Normal, 1 = High)",
       y = "Proportion") +
  theme_minimal()

# Resting ECG vs heart disease
ggplot(heart, aes(x = restecg, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by Resting ECG",
       x = "Rest ECG (0,1,2)",
       y = "Proportion") +
  theme_minimal()

# Exercise-induced angina vs heart disease
ggplot(heart, aes(x = exang, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by Exercise-Induced Angina",
       x = "Exang (0 = No, 1 = Yes)",
       y = "Proportion") +
  theme_minimal()

# Slope vs heart disease
ggplot(heart, aes(x = slope, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by ST Segment Slope",
       x = "Slope (1–3)",
       y = "Proportion") +
  theme_minimal()

# Major vessels colored by fluoroscopy vs heart disease
ggplot(heart, aes(x = ca, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by Number of Major Vessels (ca)",
       x = "CA (0–3)",
       y = "Proportion") +
  theme_minimal()

# Thalassemia vs heart disease
ggplot(heart, aes(x = thal, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = disease_colors, name = "Disease") +
  labs(title = "Heart Disease Proportion by Thalassemia Type",
       x = "Thal (3 = Normal, 6 = Fixed Defect, 7 = Reversible Defect)",
       y = "Proportion") +
  theme_minimal()

# Correlation heatmap using only numeric predictors
numeric_vars <- heart %>% select(where(is.numeric))

corr_matrix <- round(cor(numeric_vars, use = "complete.obs"), 2)

if(!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
library(reshape2)

corr_long <- melt(corr_matrix)

ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "steelblue", high = "firebrick", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap for Numeric Predictors and Outcome",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###
### Modeling
###

# LINEAR MODEL

# Keep only numeric predictors + numeric target
heart_numeric <- heart %>%
  select(where(is.numeric))  # includes target_numeric

# Create train/test split
set.seed(123)
train_index <- createDataPartition(heart_numeric$target_numeric, p = 0.8, list = FALSE)

train_data <- heart_numeric[train_index, ]
test_data  <- heart_numeric[-train_index, ]

# Train a simple linear regression model using caret
lm_model <- train(
  target_numeric ~ .,
  data   = train_data,
  method = "lm"
)

# Print model summary
print(lm_model)

# Predict numeric probabilities on the test set
lm_preds <- predict(lm_model, newdata = test_data)

# Convert numeric predictions to class labels with threshold 0.5
lm_class_preds <- ifelse(lm_preds > 0.5, "disease", "no_disease")

# True labels from target_numeric
test_labels <- ifelse(test_data$target_numeric == 1, "disease", "no_disease")

# Confusion matrix and basic accuracy metrics
cm <- confusionMatrix(
  factor(lm_class_preds, levels = c("no_disease", "disease")),
  factor(test_labels,      levels = c("no_disease", "disease"))
)

print(cm)


## LOGISTIC REGRESSION

# Train logistic regression model using caret
glm_model <- train(
  target_numeric ~ .,
  data = train_data,
  method = "glm",
  family = binomial,
  trControl = trainControl(method = "none")  # no CV yet, simple baseline
)

# Print model summary
print(glm_model)

# Predict probabilities on the test set
glm_probs <- predict(glm_model, newdata = test_data)

# caret's glm doesn't always return prob unless outcome is factor,
# so compute probabilities manually if needed
glm_probs_numeric <- predict(glm_model, newdata = test_data, type = "raw")

# Convert probabilities to class labels using threshold 0.5
glm_class_preds <- ifelse(glm_probs_numeric > 0.5, "disease", "no_disease")

# True labels from target_numeric
test_labels <- ifelse(test_data$target_numeric == 1, "disease", "no_disease")

# Confusion matrix and accuracy metrics
glm_cm <- confusionMatrix(
  factor(glm_class_preds, levels = c("no_disease", "disease")),
  factor(test_labels,      levels = c("no_disease", "disease"))
)

print(glm_cm)


## DESCISION TREES
if(!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if(!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Make sure target is a factor with levels "no_disease", "disease"
heart$target <- factor(heart$target, levels = c("no_disease", "disease"))

# Remove numeric target from predictors for this model
heart_tree <- heart %>% select(-target_numeric)

# Remove rows with missing values
heart_clean <- heart_tree %>% 
  filter(complete.cases(.))

# Train/test split based on factor target
set.seed(123)
tree_index <- createDataPartition(heart_clean$target, p = 0.8, list = FALSE)

tree_train <- heart_clean[tree_index, ]
tree_test  <- heart_clean[-tree_index, ]

# Set up cross-validation for tuning the tree
tree_ctrl <- trainControl(
  method = "cv",
  number = 10
)

# Train a decision tree model using caret (rpart) with tuning
set.seed(123)
tree_model <- train(
  target ~ .,
  data = tree_train,
  method = "rpart",
  trControl = tree_ctrl,
  tuneLength = 10  # try several cp values
)

print(tree_model)

# Optional: visualize the final tree
rpart.plot(tree_model$finalModel)

# Predict class labels on the test set
tree_preds <- predict(tree_model, newdata = tree_test)

# Confusion matrix and basic metrics (treat "disease" as positive class)
tree_cm <- confusionMatrix(
  tree_preds,
  tree_test$target,
  positive = "disease"
)

print(tree_cm)


## LOGISTIC REGRESSION — Penalized (glmnet) with CV
## Includes explicit install-if-needed checks for each package

# Check and install packages
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet", repos = "https://cloud.r-project.org")
}

library(glmnet)

# Prepare dataset — keep factor target, drop numeric target
heart_glm <- heart %>%
  select(-target_numeric) %>%
  filter(complete.cases(.))

# Train/test split (80/20)
set.seed(123)
glm_index <- createDataPartition(heart_glm$target, p = 0.8, list = FALSE)

glm_train <- heart_glm[glm_index, ]
glm_test  <- heart_glm[-glm_index, ]

# Caret train control for 10-fold CV and ROC optimization
glm_ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Train penalized logistic regression (glmnet)
set.seed(123)
glmnet_model <- train(
  target ~ .,
  data = glm_train,
  method = "glmnet",
  trControl = glm_ctrl,
  metric = "ROC",
  tuneLength = 10
)

print(glmnet_model)
plot(glmnet_model)

# Predict probabilities on test set
glmnet_probs <- predict(glmnet_model, glm_test, type = "prob")[, "disease"]

# Convert probs to classes with 0.5 threshold
glmnet_class <- ifelse(glmnet_probs > 0.5, "disease", "no_disease")

# True labels
true_labels <- glm_test$target

# Confusion matrix
glmnet_cm <- confusionMatrix(
  factor(glmnet_class, levels = c("no_disease", "disease")),
  factor(true_labels,   levels = c("no_disease", "disease"))
)

print(glmnet_cm)

# Extract coefficients of the final model at best lambda
best_lambda <- glmnet_model$bestTune$lambda
coef(glmnet_model$finalModel, s = best_lambda)



## TUNNING PROB CUTOFF

# Get predicted probabilities on the training set
train_probs  <- predict(glmnet_model, glm_train, type = "prob")[, "disease"]
train_labels <- glm_train$target

# Grid of cutoffs to test
cutoffs <- seq(0.01, 0.99, by = 0.01)

# Data frame to store training performance for each cutoff
results <- data.frame(
  cutoff = cutoffs,
  accuracy = NA,
  sensitivity = NA,
  specificity = NA,
  balanced_accuracy = NA
)

# Loop over cutoffs and compute metrics on the training set
for (i in seq_along(cutoffs)) {
  c <- cutoffs[i]
  preds <- ifelse(train_probs > c, "disease", "no_disease")
  cm <- confusionMatrix(
    factor(preds, levels = c("no_disease", "disease")),
    factor(train_labels, levels = c("no_disease", "disease"))
  )
  results$accuracy[i]          <- cm$overall["Accuracy"]
  results$sensitivity[i]       <- cm$byClass["Sensitivity"]
  results$specificity[i]       <- cm$byClass["Specificity"]
  results$balanced_accuracy[i] <- cm$byClass["Balanced Accuracy"]
}

# Pick cutoff that maximizes balanced accuracy on the training set
best_row <- results[which.max(results$balanced_accuracy), ]
best_cutoff <- best_row$cutoff
print(best_row)

# Now evaluate once on the test set using this cutoff (no leakage)
test_probs  <- predict(glmnet_model, glm_test, type = "prob")[, "disease"]
test_labels <- glm_test$target

test_preds <- ifelse(test_probs > best_cutoff, "disease", "no_disease")

final_cm <- confusionMatrix(
  factor(test_preds, levels = c("no_disease", "disease")),
  factor(test_labels, levels = c("no_disease", "disease"))
)

print(final_cm)


# Load knitr for kable
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)

# Extract metrics from each confusion matrix
model_metrics <- data.frame(
  Model = c("Linear Regression", 
            "Logistic Regression (GLM)",
            "Decision Tree (rpart)",
            "Penalized Logistic Regression (glmnet)"),
  
  Accuracy = c(
    cm$overall["Accuracy"],
    glm_cm$overall["Accuracy"],
    tree_cm$overall["Accuracy"],
    glmnet_cm$overall["Accuracy"]
  ),
  
  Sensitivity = c(
    cm$byClass["Sensitivity"],
    glm_cm$byClass["Sensitivity"],
    tree_cm$byClass["Sensitivity"],
    glmnet_cm$byClass["Sensitivity"]
  ),
  
  Specificity = c(
    cm$byClass["Specificity"],
    glm_cm$byClass["Specificity"],
    tree_cm$byClass["Specificity"],
    glmnet_cm$byClass["Specificity"]
  ),
  
  Balanced_Accuracy = c(
    cm$byClass["Balanced Accuracy"],
    glm_cm$byClass["Balanced Accuracy"],
    tree_cm$byClass["Balanced Accuracy"],
    glmnet_cm$byClass["Balanced Accuracy"]
  )
)

# Print nicely with kable
kable(
  model_metrics,
  digits = 3,
  caption = "Comparison of Model Performance Metrics"
)

