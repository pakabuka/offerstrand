# Load necessary libraries
library(caret)
library(pROC)
library(stargazer)


# Source file path
source = "/Users/frederickliu/Desktop/Offerstrand/Research/Law School Admissions Predictor/lsdata.csv"
comp = read.csv(source)

# Using data only from 2018 to 2023
data <- subset(comp, cycle_id >= 19 & cycle_id <= 23 & (simple_status == "accepted"
                                                        | simple_status == "Rejected"
                                                        | simple_status == "Accepted"
                                                        | simple_status == "Accepted, Withdrawn"
                                                        | simple_status == "Hold, Accepted, Withdrawn"
                                                        | simple_status == "WL, Accepted"
                                                        | simple_status == "Hold, Accepted"
                                                        | simple_status == "Hold, Rejected"
                                                        | simple_status == "WL, Accepted, Withdrawn"
                                                        | simple_status == "WL, Rejected"))

# Transforming simple_status to 1 for accepted and 0 for rejected
data$status_binary <- ifelse(grepl("Accepted", data$simple_status, ignore.case = TRUE), 1, 
                             ifelse(grepl("Rejected", data$simple_status, ignore.case = TRUE), 0, NA))
data$urm_binary <- ifelse(data$urm == "TRUE", 1, 0)
data$inter_binary <- ifelse(data$is_international == "TRUE", 1, 0)
data$softs_binary <- ifelse((data$softs == "T1"| data$softs == "T2"), 1, 0)

# Function to fit models and evaluate them
get_formula <- function(inputdata){
  acceptance <- inputdata$status_binary
  #lsat <- inputdata$lsat
  #gpa <- inputdata$gpa
  #international <- inputdata$inter_binary
  # urm <- inputdata$urm_binary
  #softs <- inputdata$softs_binary
  
  # Splitting the data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(acceptance, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  dataTrain <- inputdata[trainIndex,]
  dataTest <- inputdata[-trainIndex,]
  
  
  # Fit the logistic regression models
  m1 <- glm(status_binary ~ lsat + gpa, data = dataTrain, family = binomial(link = "logit"))
  m2 <- glm(status_binary ~ lsat + gpa + inter_binary, data = dataTrain, family = binomial(link = "logit"))
  m3 <- glm(status_binary ~ lsat + gpa + urm_binary, data = dataTrain, family = binomial(link = "logit"))
  m4 <- glm(status_binary ~ lsat + gpa + urm_binary + inter_binary, data = dataTrain, family = binomial(link = "logit"))
  # Fit additional models
  m5 <- glm(status_binary ~ lsat + gpa + softs, data = dataTrain, family = binomial(link = "logit"))
  m6 <- glm(status_binary ~ lsat + gpa + softs + inter_binary, data = dataTrain, family = binomial(link = "logit"))
  m7 <- glm(status_binary ~ lsat + gpa + softs + urm_binary, data = dataTrain, family = binomial(link = "logit"))
  m8 <- glm(status_binary ~ lsat + gpa + softs + urm_binary + inter_binary, data = dataTrain, family = binomial(link = "logit"))
  
  # Predict on the test set for all models
  predictions_m1 <- predict(m1, dataTest, type = "response")
  predictions_m2 <- predict(m2, dataTest, type = "response")
  predictions_m3 <- predict(m3, dataTest, type = "response")
  predictions_m4 <- predict(m4, dataTest, type = "response")
  predictions_m5 <- predict(m5, dataTest, type = "response")
  predictions_m6 <- predict(m6, dataTest, type = "response")
  predictions_m7 <- predict(m7, dataTest, type = "response")
  predictions_m8 <- predict(m8, dataTest, type = "response")
  
  # Convert probabilities to class labels
  predicted_classes_m1 <- ifelse(predictions_m1 > 0.5, 1, 0)
  predicted_classes_m2 <- ifelse(predictions_m2 > 0.5, 1, 0)
  predicted_classes_m3 <- ifelse(predictions_m3 > 0.5, 1, 0)
  predicted_classes_m4 <- ifelse(predictions_m4 > 0.5, 1, 0)
  predicted_classes_m5 <- ifelse(predictions_m5 > 0.5, 1, 0)
  predicted_classes_m6 <- ifelse(predictions_m6 > 0.5, 1, 0)
  predicted_classes_m7 <- ifelse(predictions_m7 > 0.5, 1, 0)
  predicted_classes_m8 <- ifelse(predictions_m8 > 0.5, 1, 0)
  
  # Calculate the confusion matrix and accuracy for each model
  conf_matrix_m1 <- confusionMatrix(as.factor(predicted_classes_m1), as.factor(dataTest$status_binary))
  conf_matrix_m2 <- confusionMatrix(as.factor(predicted_classes_m2), as.factor(dataTest$status_binary))
  conf_matrix_m3 <- confusionMatrix(as.factor(predicted_classes_m3), as.factor(dataTest$status_binary))
  conf_matrix_m4 <- confusionMatrix(as.factor(predicted_classes_m4), as.factor(dataTest$status_binary))
  conf_matrix_m5 <- confusionMatrix(as.factor(predicted_classes_m5), as.factor(dataTest$status_binary))
  conf_matrix_m6 <- confusionMatrix(as.factor(predicted_classes_m6), as.factor(dataTest$status_binary))
  conf_matrix_m7 <- confusionMatrix(as.factor(predicted_classes_m7), as.factor(dataTest$status_binary))
  conf_matrix_m8 <- confusionMatrix(as.factor(predicted_classes_m8), as.factor(dataTest$status_binary))
  
  # ROC Curve and AUC for each model
  roc_curve_m1 <- roc(dataTest$status_binary, predictions_m1)
  roc_curve_m2 <- roc(dataTest$status_binary, predictions_m2)
  roc_curve_m3 <- roc(dataTest$status_binary, predictions_m3)
  roc_curve_m4 <- roc(dataTest$status_binary, predictions_m4)
  roc_curve_m5 <- roc(dataTest$status_binary, predictions_m5)
  roc_curve_m6 <- roc(dataTest$status_binary, predictions_m6)
  roc_curve_m7 <- roc(dataTest$status_binary, predictions_m7)
  roc_curve_m8 <- roc(dataTest$status_binary, predictions_m8)
  
  auc_m1 <- auc(roc_curve_m1)
  auc_m2 <- auc(roc_curve_m2)
  auc_m3 <- auc(roc_curve_m3)
  auc_m4 <- auc(roc_curve_m4)
  auc_m5 <- auc(roc_curve_m5)
  auc_m6 <- auc(roc_curve_m6)
  auc_m7 <- auc(roc_curve_m7)
  auc_m8 <- auc(roc_curve_m8)
  
  # Print the models using stargazer
  #stargazer(m1, m2, m3, m4, m5, m6, m7, m8, type = "text")
  stargazer(m8, type = "text")
  
  # Print accuracy and AUC for each model
  cat("Model 1: Accuracy = ", conf_matrix_m1$overall['Accuracy'], " AUC = ", auc_m1, "\n")
  cat("Model 2: Accuracy = ", conf_matrix_m2$overall['Accuracy'], " AUC = ", auc_m2, "\n")
  cat("Model 3: Accuracy = ", conf_matrix_m3$overall['Accuracy'], " AUC = ", auc_m3, "\n")
  cat("Model 4: Accuracy = ", conf_matrix_m4$overall['Accuracy'], " AUC = ", auc_m4, "\n")
  cat("Model 5: Accuracy = ", conf_matrix_m5$overall['Accuracy'], " AUC = ", auc_m5, "\n")
  cat("Model 6: Accuracy = ", conf_matrix_m6$overall['Accuracy'], " AUC = ", auc_m6, "\n")
  cat("Model 7: Accuracy = ", conf_matrix_m7$overall['Accuracy'], " AUC = ", auc_m7, "\n")
  cat("Model 8: Accuracy = ", conf_matrix_m8$overall['Accuracy'], " AUC = ", auc_m8, "\n")
}

# Apply the function to each school's data
yale <- subset(data, school_name == "Yale University")
stanford <- subset(data, school_name == "Stanford University")
harvard <- subset(data, school_name == "Harvard University")
chicago <- subset(data, school_name == "University of Chicago")
columbia <- subset(data, school_name == "Columbia University")
penn <- subset(data, school_name == "University of Pennsylvania")
nyu <- subset(data, school_name == "New York University")
duke <- subset(data, school_name == "Duke University")
berk <- subset(data, school_name == "University of California—Berkeley")
virginia <- subset(data, school_name == "University of Virginia")
michigan <- subset(data, school_name == "University of Michigan")
northwestern <- subset(data, school_name == "Northwestern University")
cornell <- subset(data, school_name == "Cornell University")
georgetown <- subset(data, school_name == "Georgetown University")

get_formula(yale)
get_formula(stanford)
get_formula(harvard)
get_formula(chicago)
get_formula(columbia)
get_formula(penn)
get_formula(nyu)
get_formula(duke)
get_formula(berk)
get_formula(virginia)
get_formula(michigan)
get_formula(northwestern)
get_formula(cornell)
get_formula(georgetown)
