# Load necessary libraries
library(caret)
library(pROC)
library(stargazer)
library(rdrobust)
library(dplyr)
library(ggplot2)
library(rdd)

# T35 Schools besidess YHS

# Source file path
source = "/Users/frederickliu/Desktop/Offerstrand/Research/Law School Admissions Predictor/lsdata.csv"
comp = read.csv(source)

data <- subset(comp, cycle_id >= 13 & cycle_id <= 23 & (simple_status == "accepted"
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
data$scholarship_binary <- ifelse((data$scholarship > 0 & data$status_binary == 1), 1, 0)
#data$softs_binary <- ifelse((data$softs == "T1"| data$softs == "T2"), 1, 0)

# Apply the function to each school's data
#yale <- subset(data, school_name == "Yale University")
#stanford <- subset(data, school_name == "Stanford University")
#harvard <- subset(data, school_name == "Harvard University")
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
ucla <- subset(data, school_name == "University of California—Los Angeles")
ut_austin <- subset(data, school_name == "University of Texas at Austin")
vanderbilt <- subset(data, school_name == "Vanderbilt University")
usc <- subset(data, school_name == "University of Southern California")
minnesota <- subset(data, school_name == "University of Minnesota")
washington_st_louis <- subset(data, school_name == "Washington University in St. Louis")
georgia <- subset(data, school_name == "University of Georgia")
unc <- subset(data, school_name == "University of North Carolina")
ohio_state <- subset(data, school_name == "Ohio State University")
wake_forest <- subset(data, school_name == "Wake Forest University")
byu <- subset(data, school_name == "Brigham Young University")
florida_levin <- subset(data, school_name == "University of Florida (Levin)")
boston_university <- subset(data, school_name == "Boston University")
notre_dame <- subset(data, school_name == "University of Notre Dame")
boston_college <- subset(data, school_name == "Boston College")
fordham <- subset(data, school_name == "Fordham University")
texas_am <- subset(data, school_name == "Texas A&M University")
george_mason <- subset(data, school_name == "George Mason University")
utah <- subset(data, school_name == "University of Utah")
asu <- subset(data, school_name == "Arizona State University")
emory <- subset(data, school_name == "Emory University")
iowa <- subset(data, school_name == "University of Iowa")
gw <- subset(data, school_name == "George Washington University")
alabama <- subset(data, school_name == "University of Alabama")
uci <- subset(data, school_name == "University of California—Irvine")
wash_lee <- subset(data, school_name == "Washington and Lee University")
wisconsin <- subset(data, school_name == "University of Wisconsin")
kansas <- subset(data, school_name == "University of Kansas")
illinois_urbana <- subset(data, school_name == "University of Illinois—Urbana Champaign")
villanova <- subset(data, school_name == "Villanova University")
indiana_bloomington <- subset(data, school_name == "Indiana University - Bloomington")
william_mary <- subset(data, school_name == "William & Mary Law School")
smu <- subset(data, school_name == "Southern Methodist University")
pepperdine <- subset(data, school_name == "Pepperdine University")
uw <- subset(data, school_name == "University of Washington")
baylor <- subset(data, school_name == "Baylor University")
maryland <- subset(data, school_name == "University of Maryland")


getEstimatedScholarship <- function(data) {
  #Remove missing values
  data <- na.omit(data)
  
  # Split data into training and testing sets
  set.seed(123) # for reproducibility
  index <- createDataPartition(data$scholarship, p = 0.8, list = FALSE)
  train_data <- data[index, ]
  test_data <- data[-index, ]
  
  # Train the linear model
  model <- lm(scholarship ~ lsat + gpa + urm_binary + lsat*gpa, data = train_data)
  
  # Predict on test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$scholarship - predictions)^2))
  
  # Assuming binary outcome for AUC calculation
  actual_binary <- ifelse(test_data$scholarship > median(test_data$scholarship), 1, 0)
  pred_binary <- ifelse(predictions > median(predictions), 1, 0)
  auc <- roc(actual_binary, pred_binary)$auc
  
  # Output RMSE and AUC
  cat("RMSE: ", rmse, "\n")
  cat("AUC: ", auc, "\n")
  
  stargazer(model, type = "text")
  
  # # Plot actual vs predicted scholarships
  # ggplot(data = test_data, aes(x = scholarship, y = predictions)) +
  #   geom_point(alpha = 0.5) +
  #   geom_smooth(method = "lm", col = "blue") +
  #   labs(title = "Actual vs. Predicted Scholarships", x = "Actual Scholarship", y = "Predicted Scholarship")
}

getEstimatedScholarship(chicago)
getEstimatedScholarship(columbia)
getEstimatedScholarship(penn)
getEstimatedScholarship(nyu)
getEstimatedScholarship(duke)
getEstimatedScholarship(berk)
getEstimatedScholarship(virginia)
getEstimatedScholarship(michigan)
getEstimatedScholarship(northwestern)
getEstimatedScholarship(cornell)
getEstimatedScholarship(georgetown)

getEstimatedScholarship(ucla) #15
getEstimatedScholarship(vanderbilt)
getEstimatedScholarship(washington_st_louis)
getEstimatedScholarship(ut_austin)
getEstimatedScholarship(georgia)
getEstimatedScholarship(notre_dame)
getEstimatedScholarship(usc)
getEstimatedScholarship(illinois_urbana)
getEstimatedScholarship(unc)
getEstimatedScholarship(wake_forest)
getEstimatedScholarship(minnesota)
getEstimatedScholarship(ohio_state)
getEstimatedScholarship(florida_levin)
getEstimatedScholarship(boston_college)
getEstimatedScholarship(wash_lee)
getEstimatedScholarship(emory) #30

getEstimatedScholarship(iowa)
getEstimatedScholarship(boston_university)
getEstimatedScholarship(villanova)
getEstimatedScholarship(utah)
getEstimatedScholarship(william_mary)

getAP = function(data){
  #model <- glm(status_binary ~ lsat + gpa + urm_binary + lsat*gpa, data = data, family = binomial(link = "logit"))
  model <- glm(scholarship_binary ~ lsat + gpa + urm_binary + lsat*gpa, data = data, family = binomial(link = "logit"))
  stargazer(model, type = "text")
}

getAP(chicago)
getAP(columbia)
getAP(penn)
getAP(nyu)
getAP(duke)
getAP(berk)
getAP(virginia)
getAP(michigan)
getAP(northwestern)
getAP(cornell)
getAP(georgetown)

getAP(ucla) #15
getAP(vanderbilt)
getAP(washington_st_louis)
getAP(ut_austin)
getAP(georgia)
getAP(notre_dame)
getAP(usc)
getAP(illinois_urbana)
getAP(unc)
getAP(wake_forest)
getAP(minnesota)
getAP(ohio_state)
getAP(florida_levin)
getAP(boston_college)
getAP(wash_lee)
getAP(emory) #30

getAP(iowa)
getAP(boston_university)
getAP(villanova)
getAP(utah)
getAP(william_mary)


