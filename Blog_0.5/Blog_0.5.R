# Load necessary libraries
library(caret)
library(pROC)
library(stargazer)
library(rdrobust)
library(dplyr)
library(ggplot2)
library(rdd)


# Source file path
source = "/Users/frederickliu/Desktop/Offerstrand/Research/Law School Admissions Predictor/lsdata.csv"
comp = read.csv(source)

# Using data 
data <- subset(comp, (simple_status == "accepted"
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
#data$softs_binary <- ifelse((data$softs == "T1"| data$softs == "T2"), 1, 0)

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

getResult <- function(data, dyear) {
  
  # Convert date columns to Date type
  convert_date <- function(date_column) {
    if (is.character(date_column)) {
      # Try different formats
      date_column <- as.Date(date_column, format="%m/%d/%y")
      if (all(is.na(date_column))) {
        date_column <- as.Date(date_column, format="%Y-%m-%d")
      }
    }
    return(date_column)
  }
  
  # Convert necessary date columns
  date_columns <- c("sent_at", "received_at", "complete_at", "ur_at", "ur2_at", "interview_at", "decision_at")
  for (col in date_columns) {
    data[[col]] <- convert_date(data[[col]])
  }
  
  # Define start and end dates
  sdate <- paste0("20", as.character(dyear), "-10-01")
  edate <- paste0("20", as.character(dyear + 1), "-08-01")
  start_date <- as.Date(sdate, format = "%Y-%m-%d")
  end_date <- as.Date(edate, format = "%Y-%m-%d")
  
  # Filter data based on date range
  filtered_data <- data %>%
    filter((sent_at >= start_date & sent_at <= end_date) |
             (received_at >= start_date & received_at <= end_date) |
             (complete_at >= start_date & complete_at <= end_date) |
             (ur_at >= start_date & ur_at <= end_date) |
             (ur2_at >= start_date & ur2_at <= end_date) |
             (interview_at >= start_date & interview_at <= end_date) |
             (decision_at >= start_date & decision_at <= end_date))
  
  # Ensure all filtered data is within the range by checking each date column
  filtered_data <- filtered_data %>%
    filter((sent_at >= start_date & sent_at <= end_date) | is.na(sent_at)) %>%
    filter((received_at >= start_date & received_at <= end_date) | is.na(received_at)) %>%
    filter((complete_at >= start_date & complete_at <= end_date) | is.na(complete_at)) %>%
    filter((ur_at >= start_date & ur_at <= end_date) | is.na(ur_at)) %>%
    filter((ur2_at >= start_date & ur2_at <= end_date) | is.na(ur2_at)) %>%
    filter((interview_at >= start_date & interview_at <= end_date) | is.na(interview_at)) %>%
    filter((decision_at >= start_date & decision_at <= end_date) | is.na(decision_at))
  
  # Print the filtered data summary
  #print("Filtered data summary:")
  #print(summary(filtered_data))
  
  # Sort the data by the decision_at date
  sorted_data <- filtered_data %>%
    arrange(decision_at)
  
  # Print to check the filtering and sorting
  print(table(sorted_data$decision_at))
  
  # Define the cutoff date
  # median_decision_date <- median(sorted_data$decision_at, na.rm = TRUE)
  # if (is.na(median_decision_date)) {
  #   stop("Median decision date is NA. Check the date conversion and filtering steps.")
  # }
  # print(paste("Median decision date:", median_decision_date))
  
  # Define the cutoff date as the median decision date
  # cutoff_date <- median_decision_date
  #Define the cutoff date as the date that outputs the most decisions
  # cutoff_date <- as.Date(names(table(sorted_data$decision_at))[which.max(table(sorted_data$decision_at))])
  # print(paste("Date with most decisions released: ", cutoff_date))
  
  cutoff_date <- sorted_data$decision_at[1]
  
  # Create a running variable and treatment indicator
  sorted_data$days_since_cutoff <- as.numeric(sorted_data$sent_at - cutoff_date)
  sorted_data$treatment <- ifelse(sorted_data$days_since_cutoff >= 0, 1, 0)
  
  # Prepare outcome variable
  sorted_data$admitted <- sorted_data$status_binary
  
  # Ensure LSAT and GPA columns are numeric
  sorted_data$lsat <- as.numeric(sorted_data$lsat)
  sorted_data$gpa <- as.numeric(sorted_data$gpa)
  
  # Perform RDD analysis with covariates using logistic regression
  rdd_result <- rdrobust(y = sorted_data$admitted, x = sorted_data$days_since_cutoff, c = 0, covs = cbind(sorted_data$lsat, sorted_data$gpa), kernel = "triangular", p = 1)
  
  # Print the results
  print(summary(rdd_result))
  
  # Extract and exponentiate the coefficients to get odds ratios
  rdd_coef <- rdd_result$coef
  odds_ratios <- exp(rdd_coef)
  print("Odds Ratios:")
  print(odds_ratios)
  
  # Plot the RDD results
  ggplot(sorted_data, aes(x = days_since_cutoff, y = admitted)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, col = "blue", data = subset(sorted_data, days_since_cutoff < 0)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, col = "red", data = subset(sorted_data, days_since_cutoff >= 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Regression Discontinuity Design Plot",
         x = "Days Since Cutoff",
         y = "Probability of Admission") +
    theme_minimal()
  
  # # Check balance for covariates
  # check_balance <- function(data, covariate, cutoff) {
  #   result <- rdrobust(data[[covariate]], data$days_since_cutoff, c = 0, 
  #                      h = NULL, p = 1, kernel = "triangular", bwselect = "mserd")
  #   return(result)
  # }
  # 
  # covariates <- c("lsat", "gpa") # Add other covariates as needed
  # balance_results <- lapply(covariates, function(cov) check_balance(sorted_data, cov, cutoff_date))
  # 
  # # Print summary of balance tests
  # lapply(balance_results, summary)
  
  # Optional: Plot balance checks
  
  ggplot(sorted_data, aes(x = days_since_cutoff, y = lsat)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, col = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Balance Check for LSAT",
         x = "Days Since Cutoff",
         y = "LSAT Score") +
    theme_minimal()

  ggplot(sorted_data, aes(x = days_since_cutoff, y = gpa)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, col = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Balance Check for GPA",
         x = "Days Since Cutoff",
         y = "GPA") +
    theme_minimal()
}

getResult(yale, 21)
getResult(georgetown, 21)
getResult(harvard, 21)

getResult(yale, 22)
getResult(georgetown, 22)
getResult(harvard, 22)



