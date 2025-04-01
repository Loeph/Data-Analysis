data <- read.csv("C:/Users/T.M/OneDrive - University of Birmingham/Business Analytics Msc/Marketing Analytics and Behavioural Sc/Assignment/SmartFresh Retail.csv")
str(data)

#Data Cleaning
data$Customer_ID <- factor(data$Customer_ID) #Convert customer ID to categorical variable
data[data == ""] <- NA #Replace missing values with NA
data <- na.omit(data) 
nrow(data)  
ncol(data)


#Adding more variables - Remember to create a data dictionary
install.packages("lubridate")
library(lubridate)
current_year <- year(Sys.Date())
data$age <- current_year - data$Year_Birth #Calculating age from year of birth
data$Dt_Customer <- dmy(data$Dt_Customer)
data$enrollment_year <- year(data$Dt_Customer)
data$years_with_company <- current_year - data$enrollment_year #Calculating how many years the customer has been with the company
str(data)

selected_columns <- data[, c("age", "Annual_Income", "Kidhome", "Teenhome" ,"Last_Interaction", "Promo_Purchases", "Purchases_Online", "Purchases_Store", "years_with_company", "Spend_Meat", "Spend_Wine", "Spend_LuxuryGoods", "Spend_Treats", "Spend_WellnessProducts", "Spend_OrganicFood")]

#Measures of central tendency
library(modeest)
calculate_mode <- function(x) {
  unique_x <- unique(na.omit(x)) 
  return(unique_x[which.max(tabulate(match(x, unique_x)))]) #Calculate mode
}
results <- data.frame(
  Mean = sapply(selected_columns, function(col) mean(col, na.rm = TRUE)),
  Median = sapply(selected_columns, function(col) median(col, na.rm = TRUE)),
  Mode = sapply(selected_columns, function(col) calculate_mode(col)),
  Range = sapply(selected_columns, function(col) diff(range(col, na.rm = TRUE))),
  Variance = sapply(selected_columns, function(col) var(col, na.rm = TRUE)),
  Std_Deviation = sapply(selected_columns, function(col) sd(col, na.rm = TRUE)),
  Coeff_of_variation = sapply(selected_columns, function(col) sd(col, na.rm = TRUE) / mean(col, na.rm = TRUE)) 
  )
print(results)
