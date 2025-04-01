#COMPARING MEANS FOR SPEND ON TREATS
# Filter data for customers with Kidhome = 0 and Kidhome > 0
Kidhome_zero <- data %>% filter(Kidhome == 0)
Kidhome_greater_than_zero <- data %>% filter(Kidhome > 0)

# Create a summary table
kid_summary_table <- data.frame(
  Group = c("Kidhome = 0", "Kidhome > 0"),
  Count = c(nrow(Kidhome_zero), nrow(kidhome_greater_than_zero)),
  Mean_Spend_Treats = c(mean(Kidhome_zero$Spend_Treats, na.rm = TRUE), mean(Kidhome_greater_than_zero$Spend_Treats, na.rm = TRUE)),
  SD_Spend_Treats = c(sd(Kidhome_zero$Spend_Treats, na.rm = TRUE), sd(Kidhome_greater_than_zero$Spend_Treats, na.rm = TRUE))
)
print(kid_summary_table)

#Filter data for customers with Teenhome = 0 and Teenhome > 0
Teenhome_zero <- data %>% filter(Teenhome == 0)
Teenhome_greater_than_zero <- data %>% filter(Teenhome > 0)

# Create a summary table
kid_summary_table <- data.frame(
  Group = c("Teenhome = 0", "Teenhome > 0"),
  Count = c(nrow(Teenhome_zero), nrow(Teenhome_greater_than_zero)),
  Mean_Spend_Treats = c(mean(Kidhome_zero$Spend_WellnessProducts, na.rm = TRUE), mean(Kidhome_greater_than_zero$Spend_WellnessProducts, na.rm = TRUE)),
  SD_Spend_Treats = c(sd(Kidhome_zero$Spend_WellnessProducts, na.rm = TRUE), sd(Kidhome_greater_than_zero$Spend_WellnessProducts, na.rm = TRUE))
)
print(kid_summary_table)
summary(data)

data$Kidhome_Group <- ifelse(data$Kidhome > 0, "Kid home", "No Kid home")

# Perform Levene's Test for equality of variances
leveneTest(Spend_Treats ~ Kidhome_Group, data = data)

# Perform t-test for Spend_Treats between customers with Kidhome = 0 and Kidhome > 0
t.test(Spend_Treats ~ Kidhome_Group, data = data, var.equal = FALSE)



#Remove outliers from Annual_Income
# Calculate the first and third quartiles
Q1 <- quantile(data$Annual_Income, 0.25)
Q3 <- quantile(data$Annual_Income, 0.75)

# Calculate the interquartile range (IQR)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out the outliers
data_clean <- data %>%
  filter(Annual_Income >= lower_bound & Annual_Income <= upper_bound)
# View the cleaned data
head(data_clean)
nrow(data_clean)

#Income level of those that have kids
leveneTest(Annual_Income ~ Kidhome_Group, data = data)
t.test(Annual_Income ~ Kidhome_Group, data = data, var.equal = FALSE)
