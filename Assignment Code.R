# ---- Required Packages ----
# Installation
install.packages("psych")
install.packages("colorspace")
install.packages("ggplot2")
install.packages("farver")
install.packages("dplyr")
install.packages("stringi")
install.packages("stringr")

# Loading Packages
library("psych")
library("ggplot2")
library("dplyr")
library("stringr")
library("str_trim")
library("patchwork")
library("tidyr")


# ---- Extract and load dataset ---- 
retail_dataset = read.csv("retail_data.csv")

names(retail_dataset) # Display Column names

# ---- Dropping Irrelevant Columns ---- 
retail_dataset = retail_dataset[, !(names(retail_dataset) %in% 
                                    c("Name", "Phone", "Email", "Address", "State", 
                                      "City", "Zipcode", "products"))] 
names(retail_dataset) # Display Column names
dim(retail_dataset) # Display Dataset dimensions

# ---- Data Cleaning & Preparation ----
# Convert White space entry to NA
char_cols <- sapply(retail_dataset, is.character)
retail_dataset[char_cols] <- lapply(retail_dataset[char_cols], function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  x
})

# Handling Missing Data
check_missing_values <- function(data) {
  # 1. Check if there are any missing values in entire dataset
  any_missing = any(is.na(data))
  cat("Are there any missing values in the dataset?", any_missing, "\n")

  # 2. Check total number of missing values
  total_missing = sum(is.na(data))
  cat("Total number of missing values: ", total_missing, "\n")
  
  # 3. Count missing values per column
  missing_per_column = colSums(is.na(data))

  cat("Missing values per column: \n")
  for(col_name in names(missing_per_column)){
    cat(col_name, ":", missing_per_column[col_name], "\n")
  }
}
check_missing_values(retail_dataset) # Function to check missing values

impute_missing_values <- function(data) {
  # 1. Set mode for Age, Year, Total Purchases and Rating
  # Mode function
  get_mode <- function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  mode_cols = c("Age", "Country", "Gender", "Income", 
                "Customer_Segment", "Product_Category", "Product_Brand",
                "Feedback", "Shipping_Method", "Payment_Method",
                "Order_Status","Year", "Ratings")
  for(col in mode_cols) {
    if(col %in% names(data)){
      mode_val = get_mode(data[[col]][!is.na(data[[col]])])
      data[[col]][is.na(data[[col]])] = mode_val
    }
  }
  
  # 2. Set mean for Amount, Total_Amount
  # mean_cols = c("Amount", "Total_Amount")
  # for(col in mean_cols){
  #   if(col %in% names(data)){
  #     mean_val = mean(data[[col]], na.rm= TRUE)
  #     data[[col]][is.na(data[[col]])] = mean_val
  #   }
  # }
  
  # 3. Delete missing customer id and transaction id 
  id_cols = c("Customer_ID", "Transaction_ID", "Total_Purchases", 
              "Amount", "Total_Amount", 
              "Date", "Month", "Time")
  for (col in id_cols){
    if (col %in% names(data)){
      data = data[!is.na(data[[col]]), ]
    }
  }
  
  return (data)
  
}
retail_dataset = impute_missing_values(retail_dataset) # Run impute function
check_missing_values(retail_dataset) # Check missing values again

# Deleting Of Duplicate Records
find_duplicates <- function(data){
  # Find duplicate data for Transaction ID
  duplicates = duplicated(data$Transaction_ID)
  cat("Duplicate Transaction IDs: ", sum(duplicates))
}
delete_duplicates <- function(data){
  # Delete duplicates
  data = data[!duplicated(data$Transaction_ID), ]
  return (data)
}
print("=== Before Deletion ===")
find_duplicates(retail_dataset)
retail_dataset = delete_duplicates(retail_dataset)
print("=== After Deletion ===")
find_duplicates(retail_dataset)

# Handling Time format (Removing minute and seconds from Time)
format_time <-function(data){
  data$time_hour <- substr(data$Time, 1, 2) # Add new column time_hour
  return (data)
}
retail_dataset = format_time(retail_dataset)
retail_dataset$time_hour

# Handling Outliers
plot_outliers <-function(data, column = "Total_Amount"){
  ggplot(data) + 
    geom_boxplot(aes(y = .data[[column]], x = column), fill = "lightblue", color = "darkblue") +
    labs(title = "Distribution of Variable",
         y = "Value",
         x = "Variable") +
    theme_minimal() 
}
plot_outliers(retail_dataset)

remove_outliers <- function (data, column = "Total_Amount", threshold_value = 1){
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  
  # Define outlier thresholds
  lower_bound = Q1 - threshold_value * IQR
  upper_bound = Q3 + threshold_value * IQR
  
  # Filter rows (using dynamic column name)
  clean_data <- data[data[[column]] >= lower_bound & 
                       data[[column]] <= upper_bound, ]
  
  return (clean_data)
}
retail_dataset = remove_outliers(retail_dataset) # Call function to remove outliers
plot_outliers(retail_dataset) # Plot again

# Deleting Transaction ID & Time
retail_dataset = subset(retail_dataset, select= -c(Transaction_ID, Time)) # Deleting Transaction ID and Time since they are no longer needed

# ---- Data Exploration ----
data_summary <- function(data){
  head(retail_dataset) # Display top 5 rows
  
  str(retail_dataset) # View Data Structure
  
  summary(retail_dataset) # View Summary Statistics
}
data_summary(retail_dataset)
describe(retail_dataset)

# ---- Basic Visualization ----
## View Frequency and Density Distribution of Numerical Data
view_numeric_data_distribution <- function(column = "Total_Amount"){
  p1 <- ggplot(retail_dataset, aes(x = .data[[column]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Distribution of ", column),
         x = column,
         y = "Frequency") +
    theme_minimal()
  
  p2 <- ggplot(retail_dataset, aes(x = .data[[column]])) +
    geom_density(fill = "lightgreen", alpha = 0.7) +
    labs(title = paste("Density Plot of ", column),
         x = column,
         y = "Density") +
    theme_minimal()
  
  print(p1 + p2)
}
view_numeric_data_distribution()

names(retail_dataset)
## View Frequency of Categorical Data
view_categorical_data_distribution <- function(column = "Ratings"){
  ggplot(retail_dataset, aes(x = .data[[column]])) +
    geom_bar(fill = "steelblue") +
    labs(title = paste("Distribution of Categorical Variable", column),
         x = column,
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
view_categorical_data_distribution("Ratings")

# ---- Objective 1 ---- 
# -- Analysis 1-1 --
# Create first plot
p1 <- ggplot(retail_dataset, aes(x = Income, y = Amount, fill = Income)) +
  geom_boxplot() +
  labs(title = "Transaction Amount by Income Bracket",
       x = "Income Bracket",
       y = "Amount Spent per Transaction") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Create second plot
p2 <- ggplot(retail_dataset, aes(x = Income, y = Total_Purchases, fill = Income)) +
  geom_boxplot() +
  labs(title = "Purchases by Income Bracket",
       x = "Income Bracket",
       y = "Total Purchases per Transaction") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Combine plots side by side
combined_plot <- p1 + p2
print(combined_plot)

# -- Analysis 1-2 --
ggplot(retail_dataset, aes(x = Income, fill = Product_Category)) +
  geom_bar(position = "fill") +
  labs(title = "Product Category Preferences by Income Bracket",
       x = "Income Bracket",
       y = "Proportion of Purchases",
       fill = "Product Category") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent)

# Plot 2
brand_income <- retail_dataset %>%
  count(Income, Product_Brand) %>%
  group_by(Income) %>%
  mutate(percent = n / sum(n))

ggplot(brand_income, aes(x = Product_Brand, y = Income, fill = percent)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Brand Preferences by Income Bracket",
       x = "Brand",
       y = "Income Bracket",
       fill = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Analysis 1-3 --
retail_dataset %>%
  group_by(Income, Payment_Method) %>%
  summarise(avg_spend = mean(Total_Amount), .groups = "drop") %>%
  ggplot(aes(x = Payment_Method, y = avg_spend, color = Income, group = Income)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Average Spending by Payment Method and Income",
       x = "Payment Method",
       y = "Average Amount Spent ($)",
       color = "Income Bracket") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

# Chi-square test for payment method distribution
chisq.test(table(retail_dataset$Income, retail_dataset$Payment_Method))
chisq.test(table(retail_dataset$Income, retail_dataset$Payment_Method))$residuals
mosaicplot(table(retail_dataset$Income, retail_dataset$Payment_Method), shade = TRUE)

# ---- Objective 2 ----
# -- Analysis 2-1 --
chisq.test(table(retail_dataset$Gender, retail_dataset$Customer_Segment))
chisq.test(table(retail_dataset$Gender, retail_dataset$Customer_Segment))$residuals
mosaicplot(table(retail_dataset$Gender, retail_dataset$Customer_Segment), shade = TRUE)

# -- Analysis 2-2 --
chisq.test(table(retail_dataset$Gender, retail_dataset$Income))
chisq.test(table(retail_dataset$Gender, retail_dataset$Income))$residuals
mosaicplot(table(retail_dataset$Gender, retail_dataset$Income), shade = TRUE)

# -- Analysis 2-3 --
# Plot 1
ggplot(retail_dataset, aes(x = Gender, y = Total_Amount, fill = Gender)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Purchasing Power by Gender",
       subtitle = "Distribution of Transaction Amounts",
       x = "Gender",
       y = "Amount Spent ($)",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")

# Plot 2
retail_dataset %>%
  group_by(Gender, Product_Category) %>%
  summarise(avg_spend = mean(Total_Amount), .groups = "drop") %>%
  ggplot(aes(x = Product_Category, y = avg_spend, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Average Spending by Gender Across Product Categories",
       x = "Product Category",
       y = "Average Amount Spent ($)",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Plot 3
ggplot(retail_dataset, aes(x = Total_Purchases, fill = Gender)) +
  geom_density(alpha = 0.6) +
  labs(title = "Purchase Frequency Distribution by Gender",
       x = "Number of Purchases",
       y = "Density",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")

# ---- Objective 3 ----
# -- Analysis 3-1 --
# category_stats <- retail_dataset %>%
#   group_by(Product_Category) %>%
#   summarise(
#     total_revenue = sum(Total_Amount),
#     transaction_count = n(),
#     avg_spend = mean(Total_Amount),
#     .groups = 'drop'
#   )
# 
# ggplot(category_stats, aes(x=transaction_count, y = avg_spend,
#                            size = total_revenue, color=Product_Category)) +
#   geom_point(alpha=0.8) +
#   geom_text(aes(label = Product_Category), hjust = 0.5, vjust = -2.3, size = 3) +
#   scale_size(range = c(5, 15), name = "Total Revenue") +
#   labs(title="Product Category Performance: Volume vs. Value",
#        subtitle = "Bubble size represents total revenue",
#        x = "Number of transactions",
#        y = "Average Transaction value ($)")+
#   theme_minimal() +
#   scale_color_brewer(palette = "Set2") +
#   theme(legend.position = "right")

ggplot(retail_dataset, aes(
  x = Total_Purchases,
  y = Amount,         
)) +
  geom_point(alpha = 0.2) +

  labs(
    title = "Correlation: Number of Purchases vs. Spending per item",
    x = "Number of Purchases (Quantity)",
    y = "Total Amount Spent per item ($)",
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "right")

# -- Analysis 3-2 --
chisq.test(table(retail_dataset$Product_Brand, retail_dataset$Ratings))
chisq.test(table(retail_dataset$Product_Brand, retail_dataset$Ratings))$residuals
mosaicplot(table(retail_dataset$Product_Brand, retail_dataset$Ratings), shade = TRUE)

# ---- Objective 4 ----
# -- Analysis 4-1 --
payment_category <- retail_dataset %>%
  count(Payment_Method, Product_Category) %>%
  group_by(Payment_Method) %>%
  mutate(percent = n/sum(n))

ggplot(payment_category, aes(x = Product_Category, y = Payment_Method, fill = percent)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), color = "white", size = 3) +
  scale_fill_gradient(low = "steelblue", high = "darkred", name = "Percentage") +
  labs(title = "Payment Method Preferences by Product Category",
       subtitle = "Percentage of transactions for each payment method",
       x = "Product Category",
       y = "Payment Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Analysis 4-2 --
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

retail_dataset %>%
  # Convert Month to an ordered factor
  mutate(Month = factor(Month, levels = month_order)) %>%
  count(Month, Payment_Method) %>%
  ggplot(aes(x = Month, y = n, color = Payment_Method, group = Payment_Method)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Payment Method Usage Trends",
    subtitle = "Monthly transaction counts by payment method",
    x = "Month", 
    y = "Number of Transactions",
    color = "Payment Method"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ---- Objective 5 ----
# -- Analysis 5-1 --
income_levels <- retail_dataset %>%
  count(Country, Income) %>%
  group_by(Income) %>%
  mutate(percent = n/sum(n))

ggplot(income_levels, aes(x = Country, y= Income, fill=percent))+
  geom_tile() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), color = "white", size = 3) +
  scale_fill_gradient(low = "steelblue", high = "darkred", name = "Percentage") +
  labs(title = "Income levels per each country",
       subtitle = "Percentage of income level for each country",
       x = "Country",
       y = "Income level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Analysis 5-2 --
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

retail_dataset %>%
  # Convert Month to an ordered factor
  mutate(Month = factor(Month, levels = month_order)) %>%
  group_by(Country, Month) %>%
  summarise(avg_spend = mean(Total_Amount), .groups = "drop") %>%
  ggplot(aes(x = Month, y = avg_spend, color = Country, group = Country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Monthly Average Spending by Country",
       x = "Month",
       y = "Average Amount Spent (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Analysis 5-3 --
retail_dataset %>%
  count(Country, Product_Category) %>%
  group_by(Country) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = Product_Category, y = Country, fill = percent)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Product Category Preferences by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Objective 6 ----
# -- Analysis 6-1 --
age_rating <- retail_dataset %>%
  count(Age, Ratings) %>%
  group_by(Age) %>%
  mutate(total_ratings = n)

ggplot(age_rating, aes(x = Age, y = total_ratings, color = Ratings)) +
  geom_point(size = 1) +
  geom_line() +
  labs(title = "Ratings Per Age",
       x = "Age",
       y = "Total Number Of Ratings",
       color = "Ratings")
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# -- Analysis 6-2 --
age_rating_High <- retail_dataset %>%
  filter(Income == "High") %>%
  count(Age, Ratings) %>%
  group_by(Age) %>%
  mutate(total_ratings = n)

age_rating_Low <- retail_dataset %>%
  filter(Income == "Low") %>%
  count(Age, Ratings) %>%
  group_by(Age) %>%
  mutate(total_ratings = n)
  
p1 <- ggplot(age_rating_High, aes(x = Age, y = total_ratings, color = Ratings)) +
  geom_point(size = 1) +
  geom_line() +
  labs(title = "Ratings of High Income Customers",
       x = "Age",
       y = "Total Number Of Ratings",
       color = "Ratings")
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(age_rating_Low, aes(x = Age, y = total_ratings, color = Ratings)) +
  geom_point(size = 1) +
  geom_line() +
  labs(title = "Ratings of Low Income Customers",
       x = "Age",
       y = "Total Number Of Ratings",
       color = "Ratings")
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
print(p1 + p2)


