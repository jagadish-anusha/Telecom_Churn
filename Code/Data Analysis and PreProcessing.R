#Data Exploration and R
#Data Set Used : telecom_customer_churn.csv
# Installing required PAckages 

#Setting Current Working Directory
setwd("C:\\Users\\Dheeraj\\Downloads\\Studies\\Semester 3\\AIT 664\\Project\\Final Package\\Code\\DATASET")

telecom_customer_churn_df <- read.csv("telecom_customer_churn.csv", na.strings = c("", "N/A", "NULL"))
library(dplyr)

#1. Checking for missing values and plotting the summary
missing_values_summary <- sapply(telecom_customer_churn_df, function(x) sum(is.na(x)))
print(missing_values_summary)
# Load necessary libraries
library(ggplot2)
library(dplyr)
# Convert the summary to a dataframe for plotting
missing_values_df <- data.frame(
  Column = names(missing_values_summary),
  MissingValues = as.numeric(missing_values_summary)
)
# Filter out columns with zero missing values to declutter the plot
missing_values_df <- missing_values_df %>%
  filter(MissingValues > 0)
# Create the bar plot
ggplot(missing_values_df, aes(x = reorder(Column, MissingValues), y = MissingValues)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Column", y = "Number of Missing Values", title = "Missing Values Summary") +
  coord_flip() # Flip the axes to make the plot horizontal




#2. Handling Missing Values
# Numerical columns with missing values
numerical_columns <- c("Avg.Monthly.Long.Distance.Charges", 
                       "Avg.Monthly.GB.Download")

# Categorical columns with missing values
categorical_columns <- c("Multiple.Lines", 
                         "Internet.Service", 
                         "Internet.Type", 
                         "Online.Security", 
                         "Online.Backup", 
                         "Device.Protection.Plan", 
                         "Premium.Tech.Support", 
                         "Streaming.TV", 
                         "Streaming.Movies", 
                         "Streaming.Music", 
                         "Unlimited.Data","Churn.Category", "Churn.Reason")

# Assigning 0 to missing values in numerical columns
for(col in numerical_columns) {
  telecom_customer_churn_df[[col]][is.na(telecom_customer_churn_df[[col]])] <- 0
}
# Assigning "N/A" to missing values in categorical columns
for(col in categorical_columns) {
  telecom_customer_churn_df[[col]][is.na(telecom_customer_churn_df[[col]])] <- "N/A"
}
# Checking the results
summary(telecom_customer_churn_df)

# Checking for missing values again an printing the summary
missing_values_summary <- sapply(telecom_customer_churn_df, function(x) sum(is.na(x)))
print(missing_values_summary)

#success no missing values !!!!

#3. Duplucates checking for the primary column cus id
# Checking for duplicate Customer.ID values
duplicated_ids <- telecom_customer_churn_df[duplicated(telecom_customer_churn_df$Customer.ID) | 
                                              duplicated(telecom_customer_churn_df$Customer.ID, fromLast = TRUE), ]

# Printing the number of duplicates found, if any
cat("Number of duplicate Customer.ID entries:", nrow(duplicated_ids), "\n\n")

# 0 duplicates 



## Detecting outliers

# List of continuous variables
continuous_vars <- c("Age", "Monthly.Charge")

# Function to detect outliers using the IQR method and return the count of outliers
detect_outliers_iqr_count <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(sum(x < lower_bound | x > upper_bound, na.rm = TRUE))
}

# Loop through the list of continuous variables to detect and print the number of outliers
for(var in continuous_vars) {
  outlier_count <- detect_outliers_iqr_count(telecom_customer_churn_df[[var]])
  cat(var, "has", outlier_count, "outliers\n")
}


#### NO OUTLIERS!!!!!!

##
install.packages("openxlsx")
# Load the openxlsx package
library(openxlsx)

# Specify the path and name of the Excel file you want to create
file_path <- "C:\\Users\\Dheeraj\\Downloads\\Studies\\Semester 3\\AIT 664\\Project\\DATASET\\cleaned_telecom_customer_churn.xlsx"

# Write the DataFrame to an Excel file
write.xlsx(telecom_customer_churn_df, file = file_path, sheetName = "Cleaned Data", overwrite = TRUE)

# Print a message to confirm file creation
cat("Excel file created at:", file_path, "\n")

##VISUALIZATIONS
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

#Churn Rate by Internet Service Type
ggplot(telecom_customer_churn_df, aes(x = Internet.Type, fill = Customer.Status)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Churn Rate by Internet Service Type", x = "Internet Service Type", y = "Percentage")

#Customer Tenure Distribution
ggplot(telecom_customer_churn_df, aes(x = Tenure.in.Months, fill = Customer.Status)) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6) +
  labs(title = "Customer Tenure Distribution", x = "Tenure (Months)", y = "Count")

#Monthly Charges by Customer Status
ggplot(telecom_customer_churn_df, aes(x = Customer.Status, y = Monthly.Charge)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Customer Status", x = "Customer Status", y = "Monthly Charge")


#Churn Rate by Contract Type
ggplot(telecom_customer_churn_df, aes(x = Contract, fill = Customer.Status)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Churn Rate by Contract Type", x = "Contract Type", y = "Percentage")



###Distribution of Churn Categories for Churned customers
library(ggplot2)
library(scales)

# Filter the dataset for customers whose status is "Churned"
churned_customers <- subset(telecom_customer_churn_df, Customer.Status == "Churned")

# Calculate the frequency of each churn category
churn_category_freq <- table(churned_customers$Churn.Category)

# Calculate percentages
percentages <- round(prop.table(churn_category_freq) * 100, 2)

# Create a data frame for plotting
pie_data <- data.frame(churn_category = names(churn_category_freq),
                       frequency = as.numeric(churn_category_freq))

# Create a pie chart with percentage labels
pie_chart <- ggplot(data = pie_data, aes(x = "", y = frequency, fill = churn_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentages, "%")), 
            position = position_stack(vjust = 0.5)) +  
  labs(title = "Distribution of Churn Categories for Churned Customers",
       fill = "Churn Category") +
  theme_void()

# Display the pie chart
print(pie_chart)

library(ggplot2)

library(dplyr)
install.packages("leaflet")

# Load the USA map data
library(ggplot2)
library(sf)
library(dplyr)

library(leaflet)
library(dplyr)


# Filter the dataset for relevant columns
map_data <- subset(telecom_customer_churn_df, !is.na(Latitude) & !is.na(Longitude))

# Create a Leaflet map
map <- leaflet(data = map_data) %>%
  addTiles()  # Add default OpenStreetMap tiles as basemap

# Add customer locations to the map
map <- map %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~ifelse(Customer.Status == "Churned", "red",
                    ifelse(Customer.Status == "Stayed", "blue", "green")),
    radius = 5,
    popup = ~paste("Customer ID:", Customer.ID, "<br>",
                   "Customer Status:", Customer.Status),
    label = ~Customer.ID
  )

# Add icon legend
map <- map %>%
  addLegend(
    position = "bottomright",
    colors = c("red", "blue", "green"),
    labels = c("Churned", "Stayed", "Joined"),
    title = "Customer Status",
    opacity = 1
  )

# Display the map
map

# Filter the dataset for relevant columns
map_data <- subset(telecom_customer_churn_df, !is.na(Latitude) & !is.na(Longitude))

# Create a Leaflet map
map <- leaflet(data = map_data) %>%
  addTiles()  # Add default OpenStreetMap tiles as basemap

# Filter the dataset to only include churned customers
churned_data <- subset(map_data, Customer.Status == "Churned")

# Add customer locations to the map
map <- map %>%
  addCircleMarkers(
    data = churned_data,
    lng = ~Longitude,
    lat = ~Latitude,
    color = "red",
    radius = 5,
    popup = ~paste("Customer ID:", Customer.ID, "<br>",
                   "Customer Status:", Customer.Status),
    label = ~Customer.ID
  )

# Add icon legend
map <- map %>%
  addLegend(
    position = "bottomright",
    colors = "red",
    labels = "Churned",
    title = "Customer Status",
    opacity = 1
  )

# Display the map
map

library(ggplot2)

# Filter the dataset for relevant columns
offer_status_data <- subset(telecom_customer_churn_df, !is.na(Offer) & !is.na(Customer.Status))

# Aggregate data by Offer and Customer Status
agg_data <- aggregate(Customer.ID ~ Offer + Customer.Status, data = offer_status_data, FUN = length)

# Create bar plot
bar_plot <- ggplot(agg_data, aes(x = Offer, y = Customer.ID, fill = Customer.Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Customer Status by Offer",
       x = "Offer",
       y = "Number of Customers",
       fill = "Customer Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Churned" = "red", "Stayed" = "green", "Joined" = "blue"))

# Display the bar plot
print(bar_plot)

# Install and load the treemap library
install.packages("treemap")
library(treemap)

# Filter out N/A values in Churn.Category and Churn.Reason columns
churn_data <- subset(churn_data, !is.na(Churn.Category) & !is.na(Churn.Reason))

# Aggregate data by Churn Category and Churn Reason
agg_data <- aggregate(Customer.ID ~ Churn.Category + Churn.Reason, data = churn_data, FUN = length)

# Create treemap
treemap(agg_data,
        index=c("Churn.Category", "Churn.Reason"),
        vSize="Customer.ID",
        title="Churn Category vs Churn Reason",
        fontsize.title = 14,
        fontsize.labels = c(12, 10),
        fontfamily.labels = "Arial",
        align.labels = list(c("center", "center")),
        palette="RdYlBu")


str(telecom_customer_churn_df)


install.packages("corrplot")
library(corrplot)

# Select numeric columns
numeric_columns <- sapply(telecom_customer_churn_df, is.numeric)
numeric_df <- telecom_customer_churn_df[, numeric_columns]

# Compute correlation matrix
correlation_matrix <- cor(numeric_df)

# Plot correlation matrix as heatmap
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)



library(corrplot)

# Select numeric columns excluding Latitude and Longitude
numeric_columns <- sapply(telecom_customer_churn_df, is.numeric)
numeric_df <- telecom_customer_churn_df[, numeric_columns]
numeric_df <- numeric_df[, !(names(numeric_df) %in% c("Latitude", "Longitude"))]

# Compute correlation matrix
correlation_matrix <- cor(numeric_df)

# Plot correlation matrix as heatmap
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)

print(correlation_matrix)

### 4. handling cateGORICAL DATA 

# Load necessary libraries
library(readxl)
library(openxlsx)

# Read the Excel file
file_path <- "C:\\Users\\Dheeraj\\Downloads\\Studies\\Semester 3\\AIT 664\\Project\\DATASET\\cleaned_telecom_customer_churn.xlsx"
telecom_customer_churn_df <- read_excel(file_path)

# Assuming we need to identify categorical columns programmatically
# This is a simple heuristic: columns of type character or factor are treated as categorical
categorical_columns <- sapply(telecom_customer_churn_df, function(x) is.character(x) || is.factor(x))
categorical_column_names <- names(categorical_columns[categorical_columns])
print(categorical_column_names)

# Loop through each specified column and convert it to numeric codes
columns_to_encode <- c("Gender", "Married", "Offer", "Phone.Service", "Multiple.Lines", 
                       "Internet.Service", "Internet.Type", "Online.Security", "Online.Backup", 
                       "Device.Protection.Plan", "Premium.Tech.Support", "Streaming.TV", 
                       "Streaming.Movies", "Streaming.Music", "Unlimited.Data", "Contract", 
                       "Paperless.Billing", "Payment.Method", "Customer.Status", "Churn.Category", 
                       "Churn.Reason")

# Create a list to store mappings
mappings_list <- list()

for(col_name in columns_to_encode) {
  # Convert the column to a factor to extract levels
  column_factors <- factor(telecom_customer_churn_df[[col_name]])
  column_levels <- levels(column_factors)
  
  # Generate a sequence starting at 0 for each level
  column_codes <- 0:(length(column_levels) - 1)
  
  # Create a mapping of levels to numeric codes
  column_map <- setNames(column_codes, column_levels)
  
  # Store the corrected mapping in the list with column name as key
  mappings_list[[col_name]] <- column_map
  
  # Replace the categorical values with their numeric codes in the dataframe
  telecom_customer_churn_df[[col_name]] <- column_map[as.character(telecom_customer_churn_df[[col_name]])]
}

# Save mappings to a separate Excel file
mappings_wb <- createWorkbook()

for (col_name in names(mappings_list)) {
  # Create a new sheet for each column
  addWorksheet(mappings_wb, col_name)
  
  # Write header
  writeData(mappings_wb, sheet = col_name, x = c("Label", "Numeric Code"), startRow = 1, startCol = 1)
  
  # Write mappings
  writeData(mappings_wb, sheet = col_name, x = names(mappings_list[[col_name]]), startRow = 2, startCol = 1)
  writeData(mappings_wb, sheet = col_name, x = unname(as.vector(mappings_list[[col_name]])), startRow = 2, startCol = 2)
}

# Save the mappings workbook to a file
saveWorkbook(mappings_wb, "C:\\Users\\Dheeraj\\Downloads\\Studies\\Semester 3\\AIT 664\\Project\\DATASET\\encoded_data_mappings.xlsx", overwrite = TRUE)

# Save encoded data to another Excel file
write.xlsx(telecom_customer_churn_df, 
           "C:\\Users\\Dheeraj\\Downloads\\Studies\\Semester 3\\AIT 664\\Project\\DATASET\\encoded_data.xlsx", 
           sheetName = "Encoded_Data", 
           row.names = FALSE)
