#Loading the libraries
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(skimr)
library(binsreg)
library(splines)
library(sf)
library(estimatr)
library(fixest)

#loads the data
clean_data = read_csv("clean_data.csv")

#Displays statistical summary of the data
skim(clean_data)

#Exploratory Analysis
# Create a histogram of the 'VALUE' column in the 'clean_data' data 
ggplot(clean_data,aes(x=VALUE))+ 
  geom_histogram(boundary=0,bindwith = 2)

# Create a histogram of the 'NO_OF_INDIVIDUALS(INFLOW)' column in the 'clean_data' data frame
ggplot(clean_data,aes(x=`NO_OF_INDIVIDUALS(INFLOW)`))+ 
  geom_histogram(boundary=0,bindwith = 2)

#Applying log transformation to variables because they are right skewed.Applying the log will reduce the skewness, 
#compressing the larger values and spreading out the smaller ones. This will better reflect the central tendency.
# Apply log transformation to VALUE and NO_OF_INDIVIDUALS(INFLOW) to create new log-transformed variables
clean_data$LOG_VALUE = log(clean_data$VALUE + 1)
clean_data$LOG_MIGRATION = log(clean_data$`NO_OF_INDIVIDUALS(INFLOW)` + 1)

# Create a histogram of the 'LOG_VALUE' column in the 'clean_data' data frame
ggplot(clean_data,aes(x=LOG_VALUE))+ 
  geom_histogram(boundary=0,bindwith = 0.5)

# Create a histogram of the 'LOG_MIGRATION' column in the 'clean_data' data frame
ggplot(clean_data,aes(x=LOG_MIGRATION))+ 
  geom_histogram(boundary=0,bindwith = 0.5)

#Finding the relationship between migration and housing prices
# Create a scatter plot of LOG_MIGRATION vs. LOG_VALUE with a smoothing line
ggplot(clean_data, aes(x=LOG_MIGRATION, y =LOG_VALUE)) +
  # Add points with low transparent  
  geom_point(alpha = 0.1)+
  # Add a smooth line to show trend  
  geom_smooth()

#Using bin regression
#Generate a binned scatter plot of log-transformed VALUE and log-transformed NO_OF_INDIVIDUALS(INFLOW)
binsreg(log(clean_data$VALUE),log(clean_data$`NO_OF_INDIVIDUALS(INFLOW)`))

#Using local regression
# Create a scatter plot with a smooth trend line using ggplot with clean_data data set, mapping 'NO_OF_INDIVIDUAL(INFLOW)' to x-axis and 'VALUE' to y-axis
ggplot(clean_data,aes(x=LOG_MIGRATION,y= LOG_VALUE)) +
  geom_point(alpha=0.3, size=1.5)+
  # Add a smooth line using LOESS smoothing with linear fitting  
  geom_smooth(method = "loess")

#Creating a bin scatter
#  Create 50 bins based on the 'NO_OF_INDIVIDUALS(INFLOW)' variable and calculate binned means
clean_data =clean_data|>
  mutate(INDIVIDUAL_bins=factor(ntile(LOG_MIGRATION, 50)))

# Group the data by the created bins and calculate the average 'NO_OF_INDIVIDUALS' and 'VALUE' within each bin
binned_data = clean_data |>
  group_by(INDIVIDUAL_bins) |>
  # Calculate the mean 'VALUE' for each bin, ignoring missing values
  mutate(VALUE_binned = mean(LOG_VALUE,na.rm = T),
         # Calculate the mean 'NO_OF_INDIVIDUALS' for each bin, ignoring missing values
         INDIVIDUALS_binned = mean(LOG_MIGRATION, na.rm = T))

#creating the graph
# Create a scatter plot with the clean_data and binned data using 
ggplot
ggplot(binned_data) +
  geom_point(aes(x = LOG_MIGRATION, y = LOG_VALUE, color = INDIVIDUAL_bins), alpha = 0.3) +
  geom_point(aes(x = INDIVIDUALS_binned, y = VALUE_binned), size = 3, color = "navy") +  # Binned Averages
  labs(x = "Number of People inflow", y = "Average House Prices") +
  theme_minimal()

#Creating a local regression using the bins
# Create a smooth trend line plot using ggplot2
ggplot(binned_data,aes(x=LOG_MIGRATION, y= LOG_VALUE))+
  # Add a smooth trend line to visualize the overall relationship between NO_OF_INDIVIDUALS and VALUE  
  geom_smooth()


#Trends of migration
# Plot LOG_MIGRATION over YEAR for each county, with points colored by county name
ggplot(clean_data, aes(x = YEAR, y = LOG_MIGRATION, 
                       # Define grouping by county 
                       group = Y1_COUNTYNAME.x, 
                       # Set color by county name to differentiate points by county
                       color = Y1_COUNTYNAME.x)) +
  #Add points to represent LOG_MIGRATION at each YEAR
  geom_point() +
  labs(title = "Trend of NO_OF_INDIVIDUALS(INFLOW) by Year", x = "YEAR", y = "NO_OF_INDIVIDUALS(INFLOW)") +
  theme(legend.position = "none")

#Trends of the Value (housing prices)
# Plot LOG_VALUE over YEAR for each county, with points colored by county name
ggplot(clean_data, aes(x = YEAR, y = LOG_VALUE, 
                       # Define grouping by county                     
                       group = Y1_COUNTYNAME.x, 
                       # Set color by county name to differentiate points by county
                       color = Y1_COUNTYNAME.x)) +
  #Add points to represent LOG_MIGRATION at each YEAR
  geom_point() +
  labs(title = "Trend of VALUE by Year", x = "YEAR", y = "VALUE") +
  theme(legend.position = "none")

#Exploratory at the county 
#Box plot for counties with the highest mean value
# Find the top 10 counties with the highest mean of housing prices
top_counties = clean_data |>
  group_by(Y1_COUNTYNAME.x)|># Group data by county name
  summarise(mean_value = mean(LOG_VALUE, na.rm = T))|> # # Calculate the mean of housing prices for each county, excluding NA values
  arrange(desc(mean_value))|> # Arrange counties in descending order of mean_value
  # Select the top 10 counties with the highest mean_value  
  top_n(10,mean_value)

# Filters the original data to only include top counties
filtered_counties= clean_data|>
  filter(Y1_COUNTYNAME.x %in% top_counties$Y1_COUNTYNAME.x)

# Box plot for top counties 
ggplot(filtered_counties, aes(x = reorder(Y1_COUNTYNAME.x, -LOG_VALUE), y = LOG_VALUE)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box Plot of VALUE for Top 10 Counties", x = "County", y = "VALUE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Box plot for counties with the highest mean migration
# Find the top 10 counties with the highest mean of inflow (migration)
top2_counties = clean_data |>
  group_by(Y1_COUNTYNAME.x)|># Group data by county name
  summarise(mean_value = mean(LOG_MIGRATION, na.rm = T))|> # Calculate the mean of migration (inflow) for each county, excluding NA values
  arrange(desc(mean_value))|> # Arrange counties in descending order of mean_value
  #Select the top 10 counties with the highest mean_value  
  top_n(10,mean_value)

# Filters the original data to only include top counties
filter_counties= clean_data|>
  filter(Y1_COUNTYNAME.x %in% top2_counties$Y1_COUNTYNAME.x)

# Box plot for top counties 
ggplot(filter_counties, aes(x = reorder(Y1_COUNTYNAME.x, -LOG_MIGRATION), y = LOG_MIGRATION)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box Plot of Individual Inflow for Top 10 Counties", x = "County", y = "Number of Individual Inflow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Exploratory Analysis at the state
#Scatter plot of mean value by state
#Group the data by state and calculates the mean of VALUE
state_summary = clean_data|>
  group_by(STATEPOSTAL)|>
  summarise(mean_value = mean(LOG_VALUE,na.rm = T))

#Plot the box plot
ggplot(state_summary, aes(x = reorder(STATEPOSTAL, -mean_value), y= mean_value))+
  geom_point(fill = "lightblue", size = 3) +
  labs(title = "Scatter Plot of Average VALUE by State", x = "State", y = "VALUE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculates mean LOG_VALUE(Housing prices) and mean LOG_MIGRATION(Migration) for each state
state_summary2 = clean_data |>
  # Group data by state postal cod  
  group_by(STATEPOSTAL) |>
  summarise(
    mean_value = mean(LOG_VALUE, na.rm = TRUE),# Calculate mean of LOG_VALUE for each state
    mean_inflow = mean(LOG_MIGRATION, na.rm = TRUE) # Calculate mean of LOG_MIGRATION for each state
  )

# Filter for the top 10 states by mean VALUE and mean inflow
top_value_states = state_summary2 |>
  # Arrange states in descending order of mean_value  
  arrange(desc(mean_value)) |>
  # Select the first 10 rows (top 10 states)  
  slice(1:10)

top_inflow_states = state_summary2 |>
  # Arrange states in descending order of mean_inflow  
  arrange(desc(mean_inflow)) |>
  # Select the first 10 rows (top 10 states)  
  slice(1:10)

# Filter data to include only the top 10 states based on mean inflow
s1 = clean_data|>
  # Selects rows for states in top_inflow_states  
  filter(STATEPOSTAL %in% top_inflow_states$STATEPOSTAL)

# Create a box plot of LOG_MIGRATION for the top 10 states by mean inflow
ggplot(s1, aes(x = reorder(STATEPOSTAL, -LOG_MIGRATION), y = LOG_MIGRATION)) +
  # Adds a boxplot with light green fill  
  geom_boxplot(fill = "lightgreen") +
  #sets plot tltle  
  labs(title = "Top 10 States by Inflow Distribution", x = "State", y = "MIGRATION") +
  # Applies a minimal theme for a clean look  
  theme_minimal() +
  # Rotates x-axis text for better readability  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter data to include only the top 10 states based on mean value
s2 = clean_data|>
  # Selects rows for states in top_inflow_states  
  filter(STATEPOSTAL %in% top_value_states$STATEPOSTAL)

# Create a box plot of LOG_MIGRATION for the top 10 states by mean value
ggplot(s2, aes(x = reorder(STATEPOSTAL, -LOG_VALUE), y = LOG_VALUE)) +
  # Adds a boxplot with light green fill  
  geom_boxplot(fill = "lightgreen") +
  # Adds plot title  
  labs(title = "Top 10 States by VALUE Distribution", x = "State", y = "VALUE") +
  # Applies a minimal theme for a clean look  
  theme_minimal() +
  # Rotates x-axis text for better readability  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Finding the relationship between housing prices and migration using polynomial regression
# Fit a 4th degree polynomial regression model of VALUE on NO_OF_INDIVIDUALS(INFLOW)
Poly_reg = feols(VALUE ~ poly(`NO_OF_INDIVIDUALS(INFLOW)`, 4), data = clean_data)

# Display the summary of the regression model
summary(Poly_reg)

# predict to get fitted values
poly_predict = predict(Poly_reg,data = clean_data,interval = "confidence")
# Combine the original data with the predicted values and confidence intervals
poly_fitted = cbind(clean_data,poly_predict)


# Create a plot to visualize the polynomial regression fit and confidence intervals
ggplot(poly_fitted, aes(x =`NO_OF_INDIVIDUALS(INFLOW)`))+ theme() +
  geom_point(aes(y=VALUE),alpha = 0.1)+  # Scatter plot of actual data 
  # Confidence interval ribbon around the fitted line
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "coral") +
  # Fitted line from the polynomial regression
  geom_line(aes(y = fit),size = 1,color = "orange") + 
  labs(x = "Number of Immigrants", y = "Housing Prices", title = "Forth-Order Polynomial Regression") +
  theme_minimal()


#Finding the relationship between housing prices and migration using Bin regression
# Perform a binned regression using the feols function
binned_reg = feols(LOG_VALUE~as_factor(ntile(LOG_MIGRATION,10)), data = clean_data)
# Display the summary of the regression model
summary(binned_reg)

# Predict values using the binned regression model and add confidence intervals
binned_reg_predict=predict(binned_reg,data=clean_data,interval = "confidence")
# Combine the original data with the predicted values and confidence intervals
binned_reg_fitted= cbind(clean_data,binned_reg_predict)

# Create a plot to visualize the binned regression fit and confidence intervals
ggplot(binned_reg_fitted,aes(x=LOG_MIGRATION))+ theme_light() +
  # Scatter plot of actual data points with low opacity
  geom_point(aes(y = LOG_VALUE), alpha = 0.1) +
  # Confidence interval ribbon around the fitted line
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "red") +
  # Fitted line from the binned regression model
  geom_line(aes(y = fit), size = 1, color = "lightgreen")

#Finding the relationship between housing prices and migration using cubic splines
# Perform a piece wise cubic regression using B-splines
pw_cubic=feols(VALUE~ splines::bs(`NO_OF_INDIVIDUALS(INFLOW)`,df=5),
               clean_data)
# Display the summary of the regression model
summary(pw_cubic)


# Generate fitted values for cubic splines using predict
pw_cubic_predict = predict(pw_cubic,data=clean_data, interval = "confidence")
pw_cubic_fitted=cbind(clean_data,pw_cubic_predict)

# Create a plot to visualize the piece wise cubic regression fit and confidence intervals
ggplot(pw_cubic_fitted, aes(x = `NO_OF_INDIVIDUALS(INFLOW)`)) + theme_light()+
  # Scatter plot of original data points 
  geom_point(aes(y=VALUE),alpha = 0.1)+ 
  # Confidence interval ribbon around the fitted line
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "coral", alpha=2)+
  # Fitted line from the piece wise cubic regression model
  geom_line(aes(y =fit), size = 1,color = "green")+ 
  labs(x = "Number of Immigrants", y = "Housing Prices", title = "Cubic Splines Regression (5 Degrees of Freedom)") +
  theme_minimal()

#Using density plots
# Reshape data to a long format for easier plotting of multiple variables
clean_data_long <- clean_data |>
  pivot_longer(
    cols = c(LOG_MIGRATION, LOG_VALUE),# Select columns to pivot into long format
    names_to = "Variable", # Name the new column that will hold the variables names
    values_to = "Log_Value" # Name the new column that will hold the values of the variables
  )

# Plot density distributions for LOG_MIGRATION (Migration) and LOG_VALUE (Housing prices)
ggplot(clean_data_long, aes(x = Log_Value, fill = Variable)) +
  # Density plot with transparency and smoother curve  
  geom_density(alpha = 0.4, adjust = 1.2) +
  #Adds plot title  
  labs(title = "Density Plot of LOG_MIGRATION and LOG_VALUE",
       x = "Log Values",
       y = "Density") +
  # Apply a minimal theme for a clear visualization  
  theme_minimal() +
  # Separate panels for each variable, with independent scales  
  facet_wrap(~ Variable, scales = "free")  # Separate panels for each variable


