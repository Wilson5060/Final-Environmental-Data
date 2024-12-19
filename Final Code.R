# install.packages(c("dplyr", "ggplot2", "lubridate", "patchwork", "tidyverse", "olsrr", "MASS"))
library(dplyr)
library(ggplot2)       
library(lubridate)
library(patchwork)
library(tidyverse)
library(olsrr)
library(MASS)

# Load datasets
California_weather <- read.csv("./South Cali_daily_1945.csv")
Carolina_weather <- read.csv("./South Carolina_daily_1937.csv")
California_storm <- read.csv("./South California Storm Event.csv")
Carolina_storm <- read.csv("./South Carolina Storm Event.csv")

#Data Cleaning For Weather Station#######################################################################################

# **1. Define Relevant Variables and Weather Types**
Relevant_variables <- list(
  "STATION" = "Station_ID",
  "NAME" = "Station_Name", "DATE" = "Date", "AWND" = "Average_Wind_Speed", "FMTM" = "Fastest_Mile_Time",
  "PRCP" = "Precipitation", "SNOW" = "Snowfall", "SNWD" = "Snow_Depth", "TAVG" = "Avg_Temperature",
  "TMAX" = "Max_Temperature", "TMIN" = "Min_Temperature", "WT01" = "Fog", "WT02" = "Heavy_Fog", "WT03" = "Thunder",
  "WT08" = "Smoke", "WT10" = "Tornado", "WT11" = "Damaging_Winds", "WT16" = "Rain","WT18" = "Snow")

# List of weather types (dummy variables)
Weather_type <- c("Fog", "Heavy_Fog", "Thunder", "Smoke", "Tornado", "Damaging_Winds", "Rain", "Snow")

############################################################################################

# **2. Define Cleaning Function**

# Function to clean and standardize weather data
clean_weather_data <- function(df, mapping, dummy_vars) {
  # Select and rename relevant columns
  df <- df[, colnames(df) %in% names(mapping), drop = FALSE]
  colnames(df) <- sapply(colnames(df), function(col) mapping[[col]])
  
  # Replace NA values with 0 for dummy variables
  df <- df %>%
    mutate(across(all_of(dummy_vars), ~ ifelse(is.na(.), 0, .)))
  return(df)}

California_weather <- clean_weather_data(California_weather, Relevant_variables, Weather_type)
Carolina_weather <- clean_weather_data(Carolina_weather, Relevant_variables, Weather_type)

########################################################################################

# **3. Identify Missing Data**

cat("Percentage of Missing Data (California):\n")
print(round((colSums(is.na(California_weather)) / nrow(California_weather)) * 100, 2))

cat("\nPercentage of Missing Data (South Carolina):\n")
print(round((colSums(is.na(Carolina_weather)) / nrow(Carolina_weather)) * 100, 2))

########################################################################################

# **4. Remove Columns with Excessive Missing Data**
# Drop columns with high percentages of missing data

California_weather <- California_weather %>%
  dplyr::select(-Average_Wind_Speed, -Fastest_Mile_Time, -Avg_Temperature)

Carolina_weather <- Carolina_weather %>%
  dplyr::select(-Average_Wind_Speed, -Fastest_Mile_Time, -Avg_Temperature)

########################################################################################

# **5. Analyze and Filter Infrequent Weather Types**
# Count occurrences of weather types in each data set
california_event_counts <- sapply(Weather_type, function(event) {
  sum(California_weather[[event]] == 1, na.rm = TRUE)})

carolina_event_counts <- sapply(Weather_type, function(event) {
  sum(Carolina_weather[[event]] == 1, na.rm = TRUE)})

# Combine and summarize weather type counts
event_counts_summary <- data.frame(
  California_Count = california_event_counts,
  Carolina_Count = carolina_event_counts)

cat("\nWeather Type Counts:\n")
print(event_counts_summary)

# Drop infrequent weather types
California_weather <- California_weather %>%
  dplyr::select(-Tornado, -Damaging_Winds, -Snow)

Carolina_weather <- Carolina_weather %>%
  dplyr::select(-Tornado, -Damaging_Winds, -Snow)

########################################################################################

# **6. Combine and Simplify Weather Types**
# Combine `Fog` and `Heavy_Fog` into a single variable (These are interchangeable in documentary)
California_weather <- California_weather %>%
  mutate(Fog = ifelse(Fog == 1 | Heavy_Fog == 1, 1, 0)) %>%
  dplyr::select(-Heavy_Fog)

Carolina_weather <- Carolina_weather %>%
  mutate(Fog = ifelse(Fog == 1 | Heavy_Fog == 1, 1, 0)) %>%
  dplyr::select(-Heavy_Fog)


summary(California_weather)
# About 29% of days had Fog, .08% Thunder, 37% smoke, 15.5% Rain
# Mean precipitation per day is .026 inches, mean max temp is 70 F, mean min temp is 57.4 F
summary(Carolina_weather)
# About 42.68% of days had Fog, 14.27% thunder, 25.91% smoke, 32.78% rain
# Mean precipitation per day is .14 inches, mean max temp is 76.2 F, mean min temp is 55.5 F

#Data Cleaning and Analysis For Storm Events#######################################################################################

# **1. Convert Dates**
# Ensure consistent date formats in all data sets
Carolina_weather$Date <- as.Date(Carolina_weather$Date)
California_weather$Date <- as.Date(California_weather$Date)
Carolina_weather$Year <- year(Carolina_weather$Date)
California_weather$Year <- year(California_weather$Date)
Carolina_storm$BEGIN_DATE <- as.Date(Carolina_storm$BEGIN_DATE, format = "%m/%d/%Y")
Carolina_storm$END_DATE <- as.Date(Carolina_storm$END_DATE, format = "%m/%d/%Y")
California_storm$BEGIN_DATE <- as.Date(California_storm$BEGIN_DATE, format = "%m/%d/%Y")
California_storm$END_DATE <- as.Date(California_storm$END_DATE, format = "%m/%d/%Y")

########################################################################################
# **2. Deduplicate Storm Events**
# Remove duplicate rows from the storm data sets based on key identifiers. 
# Storms are logged multiple times with minor variations in details such as begin time (e.g., 1300 vs 1450), while critical fields like begin date, end date, and event type remain identical. 
# If these duplicate rows are not removed, the analysis may overestimate the frequency of storms, leading to biased conclusions.

deduplicate_storms <- function(storm_data) {
  storm_data %>%
    dplyr::select(CZ_NAME_STR, BEGIN_DATE, EVENT_TYPE, END_DATE) %>%
    distinct()}

California_storm_unique <- deduplicate_storms(California_storm)
Carolina_storm_unique <- deduplicate_storms(Carolina_storm)

# deduplication summary
deduplication_summary <- data.frame(
  Location = c("California", "California", "Carolina", "Carolina"),
  Stage = c("Before Deduplication", "After Deduplication", "Before Deduplication", "After Deduplication"),
  Count = c(nrow(California_storm), nrow(California_storm_unique), 
            nrow(Carolina_storm), nrow(Carolina_storm_unique)))

deduplication_summary

########################################################################################
# **3. Flag Storm Events in Weather Data**
# Function to flag storm occurrences in weather data
flag_storms <- function(weather_data, storm_data) {
  weather_data <- weather_data %>%
    mutate(Storm_Flag = 0, Event_Type = NA) 
  
  for (i in seq_len(nrow(storm_data))) {
    weather_data <- weather_data %>%
      mutate(
        Storm_Flag = ifelse(Date >= storm_data$BEGIN_DATE[i] & Date <= storm_data$END_DATE[i], 1, Storm_Flag),
        Event_Type = ifelse(Date >= storm_data$BEGIN_DATE[i] & Date <= storm_data$END_DATE[i], storm_data$EVENT_TYPE[i], Event_Type))}
  return(weather_data)}

Carolina_weather <- flag_storms(Carolina_weather, Carolina_storm_unique)
California_weather <- flag_storms(California_weather, California_storm_unique)

########################################################################################

# **4. Analyze Storm Occurrences**
# Count the number of occurrences of each storm event type
count_storm_events <- function(storm_data) {
  storm_data %>%
    group_by(EVENT_TYPE) %>%
    summarize(Frequency = n(), .groups = "drop") %>%
    arrange(desc(Frequency))}

# Get storm event frequencies for both locations
california_storm_event_counts <- count_storm_events(California_storm_unique)
carolina_storm_event_counts <- count_storm_events(Carolina_storm_unique)

california_storm_event_counts
carolina_storm_event_counts

########################################################################################

# EDA
# **1. Define Reusable Functions**

# Summarize yearly total precipitation
summarize_precipitation <- function(weather_data, location_name) {weather_data %>%
    group_by(Year) %>%
    summarize(Total_Precipitation = sum(Precipitation, na.rm = TRUE)) %>%
    mutate(Location = location_name)}

# Summarize yearly average temperatures
summarize_temperature <- function(weather_data, location_name) {weather_data %>%
    group_by(Year) %>%
    summarize(
      Avg_Max_Temperature = mean(Max_Temperature, na.rm = TRUE),
      Avg_Min_Temperature = mean(Min_Temperature, na.rm = TRUE)) %>%
    mutate(Location = location_name)}

# Summarize yearly weather events
summarize_weather_events <- function(weather_data, weather_events) {weather_data %>%
    group_by(Year) %>%
    summarize(across(all_of(weather_events), ~ sum(. == 1, na.rm = TRUE)))}

########################################################################################
# **2. Yearly Total Precipitation EDA**

yearly_precipitation_Cali <- summarize_precipitation(California_weather, "Southern California")
yearly_precipitation_Carolina <- summarize_precipitation(Carolina_weather, "South Carolina")
combined_yearly_precipitation <- bind_rows(yearly_precipitation_Cali, yearly_precipitation_Carolina)

plot_precipitation <- ggplot(combined_yearly_precipitation, aes(x = as.numeric(Year), y = Total_Precipitation, color = Location)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Total Precipitation Comparison",
       x = "Year", y = "Total Precipitation (in)", color = "Location") +
  theme_minimal()

########################################################################################
# **3. Yearly Average Temperatures EDA**

yearly_avg_temperature_Cali <- summarize_temperature(California_weather, "Southern California")
yearly_avg_temperature_Carolina <- summarize_temperature(Carolina_weather, "South Carolina")

combined_yearly_avg_temperature <- bind_rows(yearly_avg_temperature_Cali, yearly_avg_temperature_Carolina) %>%
  pivot_longer(cols = c(Avg_Max_Temperature, Avg_Min_Temperature),
               names_to = "Temperature_Type",
               values_to = "Temperature")

plot_temperature <- ggplot(combined_yearly_avg_temperature, aes(x = as.numeric(Year), y = Temperature, 
                                                                color = Location, 
                                                                linetype = Temperature_Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Average Temperatures Comparison",
       x = "Year", y = "Temperature (°F)", color = "Location", linetype = "Temperature Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

########################################################################################
# **4. Yearly Weather Events EDA**

weather_events <- c("Fog", "Thunder", "Smoke", "Rain")

yearly_weather_events_Cali <- summarize_weather_events(California_weather, weather_events) %>%
  pivot_longer(-Year, names_to = "Weather_Type", values_to = "Count")

yearly_weather_events_Carolina <- summarize_weather_events(Carolina_weather, weather_events) %>%
  pivot_longer(-Year, names_to = "Weather_Type", values_to = "Count")

plot_weather_Cali <- ggplot(yearly_weather_events_Cali, aes(x = Year, y = Count, color = Weather_Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Fog" = "blue", "Rain" = "darkgreen", "Smoke" = "orange", "Thunder" = "purple")) +
  scale_x_continuous(limits = c(1945, 2020)) +  
  labs(title = "Yearly Weather Events in Southern California",
       x = "Year", y = "Occurrences", color = "Weather Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# South Carolina Weather Events
plot_weather_Carolina <- ggplot(yearly_weather_events_Carolina, aes(x = Year, y = Count, color = Weather_Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Fog" = "blue", "Rain" = "darkgreen", "Smoke" = "orange", "Thunder" = "purple")) +
  scale_x_continuous(limits = c(1945, 2020)) +  # Set x-axis limits
  labs(title = "Yearly Weather Events in South Carolina",
       x = "Year", y = "Occurrences", color = "Weather Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

#############################################################################################
# **5. Combine and Display Plots**

(plot_precipitation / plot_temperature) + plot_layout(guides = 'collect')

(plot_weather_Cali / plot_weather_Carolina) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

#Analysis of storm events#######################################################################################

# ** Step 1: Aggregate Storm Data **
# Aggregate storm metrics for California and Carolina
storm_severity_california <- California_weather %>%
  filter(Storm_Flag == 1) %>%
  group_by(Year = year(Date)) %>%
  summarize(
    Total_Storm_Precipitation = sum(Precipitation, na.rm = TRUE),
    Storm_Days = n(),
    Avg_Daily_Storm_Precipitation = mean(Precipitation, na.rm = TRUE),
    Avg_Min_Temperature = mean(Min_Temperature, na.rm = TRUE),
    Avg_Max_Temperature = mean(Max_Temperature, na.rm = TRUE))

storm_severity_carolina <- Carolina_weather %>%
  filter(Storm_Flag == 1) %>%
  group_by(Year = year(Date)) %>%
  summarize(
    Total_Storm_Precipitation = sum(Precipitation, na.rm = TRUE),
    Storm_Days = n(),
    Avg_Daily_Storm_Precipitation = mean(Precipitation, na.rm = TRUE),
    Avg_Min_Temperature = mean(Min_Temperature, na.rm = TRUE),
    Avg_Max_Temperature = mean(Max_Temperature, na.rm = TRUE))

# Combine storm metrics for both locations
storm_severity_combined <- bind_rows(
  storm_severity_california %>% mutate(Location = "Southern California"),
  storm_severity_carolina %>% mutate(Location = "South Carolina"))

########################################################################################

# ** Step 2: Filter for Years with Sufficient Storm Days 
threshold <- 5
storm_severity_combined <- storm_severity_combined %>%
  filter(Storm_Days >= threshold)

# Reshape filtered temperature data for plotting
storm_temperatures_filtered <- storm_severity_combined %>%
  dplyr::select(Year, Location, Storm_Days, Avg_Min_Temperature, Avg_Max_Temperature) %>%
  pivot_longer(cols = c(Avg_Min_Temperature, Avg_Max_Temperature),
               names_to = "Temperature_Type",
               values_to = "Temperature")

########################################################################################

# ** Step 3: Define Reusable Plotting Function **
plot_storm_metric <- function(data, y_variable, title, subtitle, y_label) {
  ggplot(data, aes(x = Year, y = !!sym(y_variable), color = Location)) +
    geom_line(size = 1) +
    geom_point(aes(fill = Storm_Days), shape = 21, size = 3, color = "black", alpha = 0.8) +
    scale_fill_gradient(
      name = "Number of Storm Occurence in a Year",
      low = "yellow", high = "red") +
    labs(title = title,
         subtitle = subtitle,
         x = "Year", y = y_label, color = "Location") +
    theme_minimal() +
    theme(legend.position = "bottom")}

########################################################################################

# ** Step 4: Generate Plots **
# Total Precipitation During Storms
plot1 <- plot_storm_metric(
  storm_severity_combined,
  y_var = "Total_Storm_Precipitation",
  title = "Total Precipitation During Storms Over Time",
  subtitle = "Line color represents location, dot color represents storm days count",
  y_label = "Total Storm Precipitation (in)")

# Average Max Temperatures During Storms
plot2 <- plot_storm_metric(
  storm_temperatures_filtered %>% filter(Temperature_Type == "Avg_Max_Temperature"),
  y_var = "Temperature",
  title = "Average Max Temperatures During Storms Over Time",
  subtitle = "Line color represents location, dot color represents storm days count",
  y_label = "Temperature (°F)")

# Average Min Temperatures During Storms
plot3 <- plot_storm_metric(
  storm_temperatures_filtered %>% filter(Temperature_Type == "Avg_Min_Temperature"),
  y_var = "Temperature",
  title = "Average Min Temperatures During Storms Over Time",
  subtitle = "Line color represents location, dot color represents storm days count",
  y_label = "Temperature (°F)")

combined_plot <- (plot1 / plot2 / plot3) + 
  plot_layout(guides = "collect") &       
  theme(legend.position = "bottom")     

combined_plot

########################################################################################
# Statistical Regression Analysis: Storm Events and Weather Occurrences
# Goal: Analyze relationships for:
# 1. Smoke Days and Temperature with Wildfire-Related Storms
# 2. Rain Days and Total Precipitation with Precipitation-Wind Related Storms
########################################################################################

# ** SECTION 1: Wildfire Storm Analysis for California **
# Goal: Assess the relationship between Smoke Days, Temperature, and Wildfire Storms.
# Hypothesis: More weather smoke days recorded and higher temperature yearly should be associated with more wildfire storms
# Analysis is conducted only on California given few documented wildfires event in Carolina Storm Database
########################################################################################

# Summarize yearly average temperature
temperature_california <- California_weather %>%
  group_by(Year) %>%
  summarize(Avg_Temperature = mean((Max_Temperature + Min_Temperature) / 2, na.rm = TRUE), .groups = "drop")

# Summarize yearly wildfire storm occurrence
wildfire_storms_california <- California_storm_unique %>%
  filter(grepl("Wildfire", EVENT_TYPE, ignore.case = TRUE)) %>%
  group_by(Year = year(BEGIN_DATE)) %>%
  summarize(Wildfire_Storm_Count = n(), .groups = "drop")

# Summarize Yearly Smoke Days
smoke_days_california <- California_weather %>%
  group_by(Year) %>%
  summarize(Smoke_Days = sum(Smoke, na.rm = TRUE), .groups = "drop")

# Combine All Relevant Dataset: wildfire storms, smoke days, and temperature, and precipitation
california_combined_data <- merge(wildfire_storms_california, smoke_days_california, by = "Year", all.x = TRUE)
california_combined_data <- merge(california_combined_data, yearly_precipitation_Cali, by = "Year", all.x = TRUE) 
california_combined_data <- merge(california_combined_data, temperature_california, by = "Year", all.x = TRUE) %>%
  filter(Smoke_Days > 0)

# Multiple Linear Regression Model Analysis
california_model <- lm(Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation, data = california_combined_data)
summary(california_model)

# Visualize relationships
plot_california_smoke_days <- ggplot(california_combined_data, aes(x = Smoke_Days, y = Wildfire_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Effect of Smoke Days on Wildfire Storm Count (San Diego", x = "Smoke Days", y = "Wildfire Storm Count") +
  theme_minimal()

plot_california_avg_temp <- ggplot(california_combined_data, aes(x = Avg_Temperature, y = Wildfire_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Effect of Avg Temperature on Wildfire Storm Count (San Diego) ", x = "Average Temperature (°F)", y = "Wildfire Storm Count") +
  theme_minimal()

plot_california_total_precip <- ggplot(california_combined_data, aes(x = Total_Precipitation, y = Wildfire_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Effect of Total Precipitation on Wildfire Storm Count (San Diego) ", x = "Total Precipitation (in)", y = "Wildfire Storm Count") +
  theme_minimal()

(plot_california_smoke_days/plot_california_avg_temp/plot_california_total_precip) + plot_layout(guides= "collect")

########################################################################################
# ** Assumption Checking for Wildfire Models **
########################################################################################

# 1. Residuals and Fit
wildfire_res.full <- rstandard(california_model)
wildfire_fit.full <- fitted.values(california_model)
plot(wildfire_fit.full, wildfire_res.full, pch=19, col="grey50")
abline(h=0)
# Appears to have slight heteroscedasticity but good centering around zero

# 2. Normality Check
qqnorm(wildfire_res.full, pch=19, col="grey50")
qqline(wildfire_res.full)
# some deviation from normality in the tails suggesting outliers but fairly along the line

shapiro.test(wildfire_res.full) # P > 0.05 --> Test do not appear to be significantly deviate from normal

# 3. Model Selection
# Step-wise AIC model selection
best_model <- step(california_model, direction = "both", trace = TRUE)
# Indicates that smoke_days should be removed however it only lowers the AIC by 2

# 4. Poisson GLM for Count Data (Since Wildfire_storm_count is count data, a Poisson GLM is a more appropriate model vs log)

california_model_glm <- glm(
  Wildfire_Storm_Count ~ Smoke_Days + Avg_Temperature + Total_Precipitation,
  data = california_combined_data,
  family = poisson)
summary(california_model_glm)

# 4. Visualize Poisson GLM Predictions
california_combined_data$Predicted_Count <- predict(california_model_glm)
glm_residuals <- residuals(california_model_glm)
shapiro.test(glm_residuals) # W- increases slightly

ggplot(california_combined_data, aes(x = Smoke_Days, y = Wildfire_Storm_Count)) +
  geom_point(color = "blue") +
  geom_line(aes(y = Predicted_Count), color = "red", size = 1) +
  labs(title = "Poisson GLM: Smoke Days vs Wildfire Storm Count (California)",
       x = "Number of Smoke Days", y = "Number of Wildfire-Related Storms") +
  theme_minimal()

########################################################################################
# ** SECTION 2: Precipitation-Wind Storm Analysis for Both Regions **
# Goal: Analyze Rain Days and Total Precipitation with Precipitation-Wind Storms
# Hypothesis: increase in rain days and precipitation yearly is associated with more wind-heavy related storms
########################################################################################

# 1: Define reusable functions and events of interest 

precipitation_related_events <- c("Heavy Rain", "High Wind", "Thunderstorm Wind", "Strong Wind")

# Function: Summarize precipitation-related storms by year
summarize_precipitation_storms <- function(storm_data, events) {
  storm_data %>%
    filter(EVENT_TYPE %in% events) %>%
    group_by(Year = year(BEGIN_DATE)) %>%
    summarize(Precipitation_Storm_Count = n(), .groups = "drop")}

# Function: Summarize yearly rain days from weather data
summarize_rain_days <- function(weather_data) {
  weather_data %>%
    group_by(Year) %>%
    summarize(Rain_Days = sum(Rain, na.rm = TRUE), .groups = "drop")}

########################################################################################

# 2: Process Data for California and South Carolina

# California
precipitation_storms_california <- summarize_precipitation_storms(California_storm_unique, precipitation_related_events)
rain_days_california <- summarize_rain_days(California_weather)

# South Carolina
precipitation_storms_carolina <- summarize_precipitation_storms(Carolina_storm_unique, precipitation_related_events)
rain_days_carolina <- summarize_rain_days(Carolina_weather)

########################################################################################

# 3: Merge Data and Clean Years
# Cleaning: Remove years after 2012 as weather station data appears to stop capturing rain-related activities midway in 2013

california_precipitation_data <- merge(precipitation_storms_california, rain_days_california, by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Cali, by = "Year", all.x = TRUE) %>%
  filter(Rain_Days > 0, Year <= 2012, Precipitation_Storm_Count > 2)
# Include additional filter Precipitation_Storm_Count > 3 as California's precipitation storm generally exhibit higher values (typically above 10)
# Lower values observed in earlier years likely reflect incomplete data collection, rather than an actual lack of storms.

carolina_precipitation_data <- merge(precipitation_storms_carolina, rain_days_carolina, by = "Year", all.x = TRUE) %>%
  merge(yearly_precipitation_Carolina, by = "Year", all.x = TRUE) %>%
  filter(Rain_Days > 0, Year <= 2012) 

########################################################################################
# 4: Multi-Linear Regression Analysis 

california_precipitation_model <- lm(Precipitation_Storm_Count ~ Rain_Days + Total_Precipitation, data = california_precipitation_data)
summary(california_precipitation_model)

carolina_precipitation_model <- lm(Precipitation_Storm_Count ~ Rain_Days + Total_Precipitation, data = carolina_precipitation_data)
summary(carolina_precipitation_model)

########################################################################################
# 5: Visualization for Both Regions

plot_california_rain_days <- ggplot(california_precipitation_data, aes(x = Rain_Days, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Rain Days and Precipitation-Related Storms (California)",
       x = "Number of Rain Days",
       y = "Precipitation Storm Count") +
  theme_minimal()

plot_california_precip <- ggplot(california_precipitation_data, aes(x = Total_Precipitation, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Precipitation and Precipitation-Related Storms (California)",
       x = "Total Precipitation Yearly (in)",
       y = "Precipitation Storm Count") +
  theme_minimal()

plot_carolina_rain_days <- ggplot(carolina_precipitation_data, aes(x = Rain_Days, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Rain Days and Precipitation-Related Storms (South Carolina)",
       x = "Number of Rain Days",
       y = "Precipitation Storm Count") +
  theme_minimal()

plot_carolina_precip <- ggplot(carolina_precipitation_data, aes(x = Total_Precipitation, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Precipitation and Precipitation-Related Storms (South Carolina)",
       x = "Total Precipitation Yearly (in)",
       y = "Precipitation Storm Count") +
  theme_minimal()

(plot_california_rain_days / plot_carolina_rain_days/ plot_california_precip / plot_carolina_precip) + plot_layout(guides = "collect")

########################################################################################
# ** Checking Model Assumptions for Precipitation-Related Storms **
########################################################################################

# ** California Linear Regression Assumptions Check **

# 1. Extract Residuals and Fitted Values
Cali_precip_res.full <- rstandard(california_precipitation_model)
Cali_precip_fit.full <- fitted.values(california_precipitation_model)

# 2. Plot Residuals vs Fitted Values
plot(Cali_precip_fit.full, Cali_precip_res.full, pch = 19, col = "grey50",
     main = "Residuals vs Fitted Values (California)")
abline(h = 0, col = "red") # appears to be random

# 3. QQ Plot for Normality Check
qqnorm(Cali_precip_res.full, pch = 19, col = "grey50", 
       main = "QQ Plot of Residuals (California)")
qqline(Cali_precip_res.full, col = "red") # slight deviations along tails

# 4. Shapiro-Wilk Test for Normality
shapiro.test(Cali_precip_res.full) # California is normally distributed

########################################################################################

# ** South Carolina Linear Regression Assumptions Check **

# 1. Extract Residuals and Fitted Values
Carolina_precip_res.full <- rstandard(carolina_precipitation_model)
Carolina_precip_fit.full <- fitted.values(carolina_precipitation_model)
# Some outliers --> adjustment required

# 2. Plot Residuals vs Fitted Values
plot(Carolina_precip_fit.full, Carolina_precip_res.full, pch = 19, col = "grey50",
     main = "Residuals vs Fitted Values (South Carolina)")
abline(h = 0, col = "red")

# 3. QQ Plot for Normality Check
qqnorm(Carolina_precip_res.full, pch = 19, col = "grey50", 
       main = "QQ Plot of Residuals (South Carolina)")
qqline(Carolina_precip_res.full, col = "red") 
# Deviation along tails

# 4. Shapiro Test for Normality
shapiro.test(Carolina_precip_res.full) # P-value = 0, not normally distributed

########################################################################################
# ** Negative Binomial GLM for South Carolina to Handle Overdispersion **
########################################################################################

# 1. Fit Negative Binomial Model
carolina_precipitation_model_nb <- glm.nb(Precipitation_Storm_Count ~ Total_Precipitation + Rain_Days, 
                                          data = carolina_precipitation_data)
summary(carolina_precipitation_model_nb)

# 2. Extract Residuals and Fitted Values
Carolina_precip_res.nb <- rstandard(carolina_precipitation_model_nb)
Carolina_precip_fit.nb <- fitted.values(carolina_precipitation_model_nb)

# 3. Plot Residuals vs Fitted Values
plot(Carolina_precip_fit.nb, Carolina_precip_res.nb, pch = 19, col = "grey50",
     main = "Residuals vs Fitted Values (Negative Binomial, SC)")
abline(h = 0, col = "red")

# 4. QQ Plot for Normality Check
qqnorm(Carolina_precip_res.nb, pch = 19, col = "grey50", 
       main = "QQ Plot of Residuals (Negative Binomial, SC)")
qqline(Carolina_precip_res.nb, col = "red")

# 5. Shapiro Test for Normality
shapiro.test(Carolina_precip_res.nb) # slight improvement but not normal still

# 6. Visualize Predicted vs Observed Results
carolina_precipitation_data$Predicted_NB <- predict(carolina_precipitation_model_nb, type = "response")

# Total Precipitation vs Storm Count
ggplot(carolina_precipitation_data, aes(x = Total_Precipitation, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_line(aes(y = Predicted_NB), color = "red", linewidth = 1) +
  labs(title = "Negative Binomial: Total Precipitation vs Storm Count (SC)",
       x = "Total Precipitation", y = "Observed and Predicted Storm Count") +
  theme_minimal()

# Rain Days over Storm Counts
ggplot(carolina_precipitation_data, aes(x = Rain_Days, y = Precipitation_Storm_Count)) +
  geom_point(color = "blue") +
  geom_line(aes(y = Predicted_NB), color = "red", linewidth = 1) +  
  labs(title = "Effect of Total Precipitation on Precipitation Storm Count (Negative Binomial)",
       x = "Rain Days",
       y = "Observed and Predicted Storm Count") +
  theme_minimal()

########################################################################################
# ** Log Transformation for South Carolina to Address Skewness **
########################################################################################

# 1. Log Transformation of Variables
carolina_precipitation_data <- carolina_precipitation_data %>%
  mutate(Log_Total_Precipitation = log(Total_Precipitation),
         Log_Rain_Days = log(Rain_Days))

# 2. Fit Linear Model with Transformed Variables
carolina_precipitation_model_log <- lm(Precipitation_Storm_Count ~ Log_Total_Precipitation + Log_Rain_Days, 
                                       data = carolina_precipitation_data)
summary(carolina_precipitation_model_log)

# 3. Extract Residuals and Fitted Values
Carolina_precip_res.log <- rstandard(carolina_precipitation_model_log)
Carolina_precip_fit.log <- fitted.values(carolina_precipitation_model_log)

# 4. Plot Residuals vs Fitted Values
plot(Carolina_precip_fit.log, Carolina_precip_res.log, pch = 19, col = "grey50",
     main = "Residuals vs Fitted Values (Log Transformation, SC)")
abline(h = 0, col = "red")

# 5. QQ Plot for Normality Check
qqnorm(Carolina_precip_res.log, pch = 19, col = "grey50", 
       main = "QQ Plot of Residuals (Log Transformation, SC)")
qqline(Carolina_precip_res.log, col = "red")

# 6. Shapiro Test for Normality
shapiro.test(Carolina_precip_res.log) # Not normal

########################################################################################
