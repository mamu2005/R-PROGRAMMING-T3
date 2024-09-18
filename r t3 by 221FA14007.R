# Load necessary libraries
library(ggplot2)

# Assuming your data is in a CSV file named "train_revised.csv"
data <- read.csv("C:/Users/91868/Downloads/train_revised.csv", stringsAsFactors = FALSE)

# Convert travel_date to Date format and calculate days since the first date
data$travel_date <- as.Date(data$travel_date, format = "%d-%m-%y")
data$date_num <- as.numeric(difftime(data$travel_date, min(data$travel_date), units = "days"))

# Convert travel_time to a usable format (e.g., minutes)
data$travel_time <- strptime(data$travel_time, format = "%H:%M")
data$time_in_minutes <- data$travel_time$hour * 60 + data$travel_time$min

# Fit a linear model
model <- lm(time_in_minutes ~ date_num, data = data)

# Calculate predictions from the model
predictions <- predict(model, data)

# Calculate Mean Squared Error (MSE)
mse <- mean((data$time_in_minutes - predictions)^2)
print(paste("Mean Squared Error:", mse))

# Plot the data points and regression line using ggplot
ggplot(data, aes(x = date_num, y = time_in_minutes)) +
  geom_point(color = "blue") +                            # Plot data points in blue
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Minimum error line in red
  labs(x = "Travel Date (Days since first date)", 
       y = "Travel Time (Minutes)", 
       title = "Linear Regression: Travel Date vs Travel Time")  
#travel date vs travel time





# Load necessary libraries
library(ggplot2)
data <- read.csv("C:/Users/91868/Downloads/train_revised.csv", stringsAsFactors = FALSE)

# Extract the relevant columns
X <- as.numeric(df$ride_id)

# Convert travel_time to minutes
df$travel_time <- as.numeric(as.POSIXlt(df$travel_time, format="%H:%M")$hour * 60 + as.POSIXlt(df$travel_time, format="%H:%M")$min)
y <- df$travel_time

# Create a linear regression model
model <- lm(y ~ X)

# Predict the values
y_pred <- predict(model, data.frame(X=X))

# Plot the data and the regression line
ggplot(df, aes(x = X, y = y)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = y_pred), color = 'red') +
  labs(x = 'ride_id', y = 'travel_time', title = 'Linear Regression of ride_id vs travel_time') +
  theme_minimal()
#ride id vs travel time




# Load necessary libraries
library(ggplot2)
data <- read.csv("C:/Users/91868/Downloads/train_revised.csv", stringsAsFactors = FALSE)
# Assuming df is your data frame

# Convert travel_date to numerical values (e.g., using ordinal dates)
df$travel_date_ordinal <- as.numeric(as.Date(df$travel_date, format="%Y-%m-%d"))

# Extract the relevant columns
X <- df$travel_date_ordinal
y <- df$ride_id

# Create a linear regression model
model <- lm(y ~ X)

# Predict the values
y_pred <- predict(model, data.frame(X = X))

# Plot the data and the regression line
ggplot(df, aes(x = X, y = y)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = y_pred), color = 'red') +
  labs(x = 'travel_date_ordinal', y = 'ride_id', title = 'Linear Regression of travel_date vs ride_id') +
  theme_minimal()

#RIDE ID TRAVEL DATE
