library(readr)
library(dplyr)
library(lubridate)

data <- read_csv("supermarket_sales - Sheet1.csv")

data <- data %>%
  mutate(Hours = hour(hms::as_hms(Time)) + minute(hms::as_hms(Time)) / 60)


summary(data)

selected_data <- data %>% select(Total, Quantity, `Unit price` , Rating, Hours)
selected_data <- na.omit(selected_data)

model <- lm(Total ~  Quantity  + `Unit price` + Rating + Hours, data = selected_data)
summary(model)

#Model Predictions
predictions <- predict(model, newdata = selected_data)

#Formatting
selected_data <- selected_data %>% mutate(Predicted_Total = predictions)

head(selected_data)

