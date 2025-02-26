library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)


data <- read_csv("supermarket_sales - Sheet1.csv")

sum(is.na(data))
summary(data)

ggcorrplot(cor_matrix, lab = TRUE)


# Converting time into hours so it is accessible
data <- data %>%
  mutate(Hours = hour(hms::as_hms(Time)) + minute(hms::as_hms(Time)) / 60)


model <- lm(Total ~ Quantity + `Unit price` + Rating + Hours, data = data)
summary(model)

### Hours relationship with the 'Total' value is not explained so I will create
### a graph to show AVG sales by hour of day. I wi

time_analysis <- data %>%
  group_by(Hours = floor(Hours)) %>%
  summarize(Average_Total = mean(Total))

ggplot(time_analysis, aes(x = Hours, y = Average_Total)) +
  geom_line() +
  labs(title = "AVG sales by Hour of Day", x = "Hour of Day", y = "Average Total Sales")

# Rating analysis
rating_analysis <- lm(Total ~ Rating, data = data)
summary(rating_analysis)

### Predict total sales for a given set of conditions
### Create new data for prediction with correct column names
new_data <- data.frame(Quantity = 10, `Unit price` = 50, Rating = 8, Hours = 14)
summary(new_data)

