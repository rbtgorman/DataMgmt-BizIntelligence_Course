
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, dplyr, skimr, ggplot2, tidyr)

data <- read.csv("supermarket_sales - Sheet1.csv")

# 2. Preliminary descriptive statistical analysis ##############################
### Conduct some preliminary descriptive statistical analysis for your project data.
### Select at least 2 numeric variables of interest, report the results of descriptive statistics using summary function.  
### Select at least 2 categorical (or group) variables of interest, report frequency table and contingency table.

summary(data)

## Numeric variables ###########################################################
two_num_vars <- data[, c("cogs", "gross.income")]
summary(two_num_vars)

## Categorical variables #######################################################
table(data$Gender)
table(data$City)


table(data$Gender, data$City)

# 3. Develop box plots, histograms and frequency tables/charts #################
### Develop box plots, histograms and frequency tables/charts using the plotting features of R. Comment on any interesting finding.

## Box plots ###################################################################
#Box plot --- COGS
ggplot(data, aes(x = City, y = cogs, fill = City)) +
  geom_boxplot() +
  labs(title = "Box Plot of COGS by City", x = "City", y = "COGS")

#Box plot --- Gross Income
ggplot(data, aes(x = City, y = gross.income, fill = City)) +
  geom_boxplot() +
  labs(title = "Box Plot of Gross Income by City", x = "City", y = "Gross Income")

## Histograms ##################################################################
#Histogram --- COGS
ggplot(data, aes(x = cogs)) +
  geom_histogram(binwidth = 50, fill = "yellow", color = "black") +
  labs(title = "Histogram of Purchases by COGS", x = "COGS", y = "Frequency")

#Histogram --- Gross Income
ggplot(data, aes(x = gross.income)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  labs(title = "Histogram of Purchases by Gross Income", x = "Gross Income", y = "Frequency")

## Frequency charts ############################################################
#Frequency chart --- Gender
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "red") +
  labs(title = "Frequency of Purchases by Gender", x = "Gender", y = "Frequency")

#Frequency chart --- City
ggplot(data, aes(x = City)) +
  geom_bar(fill = "blue") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of Purchases by City", x = "City", y = "Frequency") +
  ylim(0, 500)

#Frequency chart --- Gender/City
ggplot(data, aes(x = City, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Purchases by Gender and City", x = "City", y = "Frequency")

# 4.Develop correlation and scatterplots #######################################
### Develop correlation plots for different combination of variables. Develop scatterplots to show these relationships.
### Use faceting. Comment on any observed trends.

### Created correlograms for the following sets of variables and these are the results
### https://statsandr.com/blog/correlogram-in-r-how-to-highlight-the-most-correlated-variables-in-a-dataset/


# cogs/grosDate# cogs/gross.income = 1 --> Shows a perfect positive linear relationship
# Total/cogs = 1
# Total/gross.income = 1

# Quantity/cogs = 0.71 --> Shows a strong positive correlation
# Quantity/gross.income = 0.71  
# Quantity/Total = 0.71 n

# Unit.price / cogs = 0.63 --> Shows a moderate positive correlation
# Unit.price / gross.income = 0.63
# Unit.price / Total = 0.63


#scatterplot --- cogs / gross.income = 1
ggplot(data) +
  aes(x = cogs, y = gross.income) +
  geom_point() +
  labs(title = "cogs vs gross.income",
       x = "cogs",
       y = "gross.income") +
  theme_minimal()

### This scatter plot, “cogs vs gross.income”, shows a strong positive linear relationship.
### Showing that as the cost of goods sold (cogs) increases, the gross income also rises with it.
### This implies that higher production costs are effectively translating into higher income. 


#scatterplot --- cogs / Quantity = 0.71
ggplot(data) +
  aes(x = cogs, y = Quantity) +
  geom_point() +
  labs(title = "cogs vs. Quantity",
       x = "cogs",
       y = "Quantity") +
  theme_minimal()

### This Scatter plot, “cogs vs. Quantity”, shows clear clusters of data points at certain value points on the 'cogs' axis
### (250, 500, 750, and 1000), with quantities ranging mostly between 1 and 7.5. 
### This shows that cost values are more often related to specific quantities, There is no clear trend or correlation
### indicated between “cogs” and “Quantity,” but the clustering pattern shows regular intervals in the data.


#scatterplot --- cogs / Unit.price = 0.63
ggplot(data) +
  aes(x = cogs, y = Unit.price) +
  geom_point() +
  labs(title = "cogs vs. Unit.price",
       x = "cogs",
       y = "Unit.price") +
  theme_minimal()


### This scatter plot, “cogs vs. Unit.price”, shows several upper right sloped lines of data points. 
### Showing that as cogs increases, the unit price also changes positively in fixed increments. 
### This pattern could indicate that different categories or groups within the data all have a consistent rate of increase 
### in unit price but also starting from different base prices. 

ggplot(data, 
       aes(Quantity, gross.income,
           color = Gender)) +
  geom_jitter(alpha = .5) +
  geom_smooth(method = lm) +
  facet_grid(City ~ .) +
  labs(title = "Gross Income by Quantity",
       x = "Quantity",
       y = "Gross Income")

### This scatter plot shows as customers purchase higher quantity of items the store gross income increases.
### Both males and females appear to have similar trends in gross income and quantity buying habits.

# 5. Plot sub-groups ###########################################################
data %>% count(Product.line)

df.pl <- data %>%
  filter(Product.line %in% c("Home and lifestyle", "Sports and travel","Food and beverages"))

df.pl%>%
  ggplot(aes(x = Quantity)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  facet_grid(Product.line ~ .) +
  labs(title = "Quantity by Product Line",
     x = "Quantity",
     y = "Frequency")

# "Food and Beverage" and "Home and Lifestyle" appear more uniform while "Sports and Travel" have more low and high quantity purchases.

df.pl%>%
  ggplot(aes(x = Gender, fill = Gender)) +
  geom_bar() +
  coord_flip() +
  facet_grid(Product.line ~ .) +
  labs(title = "Purchases by Gender and Product Line",
       x = "Gender",
       y = "Frequency")

# Females make more purchases in the "Food and Beverage" and "Sports and Travel" catagories

df.payment <- data %>%
  filter(Payment %in% c("Cash", "Credit card"))

df.payment%>%
  ggplot(aes(x = Total)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black") +
  facet_grid(Payment ~ .) +
  labs(title = "Total by Payment Type",
       x = "Total",
       y = "Frequency")

#There is very little difference in checkout total for customers using cash compared to credit cards.


