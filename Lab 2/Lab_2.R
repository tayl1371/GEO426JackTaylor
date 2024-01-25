################################################################################
# Author: Josh Vertalka
# Date: 11-30-2023
# Purpose: Lab 2 of Geography 426
# This lab is going to provide a quick introduction in R and some
# common exploratory data analysis that you may find helpful for your 
# or course project. 

# Objective: 
# 1. Become familiar with common R functions and how to install and load libraries
# 2. Learn some common exploratory data analysis (EDA) through R
# 3. Become more familiar with spatial dimensions
################################################################################
# Welcome to R. R is a computation language. It allows us to better understand data, 
# build maps, build models, animate data, and a bunch of other things. 
# Today we are going to focus on the basics of bringing data in and understanding
# how t

#The first thing I want to tell you about is the pound or hashtag sign and it's 
# utility in R. The # allows us to write comments in our script as a non-command
# function. A command function is a function that we instruct the computer to 
# execute. 

# Creating a vector of package names for easier management.
my_packages <- c("usdata", "tidyverse", "MASS", "maps", 
                 "GGally", "ggplot2", "parallel")
# Checking which packages are not installed and installing them.
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

#If R asks if you want to restart before installation, please say yes. 

library(usdata)       # Provides datasets related to the United States, useful for analysis and visualization
library(tidyverse)    # A collection of R packages designed for data science, including data manipulation, plotting, and more
library(MASS)         # Contains functions and datasets from the book "Modern Applied Statistics with S"; useful for statistical methods
library(maps)         # Provides map databases and plotting tools, useful for creating geographical maps

library(ggplot2)      # A system for declaratively creating graphics, based on The Grammar of Graphics
library(GGally)       # Extends ggplot2 for easy creation of complex multi-plot layouts
library(parallel)     # Provides support for parallel computation, including parallel versions of apply functions




################################################################################
################################################################################
############################ Loading Data ######################################
################################################################################
################################################################################
#First let's bring some data into our environment.
data<-read.csv("C:/Users/joshu/OneDrive/Documents/Geog 426/Vertalka_Material/Labs/Lab 2/Lab 2 Data.csv")


################################################################################
################################################################################
###################### Exploring the Data ######################################
################################################################################
################################################################################

# 1. Summary Statistics: Provides a summary for each column in the data frame.
summary(data)

# 2. Structure: Displays the structure of the data frame including data types of each column.
str(data)

# 3. Dimensions: Returns the number of rows and columns in the data frame.
dim(data)

# 4. Column Names: Retrieves the names of all columns in the data frame.
names(data)

# 5. Head and Tail: Shows the first and last 6 rows of the data frame.
head(data)
tail(data)

# 6. Missing Values: Checks for missing (NA) values in the data frame.
sapply(data, function(x) sum(is.na(x)))


#Let's focus on a particular variable. Let's look at per_capita_income. 
#we can select that variable using calling the data object and using the
# '$' sign followed by the variable name. 
data$per_capita_income

################################################################################
################################################################################
##################### Measures of Centrality ###################################
################################################################################
################################################################################

#We can follow that up with some statistics of that data field by using 'summary' function
summary(data$per_capita_income)


#We can look at measures of centrality as well
mean_value <- mean(data$per_capita_income)
median_value <- median(data$per_capita_income)
variance_value <- var(data$per_capita_income)
sd_value <- sd(data$per_capita_income)


#Important: if you want to explore a function in more depth you can use the '?'
#Here's an example:

?mean

# the '?' will open up hte help screen and provide an overview of the function.
# you can do this with any function: 

?summary

#Notice that all of the values are NA, we need to remove NAs from our data.
#Now this will populate the different objects
mean_value <- mean(data$per_capita_income, na.rm = TRUE)
median_value <- median(data$per_capita_income, na.rm = TRUE)
variance_value <- var(data$per_capita_income, na.rm = TRUE)
sd_value <- sd(data$per_capita_income, na.rm = TRUE)

#These summary statistics are helpful but they don't give us an understanding 
# of the shape of the data. We can look at the shape of the data through many different ways. 

#One way we can look at the shape of data is through a histogram
hist(data$per_capita_income, main="Histogram of Per Capita Income", xlab="Per Capita Income")

#another way is through a boxplot
boxplot(data$per_capita_income, main="Boxplot of Per Capita Income", ylab="Per Capita Income")

#The shape of the data looks pretty good. We can see a normal, bell-shaped curve to the data. 
#The boxplot identified some outliers that we'll want to be mindful of for mapping. 

#Let's see if we can find a more skewed variable. 
hist(data$median_hh_income, main="Histogram of Median Household Income", xlab="Median Household Income")
#This does not look too skewed. Let's try another variable. 

hist(data$unemployment_rate, main="Histogram of Unemployment Rate", xlab="Unemployment Rate")
#Unemployment rate looks pretty skewed. 

#we can also supply different break points for the histogram:
# Determine the number of bins for the frequency table
# Common practice is to use the square root of the number of observations
num_bins <- sqrt(length(data$unemployment_rate))
num_bins <- ceiling(num_bins)  # Round up to the nearest whole number

# Create the grouped-frequency table
breaks <- seq(from = min(data$unemployment_rate, na.rm = TRUE), 
              to = max(data$unemployment_rate, na.rm = TRUE), 
              length.out = num_bins + 1)
grouped_income <- cut(data$unemployment_rate, breaks = breaks, include.lowest = TRUE)
frequency_table <- table(grouped_income)

# Create a histogram
hist(data$unemployment_rate, breaks = breaks, main = "Histogram of Per Capita Income", xlab = "Income Bins", ylab = "Frequency")

#changing the breaks we really see how skewed the data is


# Create a boxplot (dispersion graph)
boxplot(data$unemployment_rate, main = "Dispersion Graph for Per Capita Income", ylab = "Income")



################################################################################
################################################################################
####################### Data Transformations ###################################
################################################################################
################################################################################

# Original data histogram
hist(data$unemployment_rate, main="Original Unemployment Rate", xlab="Unemployment Rate")

# Log Transformation
# Adding a small constant to avoid log(0)
log_unemployment_rate <- log(data$unemployment_rate + 1)
hist(log_unemployment_rate, main="Log Transformed Unemployment Rate", xlab="Log(Unemployment Rate + 1)")

# Square Root Transformation
sqrt_unemployment_rate <- sqrt(data$unemployment_rate)
hist(sqrt_unemployment_rate, main="Square Root Transformed Unemployment Rate", xlab="Sqrt(Unemployment Rate)")

# Inverse Transformation
# Adding a small constant to avoid division by zero
inverse_unemployment_rate <- 1 / (data$unemployment_rate + 1)
hist(inverse_unemployment_rate, main="Inverse Transformed Unemployment Rate", xlab="1/(Unemployment Rate + 1)")

# Box-Cox Transformation
# The 'boxcox' function requires a model formula, so we use unemployment_rate ~ 1
bc_transform <- boxcox(unemployment_rate ~ 1, data = data)
# Find the optimal lambda value
lambda_optimal <- bc_transform$x[which.max(bc_transform$y)]
# Apply the Box-Cox transformation using the optimal lambda
boxcox_unemployment_rate <- ((data$unemployment_rate^lambda_optimal) - 1) / lambda_optimal
hist(boxcox_unemployment_rate, main="Box-Cox Transformed Unemployment Rate", xlab="Box-Cox(Unemployment Rate)")

# Cube Root Transformation
cbrt_unemployment_rate <- (data$unemployment_rate)^(1/3)
hist(cbrt_unemployment_rate, main="Cube Root Transformed Unemployment Rate", xlab="Cube Root(Unemployment Rate)")

# When using log transformation, adding 1 is common to handle values of zero, since log(0) is undefined.
# Box-Cox transformation seeks a value of λ to best normalize the data. If λ = 0, it's essentially a log 
# transformation.
# It's important to check the distribution after transformation, and sometimes none of these 
# transformations will result in perfectly normal data, especially if the original data is not 
# transformable to normality.

################################################################################
################################################################################
###################### Correlation and Regressions #############################
################################################################################
################################################################################

#Computes the correlation coefficient between per_capita_income and unemployment_rate, 
#which quantifies the strength and direction of their linear relationship.
#Fits a linear regression model predicting unemployment_rate from per_capita_income.
#Summarizes the model to include coefficients that show the estimated relationship 
#and the R-squared value that explains the proportion of variance in the unemployment_rate 
#that can be predicted from per_capita_income.
#Plots the data points and adds a regression line to visually assess the fit.

# Calculate the correlation coefficient
cor_coef <- cor(data$per_capita_income, data$unemployment_rate, use = "complete.obs")

# Interpretation of the correlation coefficient
# cor_coef close to 1 indicates a strong positive relationship
# cor_coef close to -1 indicates a strong negative relationship
# cor_coef around 0 indicates no linear relationship

# Linear Regression Model
# Fit a model where we predict unemployment rate based on per capita income
lm_model <- lm(data$unemployment_rate ~ data$per_capita_income)

# Summary of the model to get details like coefficients and R-squared value
model_summary <- summary(lm_model)

# Plot the data and add the regression line
plot(data$per_capita_income, data$unemployment_rate, main="Regression Line",
     xlab="Per Capita Income", ylab="Unemployment Rate", pch=19, col=c("blue", "black"))
abline(lm_model, col="red") # Adds the regression line

# Output the results
list(correlation_coefficient = cor_coef, regression_summary = model_summary)


################################################################################################################################################################
################################################################################################################################################################



################################################################################
################################################################################
########################### Spatial Dimensions #################################
################################################################################
################################################################################

# POINT representation
# Let's create a simple feature object for points
points <- st_as_sf(data.frame(
  id = 1:2,
  x = c(-99.74, -99.75),
  y = c(32.45, 32.46)
), coords = c("x", "y"), crs = 4326)

# LINE representation
# Create a simple feature object for lines

# Assuming line_data is already loaded
line_data <- data.frame(
  id = c(1, 1), # Same id to indicate these points are part of the same line
  x = c(-99.74, -99.75),
  y = c(32.45, 32.46)
)

# Convert to sf object
line_sf <- st_as_sf(line_data, coords = c("x", "y"), crs = 4326)

# Aggregate points by 'id' and cast to LINESTRING
line_sf_line <- line_sf %>%
  group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")


# AREAL (Polygon) representation
# Create a simple feature object for polygons
coords <- matrix(
  c(-99.74, 32.45,  # First point
    -99.75, 32.45,  # Second point
    -99.75, 32.46,  # Third point
    -99.74, 32.46,  # Fourth point
    -99.74, 32.45), # Closing point (same as first to close the polygon)
  byrow = TRUE, ncol = 2
)
# Create a POLYGON object
poly <- st_polygon(list(coords))
# Create an sf object
polygon_sf <- st_sf(geometry = st_sfc(poly), crs = 4326)

# Plot the points, lines, and polygons
plot(st_geometry(points), col = 'blue', pch = 19)
plot(st_geometry(line_sf_line), col = 'red')
plot(polygon_sf, col = 'green')

