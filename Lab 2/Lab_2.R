###############################################################################
# Author: Josh Vertalka
# Date: 11-30-2023
# Purpose: Lab 2 of Geography 426 - Introduction to R and Exploratory Data Analysis
###############################################################################

# Objective: 
# 1. Become familiar with common R functions and how to install and load libraries.
# 2. Learn some common exploratory data analysis (EDA) techniques through R.
# 3. Become more familiar with spatial dimensions and how to handle spatial data in R.

# This script will guide you through the basics of R including how to manage packages,
# load and explore data, and begin working with spatial data.

# Install and load necessary packages
# First, we define a vector of package names that the script requires. This includes libraries
# for data manipulation, visualization, statistical analysis, and spatial data handling.
packages_needed <- c("tidyverse", "MASS", "maps", "GGally", "ggplot2", "sf", "readr", "skimr")

# Identify which of the needed packages are not currently installed on your system.
packages_to_install <- packages_needed[!packages_needed %in% installed.packages()[, "Package"]]

# Install any of the required packages that are missing. This step ensures that all
# necessary libraries are available for use in the script.
if (length(packages_to_install)) install.packages(packages_to_install)

# Load the libraries required for data manipulation, visualization, and analysis.
# Each library has a specific role, e.g., tidyverse for data science operations,
# sf for spatial data, ggplot2 for plotting, etc.
library(tidyverse)
library(MASS)
library(maps)
library(sf)
library(GGally)
library(readr)
library(skimr)

# Set the working directory to the location of your data files.
# Uncomment and adjust the path as necessary.
# setwd("path_to_your_directory")

# Load data using read_csv from the readr package. Replace the placeholder path with
# the actual location of your data file. read_csv is part of the tidyverse and is
# optimized for fast and easy data loading.
data <- read_csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 2/Lab 2 Data.csv")

###############################################################################
# Exploratory Data Analysis (EDA)
###############################################################################

# EDA is crucial for understanding the underlying patterns and structures of the data.
# It involves summary statistics, data structure exploration, and initial visualization.

# Use glimpse() to get a compact, informative display of the data structure.
# This function provides a quick overview of the data, showing types and first few values of each column.
glimpse(data)

# Generating summary statistics for each column to understand the data better.
# summary() provides descriptive statistics that summarize the central tendency,
# dispersion, and shape of the dataset's distribution, excluding NaN values.
summary(data)

# Print the dimensions of the data frame to understand its size.
# Understanding the size helps in assessing the scope of data analysis and visualization tasks.
cat("Dimensions: ", dim(data), "\n")

# Print the names of all columns to understand the variables available.
# This helps in selecting which variables to focus on in the analysis.
cat("Column Names: ", names(data), "\n")

# Calculate and print the count of missing values for each column.
# Handling missing values is a critical step in data cleaning.
na_counts <- sapply(data, function(x) sum(is.na(x)))
cat("Missing Values: ", na_counts, "\n")

# Provide a summary of a specific variable to understand its distribution.
# This example focuses on 'per_capita_income', but the variable can be changed as needed.
cat("Summary of Per Capita Income: ", summary(data$per_capita_income), "\n")

# Calculate and print basic statistical measures for 'per_capita_income', handling NAs.
# These measures provide insights into the central tendency and variability of the variable.
mean_value <- mean(data$per_capita_income, na.rm = TRUE)
median_value <- median(data$per_capita_income, na.rm = TRUE)
variance_value <- var(data$per_capita_income, na.rm = TRUE)
sd_value <- sd(data$per_capita_income, na.rm = TRUE)

# Task 1: Print out the calculated statistical measures for per capita income.
cat("The mean Per Capita Income ($) is:", mean_value, "\n")
cat("The median Per Capita Income ($) is:", median_value, "\n")
cat("The variance of the Per Capita Income ($) is:", variance_value, "\n")
cat("The standard deviation of the Per Capita Income ($) is:", sd_value, "\n")

# Task 2: Perform the above measures on another variable of choice within the data
# and print their values. Replace 'your_variable_here' with your selected variable.
# Example for 'unemployment_rate':
mean_unemployment <- mean(data$unemployment_rate, na.rm = TRUE)

# Continue with mean, median, variance, and standard deviation calculations for your selected variable.
cat("The mean Unemployment Rate is:", mean_unemployment, "\n")
cat("The median Unemployment Rate is:", median(data$unemployment_rate, na.rm = TRUE), "\n")
cat("The variance of Unemployment Rate is:", var(data$unemployment_rate, na.rm = TRUE), "\n")
cat("The standard deviation of Unemployment Rate is:", sd(data$unemployment_rate, na.rm = TRUE), "\n")

###############################################################################
# Data Visualization with ggplot2
###############################################################################

# Visualizing data helps in understanding its distribution and identifying patterns or outliers.

# Histogram: Visualizing the distribution of 'per_capita_income'.
# Histograms are useful for showing the distribution and density of data points.
hist(data$per_capita_income, main="Histogram of Per Capita Income", xlab="Per Capita Income")

# Boxplot: Identifying outliers in 'per_capita_income'.
# Boxplots are excellent for visualizing the spread and identifying outliers.
boxplot(data$per_capita_income, main="Boxplot of Per Capita Income", ylab="Per Capita Income")

# Exploring other variables for distribution and skewness through histograms.
# This helps in understanding the shape of different variables in the dataset.
hist(data$median_hh_income, main="Histogram of Median Household Income", xlab="Median Household Income")
hist(data$unemployment_rate, main="Histogram of Unemployment Rate", xlab="Unemployment Rate", )

# Adjusting histogram bins to better understand the distribution.
# The number of bins can significantly affect the interpretation of a histogram.
num_bins <- ceiling(sqrt(length(data$unemployment_rate)))
breaks <- seq(from = min(data$unemployment_rate, na.rm = TRUE), to = max(data$unemployment_rate, na.rm = TRUE), length.out = num_bins + 1)
hist(data$unemployment_rate, breaks = breaks, main = "Histogram of Unemployment Rate", xlab = "Unemployment Rate", ylab = "Frequency")

# The adjusted histogram provides a clearer view of how data is skewed.

###############################################################################
# Spatial Data Handling
###############################################################################

# Handling spatial data involves creating and manipulating spatial features like points, lines, and polygons.

# POINT representation: Creating a simple feature (sf) object for points.
points <- st_as_sf(data.frame(id = 1:2, x = c(-99.74, -99.75), y = c(32.45, 32.46)), coords = c("x", "y"), crs = 4326)

# LINE representation: Converting a dataframe to a simple feature (sf) object and aggregating points to form lines.
line_data <- data.frame(id = c(1, 1), x = c(-99.74, -99.75), y = c(32.45, 32.46))
line_sf <- st_as_sf(line_data, coords = c("x", "y"), crs = 4326)
line_sf_line <- line_sf %>% group_by(id) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")

# AREAL (Polygon) representation: Creating a simple feature (sf) object for polygons.
coords <- matrix(c(-99.74, 32.45, -99.75, 32.45, -99.75, 32.46, -99.74, 32.46, -99.74, 32.45), byrow = TRUE, ncol = 2)
poly <- st_polygon(list(coords))
polygon_sf <- st_sf(geometry = st_sfc(poly), crs = 4326)

# Plotting points, lines, and polygons to visualize spatial features.
plot(st_geometry(points), col = 'blue', pch = 19)
plot(st_geometry(line_sf_line), col = 'red')
plot(polygon_sf, col = 'green')




#####################
# QUESTIONS
#####################
# 1. The mean Per Capita Income in the United States is $26093.12.
# 2. Altering the number of bins lets you see the spread of the data in a finer 
#    view and lets you see get a better view of the outlier data. It can also 
#    let you condense the spread of the data into a less fine view, potentially 
#    giving you a smoother looking distribution.
# 3. A histogram gives you a good view of how the data is distributed over the 
#    whole range while a boxplot highlights the outliers and shows range of the 
#    data excluding the outliers.
# 4. The most effective transformation for normalizing the Unemployment rate 
#    would be to use log10.
hist(log10(data$unemployment_rate), main="Histogram of Unemployment Rate", 
     xlab="Unemployment Rate", )
# 5. A single or small number of trees would be best represented as points 
#    while a large number of trees/a forest would be represented as a polygon.


#####################
# MY CODE
#####################
# 1. 
cat("The mean Poverty Rate is:", mean(data$poverty, na.rm = TRUE), "\n")
cat("The median Poverty Rate is:", median(data$poverty, na.rm = TRUE), "\n")
cat("The variance of Poverty Rate is:", var(data$poverty, na.rm = TRUE), "\n")

# 2. 
boxplot(data$poverty, main="Boxplot of Poverty Rate", ylab="Poverty Rate")

# 3. Poverty rate is positivley skewed. You can tell because of the very 
#    large number of outliers highlighted by the boxplot and the long left tail 
#    on the histogram.
num_bins <- ceiling(sqrt(length(data$poverty)))
breaks <- seq(from = min(data$poverty, na.rm = TRUE), 
              to = max(data$poverty, na.rm = TRUE), length.out = num_bins + 1)
hist(data$poverty, breaks = breaks, main = "Histogram of Poverty Rate", 
     xlab = "Poverty Rate", ylab = "Frequency")

# 4. 
points <- st_as_sf(data.frame(id = 1:3, x = c(-99.74, -99.75, -99.63), 
                                 y = c(32.45, 32.46, 32.67)), 
                      coords = c("x", "y"), crs = 4326)
plot(st_geometry(points), col = 'black', pch = 19)

line_data <- data.frame(id = c(1, 1), x = c(99.75, -99.63), 
                        y = c(32.46, 59.67))
line_sf <- st_as_sf(line_data, coords = c("x", "y"), crs = 4326)
line_sf_line <- line_sf %>% group_by(id) %>% 
  summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")
plot(st_geometry(line_sf_line), col = 'blue')

coords <- matrix(c(-99.74, 32.45, -99.745, 32.4, -99.75, 32.45, 
                   -99.75, 32.46, -99.74, 32.46, -99.74, 32.45), 
                 byrow = TRUE, ncol = 2)
poly <- st_polygon(list(coords))
polygon_sf <- st_sf(geometry = st_sfc(poly), crs = 4326)
plot(polygon_sf, col = 'red')


# 5. 
points <- st_as_sf(data.frame(id = 1:5, x = c(-99.74, -99.75, 
                                              -99.66, -99.70, -99.72), 
                              y = c(32.45, 32.46, 32.41, 32.56, 32.50)), 
                   coords = c("x", "y"), crs = 4326)
plot(st_geometry(points), col = 'black', pch = 19)

