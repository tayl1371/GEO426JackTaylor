################################################################################
# Author: Josh Vertalka
# Date: 12-20-2023
# Purpose: Lab 3 of Geography 426
# This lab's focus is on introducing you to some core geographical 
# data types including vector and raster datasets. 

#Objectives
#1. Understand the utility of the sf library package
#2. Identify some ways we can manipulate geospatial data in R
#3. Learn how we can create geospatial data in R
#4. Examine a few ways to show different geographical phenomena
#5. Gain first hand experience with different data classification in mapping
################################################################################

################################################################################
################## Initial Setup and Package Installation ######################
################################################################################
# Installing required R packages for working with spatial data.
# 'sf' is for handling vector data, 'terra' for raster data, and 'spData' for sample datasets.
# 'spDataLarge' contains larger datasets, hosted on a specific repository.
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# Creating a vector of package names for easier management.
my_packages <- c("sf", "terra", "spData", "sfheaders", 
                 "dplyr", "ggplot2", "deldir", 
                 "classInt", "RColorBrewer", "cartography")
# Checking which packages are not installed and installing them.
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

# Loading the installed packages into the R session.
library(sf)            # classes and functions for vector data
library(terra)         # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(sfheaders)     # helps create spatial data much faster - not necessary for this course but you might need it in the future
library(dplyr)         # several useful and intuitive functions
library(ggplot2)       # useful for visualizations
library(deldir)        # Delaunay triangulation and the Dirichlet or Voronoi tessellation
library(classInt)      # For creating class intervals for map legends
library(RColorBrewer)  # Provides color schemes for maps and graphics
library(cartography)   # Tools for creating thematic maps


# Accessing vignettes for the 'sf' package for additional information and tutorials.
vignette(package = "sf")
vignette("sf1")

################################################################################
################## Exploring sf Package and Basic Vector Operations ######################
################################################################################
# Examining the structure and variable names of the 'world' dataset from 'spData'.
class(world)
names(world)

# Basic plot of the 'world' dataset to visualize global boundaries.
plot(world)

# Reading shapefile of the world and converting it into different formats.
# 'world_dfr' as a standard data frame and 'world_tbl' as a 'tibble' from tidyverse.
world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))
world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)
class(world_tbl)

# Plotting specific columns of the 'world' dataset.
plot(world[3:6])
plot(world["pop"])

# Subsetting 'world' dataset to only include North American countries.
NorthAmerica = world[world$continent == "North America", ]
plot(NorthAmerica)

# Applying spatial union to North American countries and observing the change.
# This merges all country geometries into a single geometry.
NorthAmerica = st_union(NorthAmerica)
plot(NorthAmerica)

# Adding a layer to an existing plot and modifying its color.
plot(world["pop"], reset = FALSE)
plot(NorthAmerica, add = TRUE, col = "red") 

# Creating a proportional symbols map based on population.
#This is a very gross way of using proportional symbols
#This is only to illustrate the ease at which we can create
#these types of maps but also show you how to use the 
# 'st_centroid' function. Please do NOT build proportional 
# symbol maps with strictly the code below. 
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# Focusing on India and adjusting the bounding box for the plot.
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(india), add = TRUE)


################################################################################
################## Creating and Understanding Simple Feature Geometries ######################
################################################################################
# Creating simple feature geometries (points, lines, polygons) using various functions.
lnd_point = st_point(c(0.1, 51.5))  # Creating a point geometry.
lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")  # Wrapping it as a simple feature column.
lnd_attrib = data.frame(  # Creating a data frame with additional attributes.
  name = "London",
  temperature = 25,
  date = as.Date("2023-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)  # Combining attributes and geometry into an sf object.

# Printing and plotting the created 'sf' object.
lnd_sf
plot_sf(lnd_sf)

# Creating different types of points with varying dimensions (XY, XYZ, XYM, XYZM).
st_point(c(5, 2))
st_point(c(5, 2, 3))
st_point(c(5, 2, 1), dim = "XYM")
st_point(c(5, 2, 3, 1))

# Creating MULTIPOINT and LINESTRING geometries using matrices.
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
plot(st_linestring(linestring_matrix))

# Creating POLYGON geometries, including one with a hole.
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
plot(st_polygon(polygon_with_hole_list))

# Creating MULTILINESTRING and MULTIPOLYGON geometries.
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring(multilinestring_list)
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
plot(st_multipolygon(multipolygon_list))

# Creating a GEOMETRYCOLLECTION containing a MULTIPOINT and a LINESTRING.
geometrycollection_list = list(st_multipoint(multipoint_matrix),
                               st_linestring(linestring_matrix))
plot(st_geometrycollection(geometrycollection_list))


################################################################################
################## Creating and Working with Simple Feature Columns (sfc) ######################
################################################################################
# Define points
point1 <- st_point(c(0, 0))  # Example: a point at (0,0)
point2 <- st_point(c(1, 1))  # Example: a point at (1,1)

# Create a simple feature column for points
points_sfc <- st_sfc(point1, point2)

# Define polygons
polygon1 <- st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
polygon2 <- st_polygon(list(matrix(c(2,2, 3,2, 3,3, 2,3, 2,2), ncol = 2, byrow = TRUE)))

# Create a simple feature column for polygons
polygon_sfc <- st_sfc(polygon1, polygon2)

# Define multilinestrings
multilinestring1 <- st_multilinestring(list(matrix(c(0,0, 0,1, 1,1), ncol = 2, byrow = TRUE)))
multilinestring2 <- st_multilinestring(list(matrix(c(1,1, 1,2, 2,2), ncol = 2, byrow = TRUE)))

# Create a simple feature column for multilinestrings
multilinestring_sfc <- st_sfc(multilinestring1, multilinestring2)

# Print the created sfc objects
print(points_sfc)
print(polygon_sfc)
print(multilinestring_sfc)

plot(points_sfc)
plot(polygon_sfc)
plot(multilinestring_sfc)
################################################################################
################## sfheaders: Speeding Up Construction of Spatial Data ######################
################################################################################
# Using sfheaders to quickly create simple feature geometries.
v = c(1, 1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)

# Comparing outputs from sf and sfheaders.
v_sfg_sf = st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)

# Creating LINESTRING and POLYGON using matrices and data frames.
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)
df = data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

# Creating simple feature columns using sfheaders.
sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

# Setting CRS for a simple feature created with sfheaders.
df_sf = sfheaders::sf_polygon(obj = df)
df_sf <- st_set_crs(df_sf, "EPSG:4326")

################################################################################
################## S2 Geometry Engine and GIS Operations ######################
################################################################################
# Checking the status of the S2 geometry engine.
sf_use_s2()

# Demonstrating the effect of the S2 engine on buffer calculations.
india_buffer_with_s2 = st_buffer(india, 1)  # Buffer with S2 enabled.
sf_use_s2(FALSE)  # Disabling S2.
india_buffer_without_s2 = st_buffer(india, 1)  # Buffer without S2.

# Re-enabling the S2 geometry engine.
sf_use_s2(TRUE)

################################################################################
################## Working with Raster Data ######################
################################################################################
# Reading a raster file and creating a SpatRaster object.
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)
print(my_rast)

# Basic raster plotting
plot(my_rast) # Plots the raster data stored in 'my_rast', providing a visual representation of the raster.

# Loading a single raster file
single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge") # Retrieves the file path for 'srtm.tif' from the 'spDataLarge' package.
single_rast = rast(raster_filepath) # Loads the raster file into a 'SpatRaster' object, which is a data structure for handling raster data in terra.

# Creating a new raster from scratch
new_raster = rast(nrows = 6, ncols = 6, # Specifies the raster's dimensions: 6 rows and 6 columns.
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, # Sets the spatial extent of the raster.
                  vals = 1:36) # Assigns values from 1 to 36 to each cell in the raster.

# Loading and working with a multi-layer raster
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge") # Retrieves the file path for 'landsat.tif' from the 'spDataLarge' package.
multi_rast = rast(multi_raster_file) # Loads the multi-layer raster file into a 'SpatRaster' object.
print(multi_rast) # Prints details of the 'multi_rast' object, like its dimensions, resolution, and extent.
plot(multi_rast) # Plots the multi-layer raster.

nlyr(multi_rast) # Counts and returns the number of layers in the 'multi_rast' object.

# Subsetting layers from the multi-layer raster
multi_rast3 = subset(multi_rast, 3) # Extracts the third layer from 'multi_rast'.
multi_rast4 = subset(multi_rast, "landsat_4") # Extracts the layer named 'landsat_4' from 'multi_rast'.

multi_rast34 = c(multi_rast3, multi_rast4) # Combines the two extracted layers into a new multi-layer raster.
plot(multi_rast34)
################################################################################
################## Working with Coordinate Reference Systems (CRS) ######################
################################################################################

luxembourg = world[world$name_long == "Luxembourg", ] # Subsets the 'world' dataset to include only Luxembourg.
st_area(luxembourg) # Calculates the area of Luxembourg, requires the 's2' package for spherical calculations.

units::set_units(st_area(luxembourg), km^2) # Converts the area units from square meters to square kilometers using the 'units' package.

################################################################################
################## Working with raster resolution using terra package ######################
################################################################################

res(my_rast) # Retrieves the resolution of the raster stored in 'my_rast'.

repr = project(my_rast, "EPSG:26912") # Reprojects 'my_rast' to a new CRS (EPSG:26912).
res(repr) # Retrieves the resolution of the reprojected raster 'repr'.

################################################################################
################################################################################
#################### Examples of spatial data types ############################
################################################################################
################################################################################

# Fake spatial data
spatial_data <- data.frame(
  id = 1:10, 
  land_use_type = c('Residential', 'Commercial', 'Industrial', 'Residential', 'Commercial', 'Industrial', 'Residential', 'Commercial', 'Industrial', 'Residential'), 
  soil_erosion_severity = c('Low', 'Moderate', 'Severe', 'Low', 'Moderate', 'Severe', 'Low', 'Moderate', 'Severe', 'Low'), 
  temperature = round(runif(10, -20, 40), 0), # Celsius as an interval example
  elevation = round(runif(10, 0, 2000), 0) # Meters as a ratio example
)

# Convert to sf object (assuming a simple CRS for illustration)
spatial_data_sf <- st_as_sf(spatial_data, coords = c('id', 'id'), crs = 4326) # Fake coordinates

# Nominal level: land_use_type
# These are categorical data without intrinsic ordering
print(unique(spatial_data_sf$land_use_type))

# Ordinal level: soil_erosion_severity
# These have a logical order: Low < Moderate < Severe
spatial_data_sf$soil_erosion_severity <- factor(spatial_data_sf$soil_erosion_severity, 
                                                levels = c('Low', 'Moderate', 'Severe'),
                                                ordered = TRUE)
print(spatial_data_sf$soil_erosion_severity)

# Interval level: temperature
# Temperature has meaningful differences between values, but no true zero point
print(spatial_data_sf$temperature)

# Ratio level: elevation
# Elevation has a true zero point and the differences between values are meaningful
print(spatial_data_sf$elevation)

################################################################################
################################################################################
########################### Geographic Phenomena ###############################
################################################################################
################################################################################
# Create an sf object for discrete points (trees)
set.seed(1234)  # Setting a seed for reproducibility of random generation
trees_data <- data.frame(
  id = 1:10,  # Tree identifiers
  x = runif(10, -99.76, -99.74),  # Random x-coordinates for trees within a given range
  y = runif(10, 32.45, 32.46)  # Random y-coordinates for trees within a given range
)
trees_sf <- st_as_sf(trees_data, coords = c("x", "y"), crs = 4326)  # Convert data frame to spatial features with specified CRS (Coordinate Reference System)

# Update the grid for continuous smooth surface (elevation in feet)
grid <- expand.grid(x = seq(-99.76, -99.74, length.out = 100), 
                    y = seq(32.45, 32.46, length.out = 100))  # Create a grid of points
grid$elevation <- with(grid, (sin(pi * x) + cos(pi * y)) * 2500 + 1500)  # Calculate elevation values for each grid point

# Arid region color palette for elevation
arid_palette <- colorRampPalette(c("saddlebrown", "darkkhaki", "lightyellow"))  # Define a color palette for the elevation map

# Plot the spatial data
ggplot() +
  geom_sf(data = trees_sf, aes(color = "Trees"), size = 3) +  # Plot trees as points
  geom_raster(data = grid, aes(x = x, y = y, fill = elevation), alpha = 0.5) +  # Plot elevation as a raster layer
  scale_fill_gradientn(colors = arid_palette(100), name = "Elevation (Feet)") +  # Gradient color scale for elevation
  scale_color_manual(values = "green", name = "Tree Locations", labels = "Trees") +  # Manual color scale for trees
  labs(title = "Spatial Phenomena: Discrete Trees and Smooth Elevation in Arid Region") +  # Title for the plot
  theme_minimal() +  # Use a minimal theme for the plot
  guides(color = guide_legend(override.aes = list(size = 5)))  # Custom legend for tree points

# Create more points (trees) with different coordinates
trees_data <- data.frame(
  id = 1:5,  # Tree identifiers
  x = c(-99.75, -99.742, -99.75, -99.76, -99.73),  # X-coordinates of trees
  y = c(32.45, 32.46, 32.47, 32.48, 32.49)  # Y-coordinates of trees
)
trees_sf <- st_as_sf(trees_data, coords = c("x", "y"), crs = 4326)  # Convert to sf object

# Create a line feature (road)
road_coords <- matrix(c(
  -99.76, 32.45,  # Starting coordinates of the road
  -99.72, 32.49   # Ending coordinates of the road
), ncol = 2, byrow = TRUE)
road_line <- st_linestring(road_coords)  # Create a linestring object
road_line <- st_sfc(road_line, crs = 4326)  # Convert to sf collection with CRS

# Create a polygon feature (lake)
lake_coords <- matrix(c(
  -99.75, 32.47,  # Coordinates defining the boundary of the lake
  -99.74, 32.47,
  -99.74, 32.48,
  -99.75, 32.48,
  -99.75, 32.47
), ncol = 2, byrow = TRUE)
lake_sf <- st_polygon(list(lake_coords))  # Create a polygon object
lake_sf <- st_sfc(lake_sf, crs = 4326)  # Convert to sf collection with CRS

# Plot the spatial features (trees, road, and lake)
ggplot() +
  geom_sf(data = trees_sf, aes(color = "Trees"), size = 3) +  # Plot trees
  geom_sf(data = road_line, aes(color = "Road"), size = 2, linetype = "dashed") +  # Plot road as a dashed line
  geom_sf(data = lake_sf, aes(fill = "Lake"), color = NA, alpha = 0.5) +  # Plot lake with fill color
  scale_color_manual(values = c("Trees" = "green", "Road" = "grey50")) +  # Manual color settings for trees and road
  scale_fill_manual(values = c("Lake" = "blue")) +  # Manual fill color for lake
  labs(title = "Spatial Representation of Trees, Road, and Lake") +  # Title for the plot
  theme_minimal()  # Use a minimal theme for the plot

# Basic point plot for tree locations
ggplot(trees_sf) +
  geom_sf(aes(color = "Trees"), size = 3) +  # Plot tree locations
  labs(title = "Basic Tree Locations") +  # Title for the plot
  theme_minimal()  # Use a minimal theme for the plot

# Heatmap to display the density of trees
ggplot() +
  geom_density2d_filled(data = as.data.frame(st_coordinates(trees_sf)), 
                        aes(x = X, y = Y), alpha = 0.5) +  # Create a filled density plot for trees
  labs(title = "Tree Density Heatmap") +  # Title for the heatmap
  theme_minimal()  # Use a minimal theme for the heatmap

# Voronoi Diagram to partition the space based on proximity to each tree
coords <- as.data.frame(st_coordinates(trees_sf))  # Extract tree coordinates
voronoi <- deldir(coords$X, coords$Y)  # Compute Voronoi tessellation

# Convert Voronoi edges to LINESTRING and create sf objects
edges <- voronoi$dirsgs
edges_sf <- do.call(rbind, lapply(1:nrow(edges), function(i) {
  line <- st_linestring(matrix(c(edges[i, "x1"], edges[i, "y1"], 
                                 edges[i, "x2"], edges[i, "y2"]), ncol = 2, byrow = TRUE))
  st_sf(geometry = st_sfc(line, crs = 4326))
}))

# Combine into a single sf object
edges_sf <- st_sf(geometry = do.call(st_sfc, lapply(edges_sf$geometry, function(x) x)), crs = 4326)

# Plot the Voronoi diagram
ggplot() +
  geom_sf(data = edges_sf, color = "blue") +  # Plot Voronoi edges
  geom_point(data = coords, aes(x = X, y = Y), color = "green", size = 3) +  # Plot tree points
  labs(title = "Voronoi Diagram of Trees") +  # Title for the Voronoi diagram
  theme_minimal()  # Use a minimal theme for the plot

####################################################################
####################################################################
################ Examples with Downloaded Data #####################
####################################################################
####################################################################

# Loading data for geographical and demographic analysis
# Source URLs for data:
# - Michigan marriage and divorce rates: https://www.mdch.state.mi.us/osr/marriage/MxDivCounty.asp?MType=2
# - Michigan geographical data: https://gis-michigan.opendata.arcgis.com/

# Read in shapefile containing Michigan county data
mi_counties <- st_read("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 3/mi_counties/Counties_(v17a).shp")
# Clean up county names by removing any periods
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
# Read in CSV file containing marriage rates in Michigan for 2022
marriages <- read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 3/MI_Mariage_rates_2022.csv")

# Display the first few rows of each dataset for inspection
head(mi_counties)
head(marriages)

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric for analysis
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

# Calculate state-wide average marriage rate
state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
# Calculate expected marriage rate for each county based on state average
mi_counties$marriageExp <- state_rate * mi_counties$Population
# Compute relative risk of marriage for each county
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

# Plot the spatial data of Michigan counties
plot(mi_counties)

# Visualize the data using ggplot
ggplot(mi_counties) +
  geom_sf(aes(fill = relRisk)) +  # Color counties based on relative risk
  theme_minimal()  # Use minimal theme for better visualization

# Close any open graphics devices
dev.off()

# Generate color palettes using RColorBrewer for choropleth maps
mp5 <- brewer.pal(5, "Greens")  # 5-class Green palette
mp7 <- brewer.pal(7, "Greens")  # 7-class Green palette
mp9 <- brewer.pal(9, "Greens")  # 9-class Green palette

# Create choropleth maps with different classification methods
# Equal area classification with 5 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "equal", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \n Total Marriages: \nEqual Area, 5 Colors")

# Equal area classification with 7 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "equal", nclass = 7,
           col = mp7,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nEqual Area, 7 Colors")

# Equal area classification with 9 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "equal", nclass = 9,
           col = mp9,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nEqual Area, 9 Colors")

# Quantile classification with 5 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "quantile", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nQuantile, 5 Colors")

# Standard deviation classification with 5 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "sd", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nStandard Deviation, 5 Colors")

# Natural Breaks (Fisher-Jenks) classification with 5 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "fisher-jenks", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nNatural Breaks, 5 Colors")

# Kmeans classification with 5 colors
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "kmeans", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nKmeans, 5 Colors")

# Function to create divergent breaks for choropleth mapping
div_breaks <- function(data, num_classes, midpoint) {
  if (num_classes %% 2 == 0) {
    stop("Number of classes must be an odd number to include the midpoint as a separate class.")
  }
  
  # Calculate the breaks for the lower and upper halves
  lower_half_classes <- (num_classes - 1) %/% 2
  upper_half_classes <- (num_classes - 1) %/% 2
  
  lower_half <- seq(min(data, na.rm = TRUE), midpoint, length.out = lower_half_classes + 1)
  upper_half <- seq(midpoint, max(data, na.rm = TRUE), length.out = upper_half_classes + 1)[-1] # Exclude the first element
  
  breaks <- c(lower_half, midpoint, upper_half)
  return(breaks)
}

# Calculate breaks for divergent choropleth mapping with 5 classes
breaks <- div_breaks(mi_counties$relRisk, 5, 100)
print(breaks)

# Divergent color palette with 5 colors
divp5 <- brewer.pal(5, "RdBu")

# Divergent choropleth map with 5 classes
choroLayer(x = mi_counties, var = "relRisk",
           breaks = breaks,
           col = divp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nRelative Risk: \nDivergent, 5 Colors")

# Calculate breaks for divergent choropleth mapping with 7 classes
divp7 <- brewer.pal(7, "RdBu")
breaks <- div_breaks(mi_counties$relRisk, 7, 100)
print(breaks)

# Divergent choropleth map with 7 classes
choroLayer(x = mi_counties, var = "relRisk",
           breaks = breaks,
           col = divp7,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nRelative Risk: \nDivergent, 7 Colors")

#####################
# MY CODE
#####################
# 1. 
my_mx = rbind(c(1, 4), c(2, 4), c(3, 4), c(4, 4))
my_ls = list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))

geometry_list = list(st_point(c(5, 2)), st_linestring(my_mx), st_polygon(my_ls))
geo_collection = st_geometrycollection(geometry_list)

sf_use_s2()
geo_collection_buffer = st_buffer(geo_collection, 1)
plot(geo_collection_buffer)

# 2. 
square_1 = st_sfc(st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))))
square_2 = st_sfc(st_polygon(list(cbind(c(2, 3, 3, 2, 2), c(2, 2, 3, 3, 2)))))
square_union = st_union(square_1, square_2)
plot(square_union)

# 3. 
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast1 = subset(multi_rast, 1)
multi_rast2 = subset(multi_rast, 2)
multi_rast12 = c(multi_rast1, multi_rast2)

jpeg("Lab_3.jpeg")
plot(multi_rast12)
dev.off()

# 4. 
forest = st_sfc(st_polygon(list(cbind(c(-99.75, -99.745, -99.742, -99.74, -99.75), 
                                      c(32.455, 32.453, 32.451, 32.46, 32.455)))))
forest_sf <- st_as_sf(forest, coords = c("x", "y"), crs = 4326)

grid <- expand.grid(x = seq(-99.76, -99.74, length.out = 100), 
                    y = seq(32.45, 32.46, length.out = 100))

ggplot() +
  geom_sf(data = forest_sf, aes(color = "Forest"), size = 3) +
  scale_color_manual(values = "darkgreen", name = "Legend", labels = "Forest") +
  labs(title = "Trees represented as the area of a forest") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 5)))

# 5. 
mp5 <- brewer.pal(5, "Reds")

choroLayer(x = mi_counties, var = "Divorce.Rate",
           method = "fisher-jenks", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Divorce Rate (%)")
