################################################################################
# Author: Josh Vertalka
# Date: 12-29-2023
# Purpose: Lab 4 of Geography 426
# This lab will focus on Scale, Resolution, and Generalization

# Objectives
# 1: Learn some common ways to change the scale of a map by subletting
# to geographic coordinates, selecting countries, . 

# 2: Learn some common ways to change the resolution of a map by 
# changing it's scale. 

# 1: Learn some common ways to change the resolution of a map by 
# changing it's scale. 

################################################################################

# Cartography in R - Part 1: Scale

# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(maps)
library(maptools)
library(mapdata)
library(ggspatial)
library(concaveman)
library(smoothr)

################################################################################
################################################################################
############################ Scaling Techniques ################################
################################################################################
################################################################################

# Load world map data
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# 1. Global Scale Map
ggplot(data = world) +
  geom_sf() +
  labs(title = "World Map - Global Scale") +
  theme_minimal()

world_valid <- st_make_valid(world)

# 2. Continent Scale Map - Focusing on Europe
europe <- st_crop(world_valid, xmin = -10, xmax = 40, ymin = 35, ymax = 70)
ggplot(data = europe) +
  geom_sf() +
  labs(title = "European Map - Continent Scale") +
  theme_minimal()

# 3. Country Scale Map - Focusing on Germany
germany <- st_crop(europe, xmin = 5, xmax = 15, ymin = 47, ymax = 55)
ggplot(data = germany) +
  geom_sf() +
  labs(title = "Germany Map - Country Scale") +
  theme_minimal()

# 4. City Scale Map - Focusing on Berlin
berlin <- st_crop(germany, xmin = 13.2, xmax = 13.6, ymin = 52.3, ymax = 52.7)
ggplot(data = berlin) +
  geom_sf() +
  labs(title = "Berlin Map - City Scale") +
  theme_minimal()

# 5. Street Scale Map - Focusing on central Berlin
central_berlin <- st_crop(berlin, xmin = 13.36, xmax = 13.42, ymin = 52.5, ymax = 52.52)
ggplot(data = central_berlin) +
  geom_sf() +
  labs(title = "Central Berlin Map - Street Scale") +
  theme_minimal()

# 6. Demonstrating Scale using Scale Bar and North Arrow
# Here, we'll use 'ggsn' package for scale bar and north arrow
ggplot(data = central_berlin) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +  # Add scale bar at bottom left
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +  # Add north arrow at top right
  labs(title = "Central Berlin with Scale Bar and North Arrow") +
  theme_minimal()



################################################################################
################################################################################
########################### Changing Resolution ################################
################################################################################
################################################################################


# 1. High-Resolution Raster Example
# Load a high-resolution raster dataset (example data)
data(volcano)
volcano_raster <- raster(volcano)
plot(volcano_raster, main = "High-Resolution Raster")

# 2. Lower the Resolution - Example 1
# Aggregate the data to lower the resolution
volcano_lowres1 <- aggregate(volcano_raster, fact = 2)
plot(volcano_lowres1, main = "Lower Resolution Raster (Factor = 2)")

# 3. Lower the Resolution - Example 2
volcano_lowres2 <- aggregate(volcano_raster, fact = 12)
plot(volcano_lowres2, main = "Lower Resolution Raster (Factor = 12)")

# 4. Lower the Resolution - Example 3
volcano_lowres3 <- aggregate(volcano_raster, fact = 20)
plot(volcano_lowres3, main = "Lower Resolution Raster (Factor = 20)")

# 5. Comparison of Resolutions
par(mfrow = c(2, 2))
plot(volcano_raster, main = "Original Resolution")
plot(volcano_lowres1, main = "Lower Resolution (Factor = 2)")
plot(volcano_lowres2, main = "Lower Resolution (Factor = 12)")
plot(volcano_lowres3, main = "Lower Resolution (Factor = 20)")
par(mfrow = c(1, 1))

# 6. Impact of Resolution on Analysis
# Example: Calculate the mean elevation for each resolution
highres_mean <- cellStats(volcano_raster, stat = 'mean')
lowres1_mean <- cellStats(volcano_lowres1, stat = 'mean')
lowres2_mean <- cellStats(volcano_lowres2, stat = 'mean')
lowres3_mean <- cellStats(volcano_lowres3, stat = 'mean')

cat("Mean Elevation (Original Resolution):", highres_mean, "\n")
cat("Mean Elevation (Lower Resolution - Factor 2):", lowres1_mean, "\n")
cat("Mean Elevation (Lower Resolution - Factor 12):", lowres2_mean, "\n")
cat("Mean Elevation (Lower Resolution - Factor 20):", lowres3_mean, "\n")

# 7. Visualizing Resolution Impact on Spatial Analysis
# Creating a basic function to visualize differences in analysis results
plot_resolution_impact <- function(high, low1, low2, low3) {
  res_values <- c(high, low1, low2, low3)
  barplot(res_values, beside = TRUE, 
          names.arg = c("High", "Low-2", "Low-4", "Low-8"),
          main = "Impact of Resolution on Mean Elevation Analysis")
}

plot_resolution_impact(highres_mean, lowres1_mean, lowres2_mean, lowres3_mean)

#Let's look at the resolution of our data but change the resolution of a saved
#image

width_in_inches <- 8 # desired width in inches
height_in_inches <- 6 # desired height in inches

dpi <- 50
# Convert inches to pixels
width_pixels <- width_in_inches * dpi
height_pixels <- height_in_inches * dpi
jpeg(filename = "volcano_300dpi.jpg", 
     res = dpi, width = width_pixels, height = height_pixels)
plot(volcano_raster, main = "Original Resolution - low resolution image")
dev.off()

dpi <- 900
# Convert inches to pixels
width_pixels <- width_in_inches * dpi
height_pixels <- height_in_inches * dpi
jpeg(filename = "volcano_900dpi.jpg", 
     res = dpi, width = width_pixels, height = height_pixels)
plot(volcano_raster, main = "Original Resolution - high resolution image")
dev.off()

################################################################################
################################################################################
#################### #Generalization Techniques ################################
################################################################################
################################################################################

#Let's start with simplification approaches on our data. 
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
line = st_linestring(linestring_matrix)
plot(line)

original_line <- line
generalized_line = st_simplify(original_line, dTolerance = 0.8)
plot(generalized_line)

generalized_line = st_simplify(original_line, dTolerance = 1.5)
plot(generalized_line)

generalized_line = st_simplify(original_line, dTolerance = 2)
plot(generalized_line)

#We can also do it with Polygons
# 1. Original Vector Data
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(st_geometry(nc), main="Original North Carolina Shapefile")

# 2. Generalized Vector Data
nc_simplified <- st_simplify(nc, dTolerance = 500)
plot(st_geometry(nc_simplified), main="Simplified Geometry (dTolerance = 500)")


nc_more_simplified <- st_simplify(nc, dTolerance = 4000)
plot(st_geometry(nc_more_simplified), main="Simplified Geometry (dTolerance = 4000)")


nc_extreme_simplified <- st_simplify(nc, dTolerance = 12000)
plot(st_geometry(nc_extreme_simplified), main="Simplified Geometry (dTolerance = 12000)")



par(mfrow = c(1, 4)) #This allows us to plot multiple images at once
plot(st_geometry(nc), main="Original")
plot(st_geometry(nc_simplified), main="Simplified (dTol = 500)")
plot(st_geometry(nc_more_simplified), main="Simplified (dTol = 4000)")
plot(st_geometry(nc_extreme_simplified), main="Simplified (dTol = 12000)")
par(mfrow = c(1, 1))



#Some warnings about simplifying polygons. Because you change it's shape, 
# you change it's area.
nc <- st_transform(nc, crs = 32119) 
areas_original <- st_area(nc)

nc_extreme_simplified <- st_transform(nc_extreme_simplified, crs = 32119) 
areas_extreme <- st_area(nc_extreme_simplified)

#let's look at the difference
print(areas_extreme - areas_original) #pretty big area difference

#A way around this is to store the areas in the dataset before the simplification
nc$AreaCalculated<-areas_original
print(summary(nc$AreaCalculated))
nc_extreme_simplified <- st_simplify(nc, dTolerance = 12000)
print(summary(nc_extreme_simplified$AreaCalculated))


#Can we generalize points using this approach? 

#### Let's try some smoothing of data.

#We'll use the 'smooth' function which has some nice features. This function
#will let you choose between 'chaikin', 'ksmooth', 'spline', and 'densify'
#approaches for smoothing.
?smooth # remember this will open up the help menu and let you look at the 
#arguments. 
smoothed_line <- smooth(line, method = 'chaikin')

par(mfrow = c(1, 2))
plot(line, main = "Original Line")
plot(smoothed_line, main = "Smoothed Line")

#Let's look at a couple of other smoothing options
smoothed_line_ksmooth <- smooth(line, method = 'ksmooth')
smoothed_line_spline <- smooth(line, method = 'spline')
par(mfrow = c(1, 3))
plot(smoothed_line, main = "Smoothed Line - chaikin")
plot(smoothed_line_ksmooth, main = "Smoothed Line - ksmooth")
plot(smoothed_line_spline, main = "Smoothed Line - spline")
#These are some subtle differences but useful to keep in mind.



#Let's try with Polygons
smoothed_nc <- smooth(nc, method = 'chaikin')
plot(st_geometry(nc), main="Original")
plot(st_geometry(smoothed_nc), main="Smoothed")

################
################
#Try to smooth polygons using ksmooth and spline - produce a 2x2 image with
#appropriate titles



#Let's do some data aggregation. Here's a situation - imagine you are mapping
#an area and the data you have is a bunch of lat/long data for points of trees.
#All of the point data is going to clutter your map. Instead you'll put all of 
#those points in a polygon - a much cleaner representation. 

# Generate some random points
set.seed(42)
n_points <- 50
points <- matrix(runif(10 * n_points, min = -4, max = 10), ncol = 2)
colnames(points) <- c("x", "y")
points_sf <- st_as_sf(data.frame(points), coords = c("x", "y"), crs = 4326)

# Create the convex hull as a polygon
convex_hull_polygon <- st_convex_hull(st_combine(points_sf))

# Create the concave hull as a polygon
concave_hull_polygon <- st_sf(geometry = concaveman(points_sf))



# Plot the results
plot(st_geometry(points_sf), col = 'blue', pch = 20, cex = 0.5, main = "Tree Locations")
plot(st_geometry(convex_hull_polygon), col = 'red', border = 'black', lwd = 2, main = "Tree Area (Convex)")
plot(st_geometry(concave_hull_polygon), col = 'red', border = 'black', lwd = 2, main = "Tree Area (Convave)")


#Lets look at Amalgamation approaches with the North Carolina Dataset. 
#Amalgamation is really just a Union operation.

all_Union = st_union(nc) #This will Union all polygons and keep only the outline.
par(mfrow = c(1, 2))
plot(st_geometry(nc))
plot(st_geometry(all_Union))

#Let's just Union a few polygons (or counties in NC). This requires some us
#to use two maps in which we overlay them on top of each other.
ResearchTriangle <- nc[nc$NAME %in% c("Wake", "Durham", "Chatham", "Franklin",
                                      "Granville", "Harnett", "Johnston", "Lee",
                                      "Nash", "Orange", "Person", "Vance",
                                      "Warren", "Wilson"), ] #This is one way to filter
ResearchTriangle<-nc %>% dplyr::filter(NAME %in% c("Wake", "Durham", "Chatham", "Franklin",
                                          "Granville", "Harnett", "Johnston", "Lee",
                                          "Nash", "Orange", "Person", "Vance",
                                          "Warren", "Wilson"))#This is another way to filter

plot(st_geometry(ResearchTriangle))
ResearchTriangle = st_union(ResearchTriangle)
plot(st_geometry(ResearchTriangle))

plot(st_geometry(nc), border = 'black', main = "North Carolina")
plot(st_geometry(nc), border = 'black', main = "North Carolina with Research Triangle")
plot(st_geometry(ResearchTriangle), col = "white", border = 'red', add = TRUE) #overlaying Research Triangle



#Collapse
collapsed_data <- st_centroid(nc %>% dplyr::filter(NAME == "Wake"))
plot(st_geometry(nc), border = 'black', main = "NC as Polygons")
plot(st_geometry(collapsed_data), border = 'black', main = "NC as Points")



#Refinement

#Exaggeration


#Enhancement


#Displacement - displaccement can be pretty important in smaller-scale maps
displace_line <- function(line, displacement) {
  # Convert line to matrix
  coords <- matrix(st_coordinates(line), ncol = 2)
  
  # Calculate displacement vector (perpendicular to the line)
  dx <- diff(coords[,1])
  dy <- diff(coords[,2])
  len <- sqrt(dx^2 + dy^2)
  disp_vec <- matrix(c(dy, -dx) / len, ncol = 2) * displacement
  
  # Apply the displacement
  new_coords <- coords[-nrow(coords), ] + disp_vec
  new_coords <- rbind(new_coords, tail(new_coords, n=1))  # Duplicate last point
  
  # Return a new 'LINESTRING' geometry
  st_linestring(new_coords)
}

# Usage example with two lines
line1 <- st_linestring(rbind(c(0,0), c(1,1)))
line2 <- st_linestring(rbind(c(0.01,0), c(1.01,1)))  # A line close to line1

# Displace line2
displacement_amount <- 0.05  # Change this based on your needs
line2_displaced <- displace_line(line2, displacement_amount)

# Plot to see the effect
plot(st_geometry(line1), col = 'blue', lwd = 2)
plot(st_geometry(line2), col = 'red', add = TRUE, lwd = 2)
plot(st_geometry(line2_displaced), col = 'green', add = TRUE, lwd = 2)

#####################
# QUESTIONS
#####################
#1. You might want to change the scale of your data if you need to focus on 
#   either a larger area or a smaller area.
#2. A pro of making your raster resolution lower would be a potential decreased 
#   file size. A con of making your raster resolution lower would be a lack of  
#   clear detail available, depending on the raster.
#3. A time you might want to generalize your polygons could be when you are 
#   working with a global map dataset and the shapefile you are working with is
#   huge because all of the tiny details unnecessary at a global scale.
#4. Simplifying line data removes portions of the line while simplifying polygon
#   data just reduces polygon to a much simpler form.
#5. You might want to use a concave approach if the exact boundaries of the area
#   containing the specified objects necessary/sensitive information, but you
#   might want to use a convex approach if the exact boundaries are not really
#   important.

#####################
# MY CODE
#####################
#1.
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

world_valid <- st_make_valid(world)
europe <- st_crop(world_valid, xmin = -10, xmax = 40, ymin = 35, ymax = 70)
germany <- st_crop(europe, xmin = 5, xmax = 15, ymin = 47, ymax = 55)

jpeg("Lab_4.jpeg")
ggplot(data = germany) + geom_sf() + 
  labs(title = "Germany Map") +
  theme_minimal()
dev.off()

#2.
data(volcano)
volcano_raster <- raster(volcano)
plot(volcano_raster, main = "Original")

volcano_fact30 <- aggregate(volcano_raster, fact = 30)
plot(volcano_fact30, main = "Factor = 30")

fact30_max <- cellStats(volcano_fact30, stat = 'max')

cat("Max Elevation (Factor 30):", fact30_max, "\n")

#3.
ksmooth_nc <- smooth(nc, method = "ksmooth")
spline_nc <- smooth(nc, method = "spline")

par(mfrow = c(2, 2))
plot(st_geometry(nc), main="Original")
plot(st_geometry(ksmooth_nc), main="ksmooth")
plot(st_geometry(spline_nc), main="spline")

#4.
set.seed(13453)
n_points <- 100
points <- matrix(runif(10 * n_points, min = -12, max = 24), ncol = 2)
colnames(points) <- c("x", "y")
points_sf <- st_as_sf(data.frame(points), coords = c("x", "y"), crs = 4326)

convex_polygon <- st_convex_hull(st_combine(points_sf))
concave_polygon <- st_sf(geometry = concaveman(points_sf))

par(mfrow = c(2, 2))
plot(st_geometry(points_sf), col = 'blue', pch = 20, cex = 0.5, main = "Points")
plot(st_geometry(convex_polygon), col = 'red', border = 'black', lwd = 2, main = "Convex Hull")
plot(st_geometry(concave_polygon), col = 'red', border = 'black', lwd = 2, main = "Convave Hull")

