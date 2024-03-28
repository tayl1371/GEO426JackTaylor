################################################################################
# Author: Josh Vertalka
# Date: 2-15-2024
# Purpose: Lab 7 of Geography 426
# This lab's focus is on using different interpolation methods to a create
# continuous surface of a variable of interest, plot the interpolated
# results, and create contour line maps of that variable of interest. 
# The following set of code provides you with the all of the necessary 
# material to complete the lab. 
################################################################################

library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(mapsf)
library(spatstat)
library(gstat)
library(raster)
library(ggplot2)
library(scales)
library(akima)

################################################################################
################################################################################
################################################################################

#Here's some data that focuses on New Mexico
abq <- st_read("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 7/data/abq.shp")
tracts<-read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 7/data/census_data.csv")

abq_union <- st_union(st_geometry(abq))

#The other issue is that this dataset is in the 'long' format.
#We need it to be in the wide format. here's some good 
#Documentation on that: https://www.statology.org/long-vs-wide-data/
tracts<-tracts %>% 
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>% dplyr::select(c("GEOID", "NAME", contains('estimate'))) #just keep important variables
tracts$joinName<-gsub(",.*$", "", tracts$NAME)
abq<-abq %>% 
  left_join(tracts, by = c("NAMELSAD" = "joinName"))

#variable of interest
abq$Perc_over_18 = abq$estimate_DP05_0021/abq$estimate_DP05_0001
summary(abq$Perc_over_18) #This is the percent of the population 18 and over


################################################################################
#################### Interpolation Approaches ##################################
################################################################################

#First we  need to create a grid in which we want to assign
#interpolation values to. 
bbox <- st_bbox(abq) #This creates a bounding box around our study area
grd <- st_make_grid(st_as_sfc(bbox), cellsize = 0.0035) # This creates a grid of points. Adjust cellsize for different resolutions
my_sf_grid <- st_sf(geometry = grd)


#Now we need to find the centroid of our polygons. The interpolation
#algorithm will estimate values inbetween these centroids
centroids <- st_centroid(abq)

#you should receive a warning sign about the assumption of the 
#attribute being consistent over the geometry area. This should
#sound familiar as this is also an assumption on choropleth maps.

#here's how we do Inverse Distance Weighted Interpolation
idw_result <- gstat::idw(formula = Perc_over_18 ~ 1,  # Variable to interpolate
                         locations = centroids,        # Known points
                         newdata = my_sf_grid,         # Points or grid to predict
                         idp = 2,                     # Power parameter
                         nmax = 30,                   # Max number of points to use
                         maxdist = 1000)              # Max distance for points to influence
idw_sf <- st_as_sf(idw_result, coords = c("x", "y"), crs = 4326)

#plot our map 
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation Results", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Can definetly see a spatial pattern in the data but hard to say where
#That pattern is located in our study area. Let's clip our interpolation
#results

clipped_idw <- st_intersection(idw_sf, abq) #this might take a minute to run

ggplot() + 
  geom_sf(data = clipped_idw,aes(fill = var1.pred), color = NA, size = 0.6) +
  geom_sf(data = abq, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation Results", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Now that looks more like our study area. We'll tweak it later. 

################################################################################
#Let's look at kringing interpolation.
#First we need to build a variogram to understand how
#autocorrelated our variables are at different spatial distances. 
v <- variogram(Perc_over_18~1, abq)
plot(v) 


# Fit a variogram model
v <- variogram(Perc_over_18~1, abq)
plot(v)

v_model <- fit.variogram(v, model = vgm(psill = 0.0035, 
                                        "Sph", 
                                        range = 10, 
                                        nugget = 0.0012))

#Nugget: The semivariance at distance zero. 
#It represents the micro-scale variation or 
#measurement error. In your plot, 
#it would be the value where the model intersects 
#the y-axis at zero distance. If there's a visible 
#"jump" at the origin, that's your nugget effect.

#Sill: The plateau or maximum semivariance the model reaches, 
#representing the total variance of the dataset. 
#On your plot, it's the value where the semivariance levels off.

#Range: The distance at which the variogram reaches the 
#sill. Beyond this distance, there is no spatial autocorrelation.

utm_zone <- 12
centroids_utm <- st_transform(centroids, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
my_sf_grid_utm <- st_transform(my_sf_grid, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
abq_utm <- st_transform(abq, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))

kriging_result <- krige(formula = Perc_over_18~1, 
                        locations = centroids_utm, 
                        newdata = my_sf_grid_utm, 
                        model = v_model)

clipped_kring <- st_intersection(kriging_result, abq_utm) #this might take a minute to run


ggplot() + 
  geom_sf(data = clipped_kring,aes(fill = var1.pred), color = NA, size = 0.6) +
  geom_sf(data = abq_utm, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c() +
  labs(title = "Kringing Interpolation Results", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#These are not very good results. Furst us that nearly the entire
#map is the same value. However, if you were to use Rstudio's
#zoom feature, you'd see that there are some locations that have
#different values. Odd and weird looking. Odd and weird from 
#a data perspective. 


#here's a neat package that will automatically perform kriging
#interpolation. Let's see what it can find. This will take a 
#minute to run. 

library(automap)
auto_krige_output <-autoKrige(Perc_over_18~1, centroids_utm, new_data=my_sf_grid_utm) 
clipped_auto_krige <- st_intersection(auto_krige_output$krige_output, abq_utm) #this might take a minute to run


ggplot() + 
  geom_sf(data = clipped_auto_krige,aes(fill = var1.pred), color = NA, size = 0.6) +
  geom_sf(data = abq_utm, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c() +
  labs(title = "Auto Kriging Interpolation Results", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#That looks much better. 

#Let's add some contour lines and turn our map from continous 
#into a discrete map which we can do by setting up some breaks
#in the data

breaks_to_labels <- function(breaks) {
  # Convert to percentage and create labels
  labels <- paste0(
    sprintf("%.0f", head(breaks, -1) * 100), "% - ", 
    sprintf("%.0f", tail(breaks, -1) * 100), "%"
  )
  labels[length(labels)] <- paste0("> ", sprintf("%.0f", breaks[length(breaks) - 1] * 100), "%")
  return(labels)
}

# Define the breaks for binning the data. These are the thresholds for each bin.
breaks <- c(0, 0.05, 0.10, 0.15, 0.20, Inf)

# Create human-readable percent labels from the breaks using a custom function.
# This function is not provided in your code snippet but is assumed to create labels like "0-5%", "5-10%", etc.
percent_labels <- breaks_to_labels(breaks)

# Use the cut function to bin the 'var1.pred' column of clipped_auto_krige data into discrete categories based on the breaks.
# 'include.lowest = TRUE' ensures that the lowest value is included in the first bin.
clipped_auto_krige$binned_pred <- cut(clipped_auto_krige$var1.pred, breaks = breaks, include.lowest = TRUE)

# Aggregate the geometries in clipped_auto_krige by the 'binned_pred' bins.
# This groups the data by the binned prediction and then merges the geometries within each group into a single geometry.
# 'st_union' is used to dissolve boundaries between geometries in the same bin.
# 'ungroup()' is used to remove the grouping so that the data is no longer considered grouped by dplyr.
clipped_auto_krige_polygons <- clipped_auto_krige %>%
  group_by(binned_pred) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

# Determine the number of unique levels in the 'binned_pred' factor,
# which will be used to generate a corresponding number of colors.
num_levels <- length(levels(clipped_auto_krige$binned_pred))

# Generate a color palette with a number of colors equal to the number of bins.
# The 'Greens' palette from RColorBrewer is used for thematic consistency.
bin_colors <- RColorBrewer::brewer.pal(num_levels, "Greens")

# Convert the clipped_auto_krige data to a data frame and select only the necessary columns for interpolation.
krigingDF <- clipped_auto_krige %>% 
  data.frame() %>% 
  dplyr::select(c(x, y, var1.pred, var1.stdev))

# Use akima's interp function to perform interpolation on the krigingDF.
# This creates a grid of interpolated values based on the spatial data provided.
# 'duplicate = "strip"' argument deals with duplicate points by stripping them.
grid <- akima::interp(krigingDF$x, krigingDF$y, krigingDF$var1.pred, duplicate = "strip")

# Create a data frame from the interpolated grid to act as contour lines.
# This expands the grid into a long format where each row represents a point in the grid.
griddf <- data.frame(x = rep(grid$x, each = nrow(grid$z)), 
                     y = rep(grid$y, ncol(grid$z)), 
                     z = as.vector(grid$z))

# Remove any rows from griddf where z is NA because these cannot be plotted or contoured.
griddf <- griddf[!is.na(griddf$z),]

# Bin the z values from the grid based on the original breaks.
# These binned values will be used to color the contour plot.
griddf$binned_z <- cut(griddf$z, breaks = breaks, include.lowest = TRUE, right = FALSE)

# Convert the binned factor to a numeric value for plotting.
# This is needed because ggplot2 expects a numeric value for filled contours.
griddf$binned_z_num <- as.numeric(griddf$binned_z)

ggplot() + 
  geom_sf(data = clipped_auto_krige_polygons, aes(fill = binned_pred), color = 'white', size = 0.6) +
  geom_sf(data = abq_utm, fill = NA, color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Greens", 
                    name = "Population\n18 and Over",
                    labels = percent_labels) +
  labs(title = "Percentage of Albuquerque\nPopulation 18 and Over", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right"
  )

#let's break down the above code:
# Begin a ggplot object. This initializes the plotting system and sets up the canvas.
ggplot() + 
  # Add a layer to the plot for the 'clipped_auto_krige_polygons' sf object.
  # 'aes' defines the aesthetic mappings, in this case mapping the 'fill' of the geometries to the 'binned_pred' variable.
  # 'color' defines the color of the borders of the polygons, here set to white.
  # 'size' sets the thickness of the border lines of the polygons, here set to 0.6.
  geom_sf(data = clipped_auto_krige_polygons, aes(fill = binned_pred), color = 'white', size = 0.6) +
  
  # Add another layer for the 'abq_utm' sf object.
  # This will plot the base map, with no fill ('fill = NA') and black borders.
  # The size of the borders of the base map geometries is set to 0.5.
  geom_sf(data = abq_utm, fill = NA, color = "black", size = 0.5) +
  
  # Define the fill scale using Color Brewer's 'Greens' palette.
  # 'name' sets the title of the legend for the fill scale.
  # 'labels = percent_labels' specifies the labels for the legend based on the 'percent_labels' variable.
  # This assumes 'percent_labels' is a character vector with labels corresponding to the bins in 'binned_pred'.
  scale_fill_brewer(palette = "Greens", 
                    name = "Population\n18 and Over",
                    labels = percent_labels) +
  
  # Set labels for the plot: the main title and remove axis titles by setting them to NULL.
  labs(title = "Percentage of Albuquerque\nPopulation 18 and Over", x = NULL, y = NULL) +
  
  # Apply a minimalistic theme, which removes most of the background and gridlines for a clean plot.
  theme_minimal() +
  
  # Further customize the theme by:
  # - Removing major and minor grid lines ('panel.grid.major' and 'panel.grid.minor').
  # - Removing the text labels for both axes ('axis.text.x' and 'axis.text.y').
  # - Removing the axis titles ('axis.title.x' and 'axis.title.y').
  # - Setting the legend position to the right side of the plot ('legend.position').
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right"
  )


#That's pretty good. We can always change the density of our
#contour lines by changing the number of breaks in the data. 



##################################################################
##################################################################
##################################################################
library(elevatr)

#ABQ has a really clear elevation pattern. Let's check it out. 
eleCoords<-data.frame(x = st_coordinates(my_sf_grid)[,'X'],
                      y = st_coordinates(my_sf_grid)[,'Y'])
abq_elevation <- get_elev_point(eleCoords, prj = 4326, src = "epqs") #This will take a minute


#If you're choosing to create a relief map of elevation the elevation
#data is above in the 'abq_elevation' object. 

#####################
# QUESTIONS
#####################
#1.IDW strongly emphasizes the area around the points themselves whereas 
#  Kringing creates more of a smooth gradient in the prediction.
#2.Contour lines can help the reader determine elevation but if they are either 
#  too close or too far from each other, they won't really provide helpful
#  information. Elevation also could not really be relevant to the data being
#  presented.

#####################
# MY CODE
#####################
#1
#a.

utm_zone <- 12
centroids_utm <- st_transform(centroids, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
my_sf_grid_utm <- st_transform(my_sf_grid, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
abq_utm <- st_transform(abq, crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))

#ABQ has a really clear elevation pattern. Let's check it out. 
eleCoords<-data.frame(x = st_coordinates(my_sf_grid)[,'X'],
                      y = st_coordinates(my_sf_grid)[,'Y'])
abq_elevation <- get_elev_point(eleCoords, prj = 4326, src = "epqs") #This will take a minute

jpeg("Lab_7_abq_map.jpg")

ggplot() + 
  geom_sf(data = abq_elevation, aes(fill = elevation), color = NA, size = 0.6) +
  scale_fill_viridis_c() +
  labs(title = "Elevation of Abq., NM", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()
