################################################################################
# Author: Josh Vertalka
# Date: 2-15-2024
# Purpose: Lab 6 of Geography 426
# This lab's focus is on creating choropleth, proportional symbol, and dot
# density maps. Since you are gaining more R skills, this lab will require more
# out of you. Please apply cartographic principles as needed. 
################################################################################

library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(mapsf)
library(ggplot2)

################################################################################
################################################################################
################################################################################

#Here's some data that focuses on New Mexico
abq <- st_read('/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 6/data/abq.shp')
tracts<-read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 6/data/census_data.csv")

abq_union <- st_union(st_geometry(abq))

#The other issue is that this dataset is in the 'long' format.
#We need it to be in the wide format. here's some good 
#Documentation on that: https://www.statology.org/long-vs-wide-data/
tracts<-tracts %>% 
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>% select(c("GEOID", "NAME", contains('estimate'))) #just keep important variables


#We need to clean the name field of the tracts dataset so that we can join it to our shapefile
tracts$joinName<-gsub(",.*$", "", tracts$NAME)

#Join data to abq shapefile
abq<-abq %>% 
  left_join(tracts, by = c("NAMELSAD" = "joinName"))

#Let's load some Facebook check-in data
fb<-read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 6/data/facebook_cleaned.csv")

#This data is still a bit messy
fb<-fb[!duplicated(fb),] #This will drop duplicates in the dataset.
head(fb)

table(fb$category)

#Let's just keep 15 random check-in locations
set.seed(1234)
fb<-fb %>% 
  filter(checkins > 50) %>% 
  sample_n(15)

fb_shp <- st_as_sf(fb, coords = c("lng", "lat"), crs = 4326)

#let's filter our data so that only data points that fall within abq are used
fb_shp <- fb_shp %>% 
  st_transform(st_crs(abq)) %>% 
  filter(st_intersects(geometry, st_geometry(abq_union), sparse = FALSE)[,1])


################################################################################
################################################################################
################################################################################


#2. Let's look at a proportional symbol map
plot(st_geometry(abq))
mf_map(x = fb_shp, var = "checkins", type = "prop", leg_pos = "topright")

#The above is not a great map - let's tweak some settings
plot(st_geometry(abq))
mf_map(x = fb_shp,
       var = "checkins", 
       type = "prop", 
       leg_pos = "topright",
       inches = 0.2)

#Still isn't a great map - can you make it better?

#Let's try a different library package - ggplot2
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  geom_sf(data = fb_shp, aes(size = checkins), color = "darkblue", alpha = 0.7) +  # Darker blue color
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  # Adjust these breaks as needed
                        labels = c("2.5k", "5k", "7.5k")) +  # Add custom labels if desired
  guides(size = guide_legend(override.aes = list(colour = "darkblue"), title = "Check-ins", 
                             nrow = 1, byrow = TRUE, direction = "horizontal", 
                             label.position = "bottom")) +  # More symbols and horizontal layout
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels to reduce clutter

#I'm going to tweak things a bit more with some symbol borders and 
#transparency

# Extract coordinates and also include 'checkins' data
fb_shp_coords <- cbind(as.data.frame(st_coordinates(fb_shp)), checkins = fb_shp$checkins)

# Now, you can plot using ggplot2 without encountering the 'object not found' error
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "black", fill = "darkblue", alpha = 0.5, stroke = 1.2) +  # Proportional symbols with transparency
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "darkblue", color = "black", alpha = 0.5, stroke = 2), 
                             title = "Check-ins", 
                             nrow = 1, byrow = TRUE, direction = "horizontal", 
                             label.position = "bottom")) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#I'm going to tweak the borders more
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "black", fill = NA, stroke = 1.2) +  # Black border with no fill
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, fill = "darkblue", alpha = 0.4) +  # Semi-transparent blue fill
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "darkblue", color = "black", alpha = 0.4, stroke = 1.2))) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


#that's a lot better but that's a lot of code!

#Let's break down the ggplot functions.
# Initialize a ggplot object. This is the starting point for any plot made with ggplot2.
ggplot() +
  
  # Add a layer to the plot for the base map using geometries from the 'abq' dataset.
  # The fill color for the polygons is set to light grey, and the polygon borders are colored black.
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  
  # Add a layer of points to the plot. The size of each point is determined by the 'checkins' variable.
  # This first set of points creates a solid black border for each symbol with no fill (fill = NA).
  # The stroke size is set to 1.2, making the border of the points clearly visible.
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "black", fill = NA, stroke = 1.2) +  # Black border with no fill
  
  # Add another layer of points on top of the borders created above. These points have a dark blue fill
  # with an alpha transparency of 0.4, allowing the base map to show through.
  # No stroke color is defined here, so these points do not have a border.
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, fill = "darkblue", alpha = 0.4) +  # Semi-transparent blue fill
  
  # Define the scale for the 'size' aesthetic, controlling the size of the points.
  # The 'range' argument sets the minimum and maximum sizes for the points.
  # 'breaks' and 'labels' define the points at which labels should appear on the legend,
  # with corresponding human-readable labels.
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  
  # Customize the legend for the point sizes to reflect the appearance of the points on the plot,
  # including the fill, border color, transparency, and stroke width.
  guides(size = guide_legend(override.aes = list(fill = "darkblue", color = "black", alpha = 0.4, stroke = 1.2))) +
  
  # Apply a minimalistic theme, which removes much of the background and gridlines for a clean look.
  theme_minimal() +
  
  # Add labels to the plot, including the legend title ('Check-ins') and the main title.
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  
  # Further customize the plot's theme. Specifically, move the legend to the bottom of the plot
  # and rotate the x-axis text for better readability and to prevent overlapping.
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Play around with these functions and come up with a version of your own 
#Proportional symbol map. Here's some additional resources to help
# https://jsimkins2.github.io/geog473-673/spatial-plots-with-ggplot2.html
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf



################################################################################
################################################################################
################################################################################

#Let's bring our FB data back in
fb<-read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 6/data/facebook_cleaned.csv")
fb<-fb[!duplicated(fb),] 
set.seed(1234)
fb_checkins<-fb %>% 
  filter(checkins > 0) %>% 
  select(c(lng, lat, checkins)) %>% 
  rename('X' = 'lng', 'Y' = 'lat')



# Perform a kernel density estimation
kde <- with(fb_checkins, MASS::kde2d(x = X, y = Y, n = 500, lims = c(min(X), max(X), min(Y), max(Y))))

# Convert the density estimate into a long format data frame
density_data <- expand.grid(X = kde$x, Y = kde$y)
density_data$Z <- as.vector(kde$z)

# Sample points based on density
set.seed(123) # For reproducibility
sampled_points <- density_data %>% 
  slice_sample(weight_by = Z, n = 3000) # Adjust n for the number of points

#We need to clip our sample to our abq boundary and keep only the X and Y coords.
sampled_points<-sampled_points %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(st_crs(abq)) %>% 
  filter(st_intersects(geometry, st_geometry(abq_union), sparse = FALSE)[,1]) %>% 
  st_coordinates() %>% 
  as.data.frame()


# Plot
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +
  geom_point(data = sampled_points, aes(x = X, y = Y), 
             size = 0.5, alpha = 0.8, color = "darkblue") +
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map")


#Not a horrible map but could use some TLC. 

# Correct approach to include 'checkins' data with coordinates for plotting
fb_shp_coords <- cbind(as.data.frame(st_coordinates(fb_shp)), checkins = fb_shp$checkins)

# This ensures 'checkins' is available in 'fb_shp_coords' for ggplot2 plotting
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), shape = 21, color = "black", fill = "darkblue", alpha = 0.5, stroke = 1.2) +
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "darkblue", color = "black", alpha = 0.5, stroke = 2), 
                             title = "Check-ins", 
                             nrow = 1, byrow = TRUE, direction = "horizontal", 
                             label.position = "bottom")) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


#you might have to use the 'zoom' feature in Rstudio for this to look better.

#Let's see if we can come up with a situation where 1 dot equals 500 check-ins

fb_checkin_capped<-fb %>% 
  filter(checkins > 0) %>% 
  select(c(lng, lat, checkins)) %>% 
  mutate(dot_count = ceiling(checkins / 500)) %>% 
  rename('X' = 'lng', 'Y' = 'lat') %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>% 
  st_transform(st_crs(abq)) %>% 
  filter(st_intersects(geometry, st_geometry(abq_union), sparse = FALSE)[,1]) %>% 
  as.data.frame()


# Create a new data frame where each row represents a dot
dots_data <- fb_checkin_capped[rep(1:nrow(fb_checkin_capped), fb_checkin_capped$dot_count), ] %>%
  mutate(
    X = X + runif(n(), min = -0.0001, max = 0.0001),  # Adjust these values based on your coordinate system and desired spread
    Y = Y + runif(n(), min = -0.0001, max = 0.0001)
  )
# Plot the map with these dots
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y), 
             size = 0.5, color = "darkblue") +  # Solid blue dots
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 500 check-ins") +  # Add caption to explain dot representation
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.caption = element_text(hjust = 0.5)  # Center the caption
  )

#That's interesting! 


#There looks to be different types of businesses associated with the dot-density
#Let's check those out. 
business_groups <- c(
  "SPORTS/RECREATION/ACTIVITIES" = "Entertainment/Leisure",
  "REAL ESTATE" = "Professional/Financial Services",
  "NON-PROFIT ORGANIZATION" = "Community Services",
  "LOCAL BUSINESS" = "Retail/Industry",
  "LAWYER" = "Professional/Financial Services",
  "RESTAURANT/CAFE" = "Entertainment/Leisure",
  "PROFESSIONAL SERVICES" = "Professional/Financial Services",
  "SCHOOL" = "Education/Government",
  "GOVERNMENT ORGANIZATION" = "Education/Government",
  "HEALTH/MEDICAL/PHARMACY" = "Health/Wellness",
  "SHOPPING/RETAIL" = "Retail/Industry",
  "ARTS/ENTERTAINMENT/NIGHTLIFE" = "Entertainment/Leisure",
  "LEGAL/LAW" = "Professional/Financial Services",
  "HOME IMPROVEMENT" = "Retail/Industry",
  "CHURCH/RELIGIOUS ORGANIZATION" = "Community Services",
  "HOSPITAL/CLINIC" = "Health/Wellness",
  "PET SERVICES" = "Retail/Industry",
  "DOCTOR" = "Health/Wellness",
  "EVENT PLANNING/EVENT SERVICES" = "Professional/Financial Services",
  "MOVIE THEATER" = "Entertainment/Leisure",
  "TRAVEL/LEISURE" = "Entertainment/Leisure",
  "BAR" = "Entertainment/Leisure",
  "EDUCATION" = "Education/Government",
  "SPAS/BEAUTY/PERSONAL CARE" = "Health/Wellness",
  "LANDMARK" = "Entertainment/Leisure",
  "ORGANIZATION" = "Community Services",
  "CLUB" = "Entertainment/Leisure",
  "ATTRACTIONS/THINGS TO DO" = "Entertainment/Leisure",
  "HOTEL" = "Entertainment/Leisure",
  "CONCERT VENUE" = "Entertainment/Leisure",
  "INTERNET/SOFTWARE" = "Professional/Financial Services",
  "MUSEUM/ART GALLERY" = "Entertainment/Leisure",
  "PUBLIC PLACES" = "Community Services",
  "COMMUNITY ORGANIZATION" = "Community Services",
  "HEALTH/BEAUTY" = "Health/Wellness",
  "ENERGY/UTILITY" = "Professional/Financial Services",
  "FOOD/BEVERAGES" = "Retail/Industry",
  "COMMUNITY/GOVERNMENT" = "Education/Government",
  "RADIO STATION" = "Media/Communications",
  "MEDIA/NEWS/PUBLISHING" = "Media/Communications",
  "RETAIL AND CONSUMER MERCHANDISE" = "Retail/Industry",
  "FOOD/GROCERY" = "Retail/Industry",
  "LIBRARY" = "Education/Government",
  "BOOK STORE" = "Retail/Industry",
  "UNIVERSITY" = "Education/Government",
  "HEALTH/MEDICAL/PHARMACEUTICALS" = "Health/Wellness",
  "COMPUTERS/TECHNOLOGY" = "Professional/Financial Services",
  "FARMING/AGRICULTURE" = "Retail/Industry",
  "COMPANY" = "Professional/Financial Services",
  "BANK/FINANCIAL INSTITUTION" = "Professional/Financial Services",
  "AUTOMOTIVE" = "Retail/Industry",
  "BUSINESS SERVICES" = "Professional/Financial Services",
  "SMALL BUSINESS" = "Retail/Industry",
  "NON-GOVERNMENTAL ORGANIZATION (NGO)" = "Community Services",
  "CONSULTING/BUSINESS SERVICES" = "Professional/Financial Services",
  "SPORTS VENUE" = "Entertainment/Leisure",
  "TRANSPORTATION" = "Professional/Financial Services",
  "ENGINEERING/CONSTRUCTION" = "Professional/Financial Services",
  "TOURS/SIGHTSEEING" = "Entertainment/Leisure",
  "POLITICAL ORGANIZATION" = "Community Services",
  "RECORD LABEL" = "Media/Communications",
  "MIDDLE SCHOOL" = "Education/Government",
  "AUTOMOBILES AND PARTS" = "Retail/Industry",
  "BANK/FINANCIAL SERVICES" = "Professional/Financial Services",
  "AEROSPACE/DEFENSE" = "Professional/Financial Services",
  "OUTDOOR GEAR/SPORTING GOODS" = "Retail/Industry",
  "TRANSPORT/FREIGHT" = "Professional/Financial Services",
  "SPORTS EVENT" = "Entertainment/Leisure",
  "INSURANCE COMPANY" = "Professional/Financial Services",
  "INDUSTRIALS" = "Retail/Industry",
  "TRANSIT STOP" = "Education/Government",
  "TELECOMMUNICATION" = "Professional/Financial Services",
  "AIRPORT" = "Professional/Financial Services",
  "BIOTECHNOLOGY" = "Health/Wellness",
  "MOUNTAIN" = "Entertainment/Leisure",
  "TV NETWORK" = "Media/Communications",
  "PATIO/GARDEN" = "Retail/Industry"
)

dots_data$grouped_business_type <- business_groups[dots_data$checkins]
dots_data$grouped_business_type[is.na(dots_data$grouped_business_type)] <- "Other"

dots_data<-dots_data %>% 
  filter(grouped_business_type != "Other")

num_categories <- length(unique(dots_data$grouped_business_type))
color_palette <- brewer.pal(num_categories, "Set1")
colors <- setNames(color_palette, unique(dots_data$grouped_business_type))

# Plot the dot density map with colored dots based on 'grouped_business_type'
ggplot() +
  geom_sf(data = abq, fill = "lightgrey", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y, color = grouped_business_type), 
             size = 0.5) +  # Color dots based on 'grouped_business_type'
  scale_color_manual(values = colors) +  # Use the ColorBrewer palette
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Adjust symbol size in legend
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 500 check-ins",
       color = "Business Type") +  # Label for the color legend
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.caption = element_text(hjust = 0.5)  # Center the caption
  )

# Better, it's just not a cool looking map! 


################################################################################
################################################################################
################################################################################

# 1. You've been exposed to how to make a choropleth map. Please make
# a choropleth map with the data provided. Please use the 'mapsf' package.


ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP05_0066), color = "white") +  
  scale_fill_viridis_c() +  # Use a color scale that's perceptually uniform
  labs(title = "Choropleth Map", fill = "Value") +  # Add titles and labels
  theme_minimal() +  # Use a minimal theme
  theme(legend.position = "bottom")  # Position the legend at the bottom

# 2. Immediately above I have a very basic choropleth map. Your job is to 
# apply the principals that you learned to make the map more visually appealing, 
# easier to read, and simple. 

#####################
# QUESTIONS
#####################
#1. You should use a choropleth map if the data is evenly distributed over the 
#   enumeration unit. You should use a dot density map if the data is not evenly
#   distributed.
#2. The ggplot2 library is used to create highly customization graphics. It might
#   be more useful than the mf_map function in the mapsf package as you can
#   customize the maps in more detail and also use package to make clean,
#   professional looking graphs.

#####################
# MY CODE
#####################
#1.
jpeg("Lab_6_proportional_symbol.jpeg")

ggplot() +
  geom_sf(data = abq, fill = "white", color = "black") +  # Base map
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "darkgrey", fill = NA, stroke = 1.2) +  # Black border with no fill
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, fill = "lightblue3", alpha = 0.4) +  # Semi-transparent blue fill
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "lightblue3", color = "darkgrey", alpha = 0.4, stroke = 1.2))) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Albuquerque Facebook Check-ins") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
)

dev.off()

#2. 
jpeg("Lab_6_dot_density.jpeg")

ggplot() +
  geom_sf(data = abq, fill = "white", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y), 
             size = 0.5, color = "lightblue3") +  # Solid blue dots
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 500 check-ins") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
)

dev.off()

#3. 
jpeg("Lab_6_choropleth_mapsf.jpeg")

mf_map(x = abq, 
       var = "estimate_DP02_0025",
       type = "choro",
       leg_title = "Estimate of DP02_0025 in Albuquerque")

dev.off()

#4.
jpeg("Lab_6_choropleth_ggplot2.jpeg")

ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP02_0025), color = "black") +  
  scale_fill_viridis_c() +  # Use a color scale that's perceptually uniform
  labs(title = "Choropleth Map of DP02_0025", fill = "Estimation\nof DP02_0025") +
  theme_minimal() +
  theme(legend.position = "right")

dev.off()

