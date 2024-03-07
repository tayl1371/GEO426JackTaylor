################################################################################
# Author: Jack Taylor
# Date: 02-22-2024
# Purpose: Lab 5 of Geography 426
# This lab's focus is on formally introducing color schemes in R and 
# exposing students to different projections. Of course we will create
# plots and save some of our images. 
################################################################################


library(sf)
library(dplyr)
library(RColorBrewer)
library(mapsf) #https://github.com/riatelab/mapsf/tree/69d76e3d3a49b12c02b16df458b0f04dac2b21cd

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

################################################################################
############################### Load Data ################################
################################################################################

mi_counties <- st_read("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 5/mi_counties/Counties_(v17a).shp")
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
marriages <- read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 5/MI_Mariage_rates_2022.csv")

mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
mi_counties$marriageExp <- state_rate * mi_counties$Population
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

head(mi_counties)
summary(mi_counties$Marriage.Number)

#Make some ordinal data
breaks <- c(-Inf, 40, 300, 600, 1500, Inf)
labels <- c("Very Low Marriages", "Low Marriages", 
            "Moderate Marriages", "High Marriages", 
            "Very High Marriages")
mi_counties$OrdinalMarriages<- cut(mi_counties$Marriage.Number, 
                                   breaks = breaks, 
                                   labels = labels, 
                                   include.lowest = TRUE, 
                                   ordered_result = TRUE)
table(mi_counties$OrdinalMarriages)


#Let's make some nominal data
mi_counties$CountyLetter<-substr(mi_counties$NAME, 1, 1)
table(mi_counties$CountyLetter)


################################################################################
######### Let's Look at a couple of approaches to color our Maps ###############
################################################################################

################################################################################
#The mapsf library package has its own approach to providing color
#Sequential Approach
cols <- mf_get_pal(n = 7, pal = "Reds 2") 
plot(1:7, rep(1, 7), bg = cols, pch = 22, cex = 4)

#Divergent Approach
cols <- mf_get_pal(n = c(3, 3), pal = c("Reds 2", "Blues"), neutral = "white")
plot(1:7, rep(1, 7), bg = cols, pch = 22, cex = 4)

#Divergent Approach
cols <- mf_get_pal(n = 4, pal = c("Reds 2", "Blues", "Green", "Orange"))
plot(1:4, rep(1, 4), bg = cols, pch = 22, cex = 4)

#Does not look to be any qualitative palettes - that's not good!


################################################################################
#One of the more widely used packages is the RColorBrewer
# Display all the palettes available in the RColorBrewer package
display.brewer.all() #Will likely need to zoom into the plot.

# Display all the palettes available in the RColorBrewer package
display.brewer.all()

# Sequential palettes
# Let's start by showing a few sequential palettes which are good for representing ordered data
display.brewer.pal(n = 7, name = "Blues")
display.brewer.pal(n = 7, name = "BuGn")
display.brewer.pal(n = 7, name = "OrRd")

# Divergent palettes
# These palettes are great for data that has a critical middle value, like temperature deviations
display.brewer.pal(n = 7, name = "RdBu")
display.brewer.pal(n = 7, name = "PiYG")
display.brewer.pal(n = 7, name = "BrBG")

# Qualitative palettes
# Finally, let's check out some qualitative palettes for categorical data
display.brewer.pal(n = 7, name = "Set1")
display.brewer.pal(n = 5, name = "Paired")
display.brewer.pal(n = 6, name = "Set3")

# One interesting feature of RColorBrewer is the ability to choose a palette
# that is colorblind safe, print friendly, and photocopy safe.
# Let's find a palette that is colorblind safe
brewer.pal.info[brewer.pal.info$category == "div",]

# Now let's pick a colorblind safe palette and show it
display.brewer.pal(n = 8, name = "RdBu")

# Another interesting feature is the ability to interpolate between colors in a palette
# to create a smooth gradient. This can be done using the colorRampPalette function
my_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)
image(1:100, 1, as.matrix(1:100), col = my_palette, xlab = "Interpolated 'Blues'", ylab = "", axes = FALSE)


#R has its own library of custom colors should you choose to ever take that route
head(colors())
length(colors()) #657 colors to choose from
#You can view a pdf version of the all the colors 
#here is one version: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#Here is another version - 
#though less legible in my opinion: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf

#What is going on in this code? 
my_color_names<-c("mistyrose", "mistyrose1", "mistyrose2", "mistyrose3")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
plot(1:length(my_color_names), rep(1, length(my_color_names)), 
     bg = my_colors, pch = 22, cex = 4)

#we can also create a nice table of color names and hexcodes
my_color_data<-data.frame('Color Names' = my_color_names,
                          'Hex Codes' = my_colors)
my_color_data


#Highly customized pallets can also be made in R
#Here's an example using RGB approach.
#Go to this website to find your RGB values: https://www.rapidtables.com/web/color/RGB_Color.html
ATblue <- rgb(26/255, 115/255, 186/255, 1)
ATyellow <- rgb(255/255, 219/255, 67/255, 1)
ATorange <- rgb(181/255, 75/255, 5/255, 1)

plot(1:3, rep(1, 3), bg = c(ATblue, ATyellow, ATorange), pch = 22, cex = 7)


################################################################################
######### Let's Look at a couple of approaches to color our Maps ###############
################################################################################
cols <- mf_get_pal(n = 5, pal = "Reds 2", rev = TRUE) 
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       breaks = "quantile",
       pal = cols,
       nbreaks = 5)


mfDiv <- mf_get_pal(n = c(3, 3), pal = c("Reds 2", "Blues"), neutral = "white")
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       pal = mfDiv) #need to add some custom breaks to this map

breaks <- div_breaks(mi_counties$relRisk, 7, 100)
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       pal = mfDiv,
       breaks = breaks)

mp7 <- brewer.pal(7, "PiYG") #Color Brewer Approach
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       pal = mp7)#Need add custom breaks again


mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       breaks = breaks,
       pal = mp7)

#color scheme seems like red should be where more risk is. Let's reverse our color brewer
mp7 <- brewer.pal(7, "PiYG")
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       breaks = breaks,
       pal = mp7) #That looks much better! 

mp7<-rev(mp7) #Rev short for reverse
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       breaks = breaks,
       pal = mp7) #That looks much better!



#Lets try my custom palette that I made
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors)) 
#the above map looks pretty awful - it could be enhanced with better
#saturation of color. Question 1 in 'Your turn to Code' represents
#this issue. 


#Let's look at ordinal marriages
ordinalBrewer <- brewer.pal(n = 7, name = "Set1")
mf_map(mi_counties, 
       var = "OrdinalMarriages",
       type = "choro",
       pal = ordinalBrewer) 
#The above produced an error: "Error in classInt::classIntervals(var = x, n = nbreaks, style = breaks,  : 
#var is not numeric". This is because we need to change our 'type' settings
#to 'typo'.
mf_map(mi_counties, 
       var = "OrdinalMarriages",
       typo = "choro",
       pal = ordinalBrewer) 
#This ordinal based map does not look correct - can you change it?

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
#################################### Projects in R #############################
################################################################################
world <- st_read("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 5/ne_110m_admin_0_countries")

#We can use the print statement to identify what, if any, projection the sf object has
print(world$geometry) #This should show us several pieces of important geogrpahic infomration

#you should see that there is only a geodetic coordinate system

#We can provide our sf object with a projection through a couple of approaches
#1. Using EPSG identifiers
#2. Using ESRI based approaches
#3. Using Proj4String
#All three of these approaches have pros and cons. You'll need to answer 
#the pros and cons in the lab. For the below we'll focus on the EPSG and Proj4string approaches
#Let's save these side-by-side plots too

jpeg("C:/Users/joshu/OneDrive/Documents/Geog 426/Vertalka_Material/Labs/Lab 5/world_projection_example.jpeg", quality = 1200, width = 1500, height = 1200)
par(mfrow = c(1, 2))
plot(st_geometry(world))

#EPSG Approach
world<-st_transform(world, 3857) #These four digits represent EPSG of 3857 which is a Web Mercator projection
plot(st_geometry(world))
dev.off()

#Notice what happened? Instead of our images displaying in the 'Plots' window they are now saved 
#To the jpeg file. This happens when 'jpeg', 'png', 'pdf' and other image saving function get called. 
# we can call and view our plots to the 'Plots' window in Rstudio. 
par(mfrow = c(1, 2))
plot(st_geometry(st_transform(world, 4386)))
plot(st_geometry(st_transform(world, 3857)))


#Proj4 string approach
webMercator<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"
world<-st_transform(world, webMercator)


#We can look at Lamber Conformal Conic Projection
world<-st_transform(world, 3082)
plot(st_geometry(world))

#We can look at Albert Equal Area Projection focused on Texas. Doesn't make a whole lot of sense.
world<-st_transform(world, 3085)
plot(st_geometry(world))

#We can look at Albert Equal Area Projection focused on North America Doesn't make a whole lot of sense.
world<-st_transform(world, 5070) 
plot(st_geometry(world))

#Here's World Equidistant Cylindrical Map
world<-st_transform(world, 4087) 
plot(st_geometry(world))
#Do you notice the lines going across the maps? 
#This is because a polygon is being split to both sides of the map. 
#For example Alaska is stretching across the the Northern latitudes 
#so part of its polygons are on the left side of the map and some are
#on the right side of the map. 

#Let's look at the United States 
US<-world %>% filter(NAME == "United States of America")
plot(st_geometry(US))

#Let's go back to Albert's Equal Area
US<-st_transform(US, 5070) 
plot(st_geometry(US))

#You can play around with more projections by going to https://epsg.io/
#Search for World and select any of the options that come up. 
#Scroll down to the bottom and you can see different options 
#to export your projection. Take a look at a few of them. 

#We can also add graticules to the map 
dev.off()
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
world = st_transform(world, robinson)
graticule = st_graticule(lat=seq(-80, 80, 10), lon=seq(-180, 180, 10))
graticule = st_transform(graticule, robinson)
plot(st_geometry(world))
plot(graticule$geometry, col="#00000040", add=T)

st_bbox(US)
graticule = st_graticule(lat=seq(-18.91619, 71.35776, 3), lon=seq(-171.79111, 66.96466, 3))
graticule = st_transform(graticule, 5070)
plot(st_geometry(US))
plot(graticule$geometry, col="#00000040", add=T)

#####################
# QUESTIONS
#####################
#1. Hue refers to the actual color, saturation refers to the intensity of the 
#   color, and lightness refers to the shade of the color. You might want to 
#   change color if the data is ordinal, you might want to change saturation if
#   the data is ordinal, and you might want to change lightness if the map will 
#   be in greyscale.
#2. Red: 255, 0, 0; Green: 0, 255, 0; Blue: 0, 0, 255
#3. The EPSG projection codes don't require extra work to implement in R but 
#   there is a smaller number of them. The ESRI projection codes require more 
#   work to implement in R but there is a larger number of them.
#4. An EPSG value appropriate for the state of Michigan would be EPSG:5070.

#####################
# MY CODE
#####################
#1.
my_colors_new <- c("#ffe4e1", "#ffada4", "#ee6d5d", "#cd483c")

jpeg("Lab_5_pic_1.jpeg")
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors_new,
       nbreaks = length(my_colors_new))
dev.off()

#2. 
cols <- mf_get_pal(n = 4, pal = "Blues 2", rev = TRUE) 
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       breaks = "quantile",
       pal = cols,
       nbreaks = 4)
  
#3. 
michigan_albers <- st_transform(mi_counties, 5070)
plot(st_geometry(michigan_albers))

#4. 
lithuania <- world %>% filter(NAME == "Lithuania")
lithuania_lks94 <- st_transform(lithuania, 3346)

jpeg("Lab_5_pic_2.jpeg")
par(mfrow = c(1, 2))
plot(st_geometry(lithuania), main="WGS84")
plot(st_geometry(lithuania_lks94), main="LKS94")
dev.off()

#5.
cols <- mf_get_pal(n = 5, pal = "Dark Mint", rev = TRUE)
mf_map(michigan_albers, 
       var = "relRisk",
       type = "choro",
       breaks = "quantile",
       pal = cols,
       nbreaks = 5)

