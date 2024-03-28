################################################################################
# Author: Josh Vertalka
# Date: 2-15-2024
# Purpose: Lab 8 of Geography 426
# This lab's focus is on using creating a cartogram map of ABQ. You will 
# Then you will then edit the map in Adobe Illustrator.  
################################################################################
library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(raster)
library(ggplot2)
library(cartogram)
library(svglite)
################################################################################
################################################################################
################################################################################

#Here's some data that focuses on New Mexico
abq <- st_read("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 8/data/abq.shp")
tracts<-read.csv("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 8/data/census_data.csv")

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
############################## Cartogram Map ###################################
################################################################################

#Let's reproject our data
abq <- st_transform(abq, crs = 26913) #This is the UTM for ABQ. 
#This projection will preserve distance and area

#This function will set us up for a cartogram. It will 
#warp the geometry of the shapefile based on the data input. 
abq_cartogram <- cartogram_cont(abq, "estimate_DP05_0001", itermax=5)

par(mfrow = c(1, 2))
plot(st_geometry(abq), main = 'Original ABQ Geometry \n on Population')
plot(st_geometry(abq_cartogram), main = 'Cartogram ABQ Geometery \n on Population')

quantile_breaks <- quantile(abq_cartogram$estimate_DP05_0001, probs = seq(0, 1, length.out = 6), na.rm = TRUE)

my_cartogram<-ggplot() +
  geom_sf(data = abq_cartogram, aes(fill = estimate_DP05_0001), color = "black") +
  theme_void() +
  scale_fill_distiller(palette = "Greens",
                       name = "Population\n18 and Over",
                       breaks = quantile_breaks,
                       labels = paste0(quantile_breaks, " People"), # Customize labels as needed
                       guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Population\n18 and Over")

#There's a lot of things wrong with this map. 
plot(my_cartogram)

#Let's save it as an svg object so that we can import it into Adobe Illustrator
ggsave("/Users/jacktaylor/Documents/College/Senior/Semester II/GEO 426/GEO426JackTaylor/Lab 8/my_plot.svg", plot = my_cartogram, width = 10, height = 8, units = "in")



