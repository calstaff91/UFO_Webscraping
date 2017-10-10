# TWAU Hack Day - UFO Sightings Choropleth Map
# Isi Avbulimen
# October 2017

# install.packages("readr")
# install.packages("dplyr")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("classInt")
# install.packages("RColorBrewer")

# Load libraries
library(readr) # for reading and writing CSV files
library(dplyr) # for working with data

# Spatial libraries
library(maptools) # allows R to handle geographic data
library(rgdal) # Reprojects data between co-ordinate systems (e.g. lat/long to BNG)
library(rgeos) # allows spatial overlay

# Other libraries you will need
library(classInt) # defines intervals for data sets
library(RColorBrewer) # defines colour palettes

# you may have to install some of these packages if they're not already installed


# SHAPE FILES ---------------------------------------------------------
# These give the shape of the map 

# Load in the shapefiles from the directory where they are saved
States <- rgdal::readOGR("Shapefiles","states")

# Use plot to see blank shape file i.e. the outlines of the map
plot(States)

# DRAW A SIMPLE CHLOROPLETH MAP -----------------------------------------------------------------


# Read in your data for the map e.g. my_data
US_data <- read.csv("Data/US_random_data.csv")

# N.B. your variable needs to be numeric
US_data$Percentage <- as.numeric(US_data$Percentage)


##### 1st you need to Join the data you would like to the shape file 
# This is done using the data slot
# slots are used to store information - data, polygons, plotOrder, bbox, proj4string
# To access a slot you use the @ sign after the object name

# The data slot acts like a data frame and stores all the data connected with the shape file
# Use head() to look at the top six lines of the shapefile
head(States@data)

# We join the data by State Abbreviation 
States@data <- left_join(States@data, US_data, by = c("STATE_ABBR" = "Official_USPS_Code"))


#use head() to check the top lines of the data again
head(States@data)


#### 2nd create the intervals you would like to map the data over

# Extract variable you are mapping call it 'var'
var <- States@data[,"Percentage"]

# Find intervals
# set your desired number of breaks e.g. n=5
# use your preferred method to create the intervals i.e.
# "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
my_breaks <- classIntervals(var, n=5, style = "fisher")

# check out the breaks which your choosen interval method has created
my_breaks$brks

# if you want you can compare your interval method to another method or create your own breaks
# equal breaks
my_breaks_equal <- classIntervals(var, n=5, style = "equal")
# tip: look at the range of your variable to decide where your fixed breaks could be

#check out the breaks which you have created
my_breaks_equal$brks

# Have a closer look distribution of the variable and the breaks you have defined using abline
# this way you can see how much data is falling into each interval for each method
hist(var)
abline(v=my_breaks$brks, col="blue")
abline(v=my_breaks_equal$brks, col="red")

##### 3rd Select the correct number of colours from the palette

#view the different palettes available
brewer.pal.info

#select your palette with the number colours equal to number of breaks
my_colours <- brewer.pal(5, "Blues")

#### 4th - Now you can plot your map

# Here, I'm plotting using the 'fisher' breaks 
plot(States, col=my_colours[findInterval(var, my_breaks$brks, all.inside = TRUE)], axes=FALSE, border="black")

#### 5th - Add a legend
legend(x=-75,y=70, legend=leglabs(my_breaks$brks, under="Under", over="Over"), 
       fill=my_colours, border="black", 
       bty="n", cex=0.8, title="legend_title")

# Add a "tidier" legend
# This one rounds your values up
# If you add this it will appear on top of any legend which has already been drawn so
# you will need to re-draw the map
plot(States, col=my_colours[findInterval(var, my_breaks$brks, all.inside = TRUE)], axes=FALSE, border="black")

legend(x=-75,y=70, legend=leglabs(round(my_breaks$brks,0), under="Under", over="Over"), 
       fill=my_colours, border="black", 
       bty="n", cex=0.8, title="legend_title")

#### 6th Add title
title("Plot title")
# using '\n' will put text after it on the line below 


###############################

#Alternative legend 


#Create legend content
legend_labels <- c("5 - 27", 
                   "27 - 45",
                   "45 - 67",
                   "67 - 83",
                   "83 - 99")
legend_colours <- c('#EFF3FF', 
                    '#BDD7E7',
                    '#6BAED6',
                    '#3182BD',
                    '#08519C') 
#white - #FFFFFF

#Plot legend
legend(x=-75, y=70, legend=legend_labels, fill=legend_colours, 
       bty="n", #type of box drawn around legend
       x.intersp = 0.3, #horizontal spacing of legend
       y.intersp = 0.6, #vertical spacing
       cex = 0.8, #size of text
       title="Legend title")

#to play around with the position of the legend, in the plot set axes=TRUE in order to see the axes

#############################################
#Labels

#### Add all labels ####

# Find the centroid for each LA 
centroids <- gCentroid(States, byid=TRUE)

# Add all labels using text which takes the centroid coordinates and state names as inputs
text(centroids@coords, labels= States@data$STATE_ABBR)



##### Add a subset of labels ###

# This chooses the rows in the coordinate dataframe where the variable in the data slot is over 50
filtered_centroid <- centroids@coords[which(States@data$Percentage>50),]

# This chooses the equivalent names and makes them characters
filtered_data <- States@data$STATE_ABBR[which(States@data$Percentage>50)]
filtered_data <- as.character(filtered_data)

# Plot map as usual
plot(States, col=my_colours[findInterval(var, my_breaks$brks, all.inside = TRUE)], axes=FALSE, border="black")

# Add the resulting labels
text(filtered_centroid, filtered_data)



#### Add your own chosen labels ####

# Create vector with labels you want
chosen_states <- c("AK", "HI", "CA")

# chooses relevant rows in the coordinate dataframe where states are within those chosen
chosen_centroid <- centroids@coords[which(States@data$STATE_ABBR %in% chosen_states),]

# This chooses the equivalent names and makes them characters
chosen_data <- States@data$STATE_ABBR[which(States@data$STATE_ABBR %in% chosen_states)]
chosen_data <- as.character(chosen_data)

# Plot map as usual
plot(States, col=my_colours[findInterval(var, my_breaks$brks, all.inside = TRUE)], axes=FALSE, border="black")

# Add the resulting labels
text(chosen_centroid, chosen_data)
