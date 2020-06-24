library(leaflet)
library(leaflet.minicharts)
library(shinyWidgets)
library(rgeos)
library(rworldmap)
library(shiny)
library("rworldxtra")
library(lubridate)
library(data.table)
library(tidyr)
library(dplyr)

# Set the working directory so that file containing the SARS data labeled sars_2003_complete_dataset_clean.csv may be read in conveniently.
# getwd()
setwd("C:/Users/hp-user/Desktop/Visualizing SARS/STAT FInal Project Report")

# Read in the dataset
sars1 <- read.csv("sars_2003_complete_dataset_clean.csv", header = TRUE,stringsAsFactors = FALSE, ",")
#head(sars1)

# Get the number of unique countries in the data
number <- unique(sars1$Country)

# Get the world map to extract the centroid coordinates and polygons of the countries that will be used to plot charts of the SARS data on the world map.
# Get the World Map data in high resolution, note alternatively that low resolution can be selected to improve speeds,
# at the cost of having the polygon data representing the countries being of lower resolution and thus appearing distorted.
wmap <- getMap(resolution="high")
#head(wmap)

# Examine the names of the countries contained in the wmap.
#wmap$NAME

# This centroids dataframe will be crucial as it forms the coordinates on the world map to plot pie charts of the distribution and composition of deaths, cases and recovered.
centroids <- gCentroid(wmap, byid=TRUE)
df_centroids <- as.data.frame(centroids)
df_centroids$Country <- row.names(df_centroids)
#head(df_centroids)
#colnames(df_centroids)
# Rename the centroids dataframe to reflect the longitudes and lattitudes of the data.
names(df_centroids) <- c("Long","Lat","Country")
#head(df_centroids)
#head(world)
### use the world data and extract all countries as found in the SARS dataset
#class(number)
number <- as.character(number)


# Some filtering is required on the original sars1 data.
sars_filtered <- sars1
#head(sars1)
# Rename the countries to fit the names found in the world data that contains the polygons that we will use to overlay the selected countries.
sars_filtered$Country[sars_filtered$Country == "Republic of Ireland"] <- "Ireland"
sars_filtered$Country[sars_filtered$Country == "Viet Nam"] <- "Vietnam"
sars_filtered$Country[sars_filtered$Country == "Taiwan, China"] <- "Taiwan"
sars_filtered$Country[sars_filtered$Country == "Russian Federation"] <- "Russia"
sars_filtered$Country[sars_filtered$Country == "Hong Kong SAR, China"] <- "Hong Kong"
sars_filtered$Country[sars_filtered$Country == "Macao SAR, China"] <- "Macau"
sars_filtered$Country[sars_filtered$Country == "Republic of Korea"] <- "S. Korea"

# Filter the countries in the world dataframe to be those only found in the sars data.
number <- unique(sars_filtered$Country)
#number


world_filtered <- subset(wmap,wmap$NAME %in% number)
#world_filtered
#world_filtered$NAME
# Filter the centroids to be those only found in the sars data.
df_centroids$Country[df_centroids$Country == "United States of America"] <- "United States"
df_centroids$Country[df_centroids$Country == "South Korea"] <- "S. Korea"
df_centroids$Country[df_centroids$Country == "Hong Kong S.A.R."] <- "Hong Kong"
df_centroids$Country[df_centroids$Country == "Macau S.A.R"] <- "Macau"


centroids_filtered <- subset(df_centroids,df_centroids$Country %in% number)
#centroids_filtered$Country
#dim(centroids_filtered)
#head(world_filtered)
#tail(world_filtered)
#unique(world_filtered$NAME)
#number


# The following code deals with the creation of the Shiny App that will be
# implemented to create an interactive map showing the evolution of the cases
# of SARS over time.

# This will be used to get the background map on which we will build our app.
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

sars_filtered$Date <- mdy(sars_filtered$Date)
# set the colors that will be used to illustrate the cumulative probable cases, deaths and the nuber of recovered.
colors <- c("red","blue","lightgreen")


##### The following lines of code deal with the issue of creating a dataframe containing 116 dates from 2003/03/17 to 2003/07/11
##### this implies the number of dates are 117 this means to construct an appropriate dataframe there will need to be 117*37(number of countries) rows == 4329.

# As the centroids_filtered df already contains the list of all the needed countries it serves as the ideal building block for the dataframe
# Name this completed dataframe df_1

df_1 <- centroids_filtered
# Get all the dates between the minimum and the maximum of the sars_filtered.
Date <- seq(as.Date(min(sars_filtered$Date)), as.Date(max(sars_filtered$Date)), by="days")
Date <- rep(Date, each = 37)
#Date

# Verify that the length of the Dates is indeed 4329
#length(Date)


# Expand df_1 to now contain the needed 4329 rows
df_1 <- df_1[rep(row.names(df_1),117),1:3]
#df_1

# Add the dates column to the df_1 dataframe
df_1$Date <- Date

# Now we have a dataframe with 4329 rows and 7 columns as needed however the last 3 attribute data columns contain all zeros and need to be updated.

# Use full_join to merge the df_1 and sars_filtered dataframes.
df_1 <- full_join(df_1,sars_filtered, by = c("Date","Country"))

# Verify that the dimensions of df_1 are (4329, 7)
dim(df_1)

# The above dataframe is now continuous over the total dates from the beginning to the end of the dataset but there are still
# a number of dates that do not yet have their NA values adjusted to the previous days' values (most up to date information untill that day).

# Get all the dates in the df_1 dataframe and the sars_filtered so that the dates that are not in common (those needing updating) can be found.
dates_df_1 <- c(unique(df_1$Date))
#dates_df_1
dates_sars_filtered <- c(unique(sars_filtered$Date))
#dates_sars_filtered

# The following dates are the dates where there are no entries or records in the original sars data
# the values of death, recovered and cumulative cases at these dates will be set equal to the values one date prior.
dates_for_replacement <- dates_df_1[!(dates_df_1 %in% dates_sars_filtered )]
#dates_for_replacement
dates_for_replacement_1 <- dates_for_replacement[1:12]
#dates_for_replacement_1

dates_for_replacement_2 <- dates_for_replacement[13:21]
#dates_for_replacement_2
# These are the dates that will be used to replace the missing values.
replacement_dates_1 <- dates_for_replacement_1 - 1
#replacement_dates_1

# The second part of the replacement dates have consecutive dates and thus it is needed to subtract the consecutive date by 2 to get back to
# a date that has an existing record.
replacement_dates_2 <- dates_for_replacement_2 - c(1,1,2,1,2,1,2,1,2)
#replacement_dates_2

# Sort df_1 in a manner tht makes it more convenient to examine and use.
df_1 <- df_1[with(df_1,order(df_1$Date,df_1$Country,na.last = FALSE)),]
#df_1
a <- df_1[which(df_1$Date %in% dates_for_replacement_1),]
a[,c(5,6,7)] <- df_1[which(df_1$Date %in% dates_for_replacement_1),c(5,6,7)]
#head(a,37)
c <- df_1[which(df_1$Date %in% replacement_dates_1),]
# adjust the date back to the value that it was in the original data.
c$Date <- c$Date + 1

d <- left_join(a,c,by = c("Country","Date"))
drops <- c("Cumulative.x", "deaths.x", "recovered.x", "Long.y", "Lat.y")
d <- d[ , !(names(d) %in% drops)]

colnames(d) <- c("Long","Lat","Country","Date","Cumulative","deaths","recovered")
# d now contains the correct data and dates to allow for the shiny app to run appropriately.

# Set the rows that correspond to the NA dates equal to the updated rows in d.
df_1[df_1$Date %in% dates_for_replacement_1,] <- d
#df_1[df_1$Date %in% dates_for_replacement_1,]

# Update the rows with missing values to the values found in the nearest corresponding date on record in the sars data.
df_1[df_1$Date == dates_for_replacement_2[1],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[1],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[2],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[2],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[3],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[3],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[4],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[4],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[5],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[5],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[6],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[6],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[7],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[7],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[8],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[8],c(5,6,7)]
df_1[df_1$Date == dates_for_replacement_2[9],c(5,6,7)] <- df_1[df_1$Date == replacement_dates_2[9],c(5,6,7)]

# To improve computational speeds use data table
# Set all NA values to be equal to 0 to allow for the scaling of text and charts by proportion to work properly.
df_1[is.na(df_1)] = 0
#df_1
df_1_dt <- as.data.table(df_1)


# The code for the shiny app that renders the interactive map showing the evolution of cases over time.
ui = fluidPage(
  
  # The titlePanel is where the title of the ShinyApp is written
  titlePanel("Case History of the SARS outbreak"),
  # The fluidRow is used to create the UI that allows an individual to selected the country, date and scaling parameter for visualization.
  fluidRow(
    column(width=4, pickerInput("country",label = "Pick a country or countries", choices=c(sort((number))),selected = c("Hong Kong"), width="100%", options = list("actions-box" = TRUE, size = 10), multiple = TRUE)),
    column(width=4, pickerInput("Scaler",label = "Pick a statistic to scale the charts with", choices = c("Cumulative","deaths","recovered"), width="100%", options = list("actions-box" = TRUE, size = 10), multiple = FALSE)),
    column(width=4, sliderInput("Date",label="Select the date",min = as.Date(min(sars_filtered$Date)),max =as.Date(max(sars_filtered$Date)),value=as.Date(min(sars_filtered$Date)),step = 3,timeFormat="%d-%m-%Y",animate = animationOptions(interval = 1500,loop = TRUE)))),
  mainPanel(
    leafletOutput(outputId = "map",width = "100%",height = 500))
  
)
server = function(input, output,session) {
  # Ensure that no minicharts are rendered or updated if there are no countries selected.
  chart_data <- reactive({
    validate(
      need(length(input$country) != 0, "Please select a country!")
    )
    df_1_dt[Country %in% input$country & Date == input$Date]})
  
  
  output$map = renderLeaflet({
    # If no country is selected the ShinyApp will inform the user that they should select a country or countries.
    if (length(input$country) == 0){
      data <- NULL
      validate(
        need(length(input$country) != 0, "Please select a country or countries!")
      )
      
    }
    else{
      data <- world_filtered[which(world_filtered$NAME %in% input$country),]
    }
    # Plot the basemap that will have the polygons for the selected countries on it but will not update unless a change occurs to the selected countries.
    basemap <- leaflet(width = "150%", height = "400px") %>%
      addPolygons(data = data,color = "black") %>%
      addTiles(tilesURL) %>% 
      addScaleBar(position = "bottomright") 
    basemap
  })
  
  # Create an observer that will only update the minicharts as country, date or selected scaling statistic changes.
  observe({
    leafletProxy("map",session) %>%
      # The following line of code deals with adding the Minicharts to the Map so that the information about cumulative cases, deaths and recovered can be displayed.
      addMinicharts(layerId = chart_data()$Country,chart_data()$Long,chart_data()$Lat, type = 'pie', opacity = 0.8, 
                    colorPalette = colors, chartdata = chart_data()[,c("deaths","Cumulative","recovered")], showLabels = TRUE,
                    # Using the same scaling scheme for scaling the size of the text inside the minicharts as well as
                    # scaling the width of the minicharts themselves to allow for countries with higher number of the 
                    # selected scaling statistics (cumulative, deaths or recovered) to have a large chart.
                    # This allows for an easier visualization of the trends of the SARS outbreak over time. 
                    labelMinSize = 15*(1 - eval(parse(text = paste("chart_data()$",input$Scaler)))/(1+sum(eval(parse(text = paste("chart_data()$",input$Scaler)))))),
                    labelMaxSize = 15*(0.70 + eval(parse(text = paste("chart_data()$",input$Scaler)))/(1+sum(eval(parse(text = paste("chart_data()$",input$Scaler)))))), 
                    width = 60*(0.70 + eval(parse(text = paste("chart_data()$",input$Scaler)))/(1+sum(eval(parse(text = paste("chart_data()$",input$Scaler)))))), height = 60)
    
  })
  leafletOutput("map", height = 500, width = "100%")
  options = list(height = 800)
}
shinyApp(ui, server)

