# NYC Fire Incidents Visualization
R
```{r setup, include=FALSE, message=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Location of "Signal 7-5"

Even though the highest level of fire alarm in this dataset is "110 - Eleventh Alarm", there is only one observation with this value; therefore, we use "7 - signal 7-5" instead for completing the following tasks.

```{r, echo = FALSE, message=FALSE, warning=FALSE}
library(devtools)
devtools::install_github("rstudio/leaflet")
library(leaflet)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(leaflet.extras)
library(leaflet.extras2)
library(ggplot2)
library(sf)
library(tmap)
library(dplyr)

# Import the fire incident csv file
fires <- read_csv("data/building_fires.csv")

# Check the unique values of HIGHEST_LEVEL_DESC: the highest alarm should be "110 - Eleventh Alarm"
#unique(fires$HIGHEST_LEVEL_DESC)
# Check the unique values of BOROUGH_DESC: no locations outside the 5 boroughs
#unique(fires$BOROUGH_DESC)

# Subset to the target value "7 - signal 7-5"
highest_fires <- fires %>%
  filter(HIGHEST_LEVEL_DESC == "7 - Signal 7-5")

# Make a leaflet map
content1 <- paste("Level:", highest_fires$HIGHEST_LEVEL_DESC, "<br/>",
                  "Incident Datetime:", highest_fires$INCIDENT_DATE_TIME, "<br/>",
                  "Arrival Datetime:", highest_fires$ARRIVAL_DATE_TIME, "<br/>",
                  "Cleared Datetime:", highest_fires$LAST_UNIT_CLEARED_DATE_TIME,
                  "<br/>")
leaflet(highest_fires) %>%
  setView(lng=-73.9561932, lat=40.711118, zoom=10) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircles(col="red",
             popup=content1)
```

# Color Incidents Using Property Type and Make Clusters

## Color Incidents Using Property Type

By using unique() function to look up all property types that happened to have a fire incident recorded as "signal 7-5", we notice that there are 105 types in total. To create a more concise map, we could collapse those types into just 5 categories. Based on the brief description for each property type, we have created the following categories: Residential, Commercial, Industrial, Public, and Others.

```{r, echo = FALSE, message=FALSE, warning=FALSE}
#unique(highest_fires$PROPERTY_USE_DESC)

# Collapse categories for property
highest_fires$property_type[highest_fires$PROPERTY_USE_DESC %in% 
                              c("419 - 1 or 2 family dwelling",
                                "429 - Multifamily dwelling",
                                "400 - Residential, other",
                                "449 - Hotel/motel, commercial",
                                "311 - 24-hour care Nursing homes, 4 or more persons",
                                "439 - Boarding/rooming house, residential hotels",
                                "899 - Residential or self-storage units",
                                "459 - Residential board and care")] <- "Residential"
highest_fires$property_type[highest_fires$PROPERTY_USE_DESC %in% 
                              c("579 - Motor vehicle or boat sales, services, repair",
                                "161 - Restaurant or cafeteria",
                                "564 - Laundry, dry cleaning",
                                "559 - Recreational, hobby, home repair sales, pet store",
                                "500 - Mercantile, business, other",
                                "549 - Specialty shop",
                                "519 - Food and beverage sales, grocery store",
                                "557 - Personal service, including barber & beauty shops",
                                "511 - Convenience store",
                                "162 - Bar or nightclub",
                                "539 - Household goods, sales, repairs",
                                "581 - Department or discount store")] <- "Commercial"
highest_fires$property_type[highest_fires$PROPERTY_USE_DESC %in% 
                              c("700 - Manufacturing, processing",
                                "891 - Warehouse",
                                "610 - Energy production plant, other")] <- "Industrial"
highest_fires$property_type[highest_fires$PROPERTY_USE_DESC %in% 
                              c("131 - Church, mosque, synagogue, temple, chapel",
                                "210 - Schools, non-adult, other",
                                "332 - Hospices",
                                "170 - Passenger terminal, other",
                                "180 - Studio/theater, other",
                                "183 - Movie theater",
                                "241 - Adult education center, college classroom",
                                "152 - Museum",
                                "130 - Places of worship, funeral parlors, other",
                                "300 - Health care, detention, & correction, other",
                                "150 - Public or government, other",
                                "174 - Rapid transit station",
                                "200 - Educational, other",
                                "215 - High school/junior high school/middle school",
                                "321 - Mental retardation/development disability facility",
                                "142 - Clubhouse",
                                "140 - Clubs, other")] <- "Public"
typed_values <- c("419 - 1 or 2 family dwelling",
                  "429 - Multifamily dwelling",
                  "400 - Residential, other",
                  "449 - Hotel/motel, commercial",
                  "311 - 24-hour care Nursing homes, 4 or more persons",
                  "439 - Boarding/rooming house, residential hotels",
                  "899 - Residential or self-storage units",
                  "459 - Residential board and care",
                  "579 - Motor vehicle or boat sales, services, repair",
                  "161 - Restaurant or cafeteria",
                  "564 - Laundry, dry cleaning",
                  "559 - Recreational, hobby, home repair sales, pet store",
                  "500 - Mercantile, business, other",
                  "549 - Specialty shop",
                  "519 - Food and beverage sales, grocery store",
                  "557 - Personal service, including barber & beauty shops",
                  "511 - Convenience store",
                  "162 - Bar or nightclub",
                  "539 - Household goods, sales, repairs",
                  "581 - Department or discount store",
                  "700 - Manufacturing, processing",
                  "891 - Warehouse",
                  "610 - Energy production plant, other",
                  "131 - Church, mosque, synagogue, temple, chapel",
                  "210 - Schools, non-adult, other",
                  "332 - Hospices",
                  "170 - Passenger terminal, other",
                  "180 - Studio/theater, other",
                  "183 - Movie theater",
                  "241 - Adult education center, college classroom",
                  "152 - Museum",
                  "130 - Places of worship, funeral parlors, other",
                  "300 - Health care, detention, & correction, other",
                  "150 - Public or government, other",
                  "174 - Rapid transit station",
                  "200 - Educational, other",
                  "215 - High school/junior high school/middle school",
                  "321 - Mental retardation/development disability facility",
                  "142 - Clubhouse",
                  "140 - Clubs, other")
remaining_values <- setdiff(unique(highest_fires$PROPERTY_USE_DESC), typed_values) 
highest_fires$property_type[highest_fires$PROPERTY_USE_DESC %in% 
                              remaining_values] <- "Others"

# Check the output of the new column containing the collapsed 5 categories
#unique(highest_fires$property_type)

# Define palette
pal = colorFactor("Set1", domain=highest_fires$property_type)
color_property = pal(highest_fires$property_type)

# Define popup
content2 <- paste("Property Type:", highest_fires$property_type, "<br/>",
                  "Incident Datetime:", highest_fires$INCIDENT_DATE_TIME, "<br/>",
                  "Arrival Datetime:", highest_fires$ARRIVAL_DATE_TIME, "<br/>",
                  "Cleared Datetime:", highest_fires$LAST_UNIT_CLEARED_DATE_TIME, "<br/>")

# Make a leaflet map
leaflet(highest_fires) %>%
  setView(lng=-73.9561932, lat=40.711118, zoom=10) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(color=color_property,
                   popup=content2) %>%
  addLegend(pal=pal, values=~highest_fires$property_type, title="Property Type")
```

## Make Clusters 

```{r, echo = FALSE, message=FALSE, warning=FALSE}
# Make a leaflet map with clusters
leaflet(highest_fires) %>%
  setView(lng=-73.9561932, lat=40.711118, zoom=10) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(color=color_property,
                   popup=content2,
                   clusterOptions=markerClusterOptions()) %>%
  addLegend(pal=pal, values=~highest_fires$property_type, title="Property Type")
```

# Multiple Layers

Map fire houses in the New York City and visualize the severity of fire incidents by the size of point. Then, provide a layer control option for users to select which information to show.

```{r, echo = FALSE, message=FALSE, warning=FALSE}
# Provide the non-clustered map
map3 <- leaflet(highest_fires) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(group="Incidents",
                   color=color_property,
                   radius=~UNITS_ONSCENE/5, # define severity
                   popup=content2) %>%
  addLegend(pal=pal, values=~highest_fires$property_type, title="Property Type")

# Import firehouses data
firehouses <- read_csv("data/FDNY_Firehouse_Listing.csv")

# Define firehouse icon
firehouseIcon <- icons(
  iconUrl="image/firehouse.png",
  iconWidth=15, iconHeight=15
)

# Add the firehouse layer
map3 %>%
  addMarkers(data=firehouses,
             icon=firehouseIcon,
             group="Firehouses") %>%
  # Add layer controls
  addLayersControl(
    baseGroups="OpenStreetMap",
    overlayGroups=c("Incidents", "Firehouses"),
    options=layersControlOptions(collapse=TRUE))
```

# Map of Response Times

As the response time is a value contained in each point (i.e. fire incidents), to make a choropleth map for response time, we need to calculate the average of response time within each polygon. Since there is a variable named "ZIP_CODE" included in the building_fires.csv file and the zip-code level itself could provide more information while investigating the relationship between the response time and property type or fire severity, we could create a choropleth map about the average response time at the zip-code level.

## Calculate Response Time by Zip Code Only

While zip-code areas with darker blues are areas where fire incidents had longer time of response, we could notice that the areas having the darkest blue have fewer buildings had experienced fire incidents and most of the buildings within those areas belong to the "Residential" category. However, it is hard to investigate whether fire severity impacts the response time since all zip-code areas are covered by different levels of fire severity.

```{r, echo = FALSE, message=FALSE, warning=FALSE}
# Calculate response time
highest_fires$INCIDENT_DATE_TIME <- mdy_hms(highest_fires$INCIDENT_DATE_TIME)
highest_fires$ARRIVAL_DATE_TIME <- mdy_hms(highest_fires$ARRIVAL_DATE_TIME)
highest_fires$response_time <- difftime(highest_fires$ARRIVAL_DATE_TIME,
                                        highest_fires$INCIDENT_DATE_TIME)
highest_fires$response_time <- as.numeric(highest_fires$response_time)

# Create a "year" column 
highest_fires$year <- format(as.Date(highest_fires$INCIDENT_DATE_TIME), "%Y")
highest_fires$year <- as.numeric(highest_fires$year)

# Create a new dataframe to store average response time for each zip-code area
zip_response_time <- aggregate(response_time ~ ZIP_CODE, highest_fires, mean)
zip_df <- data.frame(ZIPCODE=zip_response_time$ZIP_CODE, response_time=zip_response_time$response_time)
zip_df$ZIPCODE <- as.character(zip_df$ZIPCODE)

# Import zip-code shapefile of the New York City
nyc <- read_sf("data/ZIP_CODE_040114.shp")
nyc <- st_transform(nyc, "+proj=longlat +datum=WGS84")

# Join the zip_df with the nyc shapefile
joined <- left_join(nyc, zip_df, by="ZIPCODE")

# Remove NA or 0 for response time for making bins
joined <- joined %>%
  mutate(response_time=replace_na(response_time,0))
joined <- subset(joined, response_time != 0)
#colnames(joined)

# Leaflet
my_colors <- c('#BCC6CC', '#87CEFA', '#1E90FF', '#1569C7', '#0000A5')
# Create the "Response Time" layer
map4 <- leaflet(joined) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addPolygons(group="Response Time",
              fillColor=~colorQuantile(palette=my_colors, domain=round(joined$response_time, digits=2))(joined$response_time),
              stroke=FALSE, 
              fillOpacity=0.8,
              highlightOptions=highlightOptions(color="white", weight=2, bringToFront=TRUE),
              label=~paste(ZIPCODE, "Avg Response Time:", response_time, "seconds")) %>%
  addLegend(position="bottomleft",
            colors=my_colors,
            labels=round(quantile(joined$response_time, probs=seq(0, 1, 0.25)), 1),
            title="Avg Response Time (seconds)")
# Combine Incidents and Firehouses layers
map4 %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(data=highest_fires,
                   group="Incidents",
                   color=color_property,
                   radius=~UNITS_ONSCENE/5, # define severity
                   popup=content2) %>%
  addLegend(pal=pal, values=~highest_fires$property_type, title="Property Type") %>%
  addMarkers(data=firehouses,
             icon=firehouseIcon,
             group="Firehouses") %>%
  addLayersControl(
    baseGroups="OpenStreetMap",
    overlayGroups=c("Incidents", "Firehouses", "Response Time"),
    options=layersControlOptions(collapse=TRUE))
```

## Calculate Response Time by Zip Code and Year

Within this map, users can choose to display the response time in whichever year they want. By comparing the average response time among different years, we could notice that the average response times in 2013 and 2014 have similar patterns across the New York City. Furthermore, the average response time in the central area of Staten Island had a great improvement from 2015 and the overall average response time across the New York City in 2017 is shorter. It is noted that there is not enough data for analyzing the response time in 2018.

```{r, echo = FALSE, message=FALSE, warning=FALSE}
# Create a new data frame to store average response time for each borough and year
zip_response_time2 <- aggregate(response_time ~ ZIP_CODE + year, highest_fires, mean)
zip_df2 <- data.frame(ZIPCODE=zip_response_time2$ZIP_CODE, 
                     year=zip_response_time2$year,
                     response_time=zip_response_time2$response_time)
zip_df2$ZIPCODE <- as.character(zip_df2$ZIPCODE)

# Join the borough_year_df with the nyc shapefile
joined2 <- left_join(nyc, zip_df2, by="ZIPCODE")
joined2 <- joined2 %>%
  mutate(response_time=replace_na(response_time,0))
joined2 <- subset(joined2, response_time != 0)
#unique(joined2$year)
# Create subset for each year
joined_13 <- subset(joined2, year == 2013)
joined_14 <- subset(joined2, year == 2014)
joined_15 <- subset(joined2, year == 2015)
joined_16 <- subset(joined2, year == 2016)
joined_17 <- subset(joined2, year == 2017)
joined_18 <- subset(joined2, year == 2018)

# Create a list of subsets
zip_list <- list(joined_13, joined_14, joined_15, joined_16, joined_17, joined_18)

# Create a list of years
year_list <- c("2013", "2014", "2015", "2016", "2017", "2018")

# Create a leaflet map with initial view set to New York City
map <- leaflet(joined2) %>% addTiles() %>%
  setView(lng=-73.985428, lat=40.748817, zoom=10)

# Add a layer for each subset with a unique color
for (i in 1:length(zip_list)) {
  map <- addPolygons(map=map, data=zip_list[[i]], group=year_list[i],
                     fillColor=colorQuantile(palette=my_colors, 
                                               domain=zip_list[[i]]$response_time)(zip_list[[i]]$response_time),
                     weight=1, fillOpacity=0.8,
                     highlightOptions=highlightOptions(color="white", weight=2, bringToFront=TRUE),
                     label=~paste(ZIPCODE, "Avg Response Time:", response_time, "seconds"))
}

# Add a legend
map <- addLegend(map=map, position="bottomleft",
                 colors=my_colors,
                 label=round(quantile(joined2$response_time, probs=seq(0, 1, 0.25)), 1),
                 title="Avg Response Time (seconds)")

# Combine Incidents and Firehouses layers
map %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(data=highest_fires,
                   group="Incidents",
                   color=color_property,
                   radius=~UNITS_ONSCENE/5, # define severity
                   popup=content2) %>%
  addLegend(pal=pal, values=~highest_fires$property_type, title="Property Type") %>%
  addMarkers(data=firehouses,
             icon=firehouseIcon,
             group="Firehouses") %>%
  addLayersControl(
    baseGroups="OpenStreetMap",
    overlayGroups=c("Incidents", "Firehouses", "2013", "2014", "2015", "2016", "2017", "2018"),
    options=layersControlOptions(collapse=TRUE))
```
