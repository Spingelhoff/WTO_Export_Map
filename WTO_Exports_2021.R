library(tidyverse)
library(ggmap)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

data <- tibble(read.csv("AG_Exports_2021.csv"))

##WTO output includes indicators I did not select for
list(unique(data$Indicator.Category))

##Check for non UTF-8 characters
list(unique(data$Reporting.Economy))
list(unique(data$Partner.Economy))

##Simplify data and convert non UTF-8 characters
filtered_data <- data %>%
  filter(Indicator == "AG - Value of exports to partner (imports by partner)" |
         Indicator == "Non-AG - Value of exports to partner (imports by partner)") %>%
  select(Reporting.Economy, Partner.Economy, Value) %>%
  group_by(Reporting.Economy, Partner.Economy) %>%
  summarise(Value = sum(Value)) %>%
  mutate(Reporting.Economy = ifelse(Reporting.Economy == "C\xf4te d'Ivoire",
                                    "Cote d'Ivoire",
                                    ifelse(Reporting.Economy == "T\xfcrkiye",
                                           "Turkiye",
                                           Reporting.Economy)),
         Partner.Economy = ifelse(Partner.Economy == "C\xf4te d'Ivoire",
                                  "Cote d'Ivoire",
                                  ifelse(Partner.Economy == "T\xfcrkiye",
                                         "Turkiye",
                                         ifelse(Partner.Economy == "Sao Tom\xe9 and Principe",
                                                "Sao Tome and Principe",
                                                Partner.Economy))))

filtered_data

##get geographical coordinates for countries (requires Google api key set up)
geolocated_data <- filtered_data %>%
  mutate_geocode(Reporting.Economy) %>%
  rename(Reporting.lon = lon, Reporting.lat = lat) %>%
  mutate_geocode(Partner.Economy) %>%
  rename(Partner.lon = lon, Partner.lat = lat)

geolocated_data

##subset coordinate data to reporting country
Reporting_lonlat <- geolocated_data %>%
  select(Reporting.Economy, Reporting.lon, Reporting.lat) %>%
  unique() %>%
  mutate(Grouping = "World")

Reporting_lonlat

##add degree data for sizing
degree_data <- geolocated_data %>%
  ungroup() %>%
  select(Reporting.Economy, Partner.Economy) %>%
  group_by(Reporting.Economy) %>%
  summarise(Degree = n())

degree_data

##Create world data frame for sizing
world_data <- geolocated_data %>%
  summarize(Value = sum(Value)) %>%
  left_join(Reporting_lonlat) %>%
  left_join(degree_data)
  
world_data

##combine data
combined_data <- geolocated_data %>%
  left_join(degree_data, by = c("Partner.Economy" = "Reporting.Economy"))

##functions and preparation for leaflet
quantile_pal <- colorQuantile("Reds", geolocated_data$Value, 5)
quantile_pal_world <- colorQuantile("Blues", world_data$Value, 5)
degree_size <- function(x) {
  ifelse(x > 40, 60,
         ifelse(40 > x & x > 30, 50,
                ifelse(30 > x & x > 20, 40,
                       ifelse(20 > x & x > 10, 30, 20))))
}

##visualize with leaflet
WTO_Export_Map <- leaflet(combined_data) %>%
  addProviderTiles("Stamen.TonerLite",
                   options = providerTileOptions(minZoom = 0,
                                                 maxZoom = 4)) %>%
  addCircleMarkers(lng = ~Partner.lon,
                   lat = ~Partner.lat,
                   group = ~Reporting.Economy,
                   label = ~paste0(Partner.Economy,
                                  ": ",
                                  round(Value, 2),
                                  " Million"),
                   stroke = TRUE,
                   color = ~quantile_pal(Value),
                   radius = ~degree_size(Degree)) %>%
  addCircleMarkers(data = world_data,
                   lng = ~Reporting.lon,
                   lat = ~Reporting.lat,
                   group = ~Reporting.Economy,
                   label = ~paste0(Reporting.Economy,
                                   " Total Export Value: ",
                                   round(Value, 2),
                                   " Million"),
                   color = ~quantile_pal_world(Value),
                   radius = ~degree_size(Degree)) %>%
  addCircleMarkers(data = world_data,
                   lng = ~Reporting.lon,
                   lat = ~Reporting.lat,
                   group = ~Grouping,
                   label = ~paste0(Reporting.Economy,
                                   " Total Export Value: ",
                                   round(Value, 2),
                                   " Million"),
                   color = ~quantile_pal_world(Value),
                   radius = ~degree_size(Degree)) %>%
  addLegend(data = world_data,
            pal = quantile_pal_world, 
            values = world_data$Value,
            group = "World") %>%
  addLayersControl(overlayGroups = ~c(Reporting.Economy, "World")) %>%
  hideGroup(group = ~Reporting.Economy) %>%
  setMaxBounds(-190, -100, 190, 100)

WTO_Export_Map

saveWidget(WTO_Export_Map, "WTO_Export_Map_Widget.html",
           title = "WTO_Export_Map_Widget.html")