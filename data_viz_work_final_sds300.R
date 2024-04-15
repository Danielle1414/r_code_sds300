## SDS final project 

library(leaflet)
library(grDevices)
library(leaflet.extras)
library(ggplot2)
library(tidyverse)
library(dplyr)

# filter data 

# coral florida data
coral <- read.csv("C:/Users/Danie/Documents/SDS300_R_WORK_DW/final_proj/benthic_coral_florida.csv") |>
  mutate(REGION = recode(REGION, `Tortugas` = "DRT")) |>
  filter(REGION %in% c("FLK", "DRT"))

# view data
head(coral) 
summary(coral)

# species with >20 data points 
count_species <- coral |>
  group_by(COVER_CAT_NAME)|>
  summarise(n = n(),
            count = n())|>
  arrange(desc(count))
count_species

coral1 <- coral |>
  group_by(COVER_CAT_NAME)|>
  filter(n()>20) |>
  filter(!is.na(COVER_CAT_NAME))
head(coral1)

# filter down to only hard corals with >20 data points (18 hard coral species)
coral1 <- coral1 |>
  filter(COVER_CAT_NAME %in% c("Acropora cervicornis", "Agaricia agaricites", "Colpophyllia natans", "Dichocoenia stokesii", "Diploria labyrinthiformis", "Madracis decactis", "Meandrina meandrites", "Montastraea cavernosa", "Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Porites astreoides", "Porites porites", "Siderastrea radians ", "Siderastrea siderea", "Solenastrea bournoni", "Stephanocoenia intersepta", "Pseudodiploria strigosa"))
# view data
head(coral1)

# 2360 observations for all hard coral species included
# locations = FLK (florida keys) DRT (tortugas) and SEFCRI (south east florida coral reef initiative)
count_location <- coral1 |>
  group_by(REGION) |>
  summarise(count = n())
count_location
# observations: DRT (1264) FLK (820) SEFCRI (276) 

# carbonate chemistry data
# create location abbreviations + filter data by region
carb1 <- read.csv("C:/Users/Danie/Documents/SDS300_R_WORK_DW/final_proj/carbonate_chem_atlantic.csv") |>
  mutate(Region = recode(Region, `FL` = "FLK", `DRTO` = "DRT", `DT` = "DRT")) |>
  filter(Region %in% c("FLK", "DRT"))
# view data
carb1

head(carb1)
summary(carb1)

# select columns
carb1 <- carb1 |>
  select(time, latitude, longitude, Region, Year, DIC, pH_measured, pH_calculated, pCO2, Aragonite_Sat, Temperature_C)
head(carb1)

# factor -> numeric
carb1$Temperature_C = as.numeric(carb1$Temperature_C)

# summary stats looking at data in each region
ds_chem <- carb1 |>
  group_by(Region, na.rm = TRUE) |>
  summarise(n = n(),
            mean_PH = mean(pH_measured, na.rm=TRUE),
            mean_Temp = mean(Temperature_C, na.rm=TRUE),
            mean_AS = mean(Aragonite_Sat, na.rm=TRUE))
head(ds_chem)

# 2016-2022
# summary stats looking at data in each year + region
ds2_chem <- carb1 |>
  group_by(Year, Region,na.rm = TRUE) |>
  summarise(n = n(),
            mean_PH = mean(pH_measured, na.rm=TRUE),
            mean_Temp = mean(Temperature_C, na.rm=TRUE),
            mean_AS = mean(Aragonite_Sat, na.rm=TRUE))
head(ds2_chem)

######################################################################

# DATA VIZ!

# Ensure data types are correct
carb1$longitude <- as.numeric(carb1$longitude)
carb1$latitude <- as.numeric(carb1$latitude)

# Filter out missing data
carb1 <- carb1 %>%
  filter(!is.na(Aragonite_Sat))

# Define colors based on Aragonite_Sat variable
color_palette <- colorNumeric(palette = "magma", domain = carb1$Aragonite_Sat)

# Create a Leaflet map
leaflet(data = carb1) |>
  addTiles() |> # Adds default OpenStreetMap tiles
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 10,  # Fixed radius for all points
    color = ~color_palette(Aragonite_Sat),  
    popup = ~paste(Region, "<br> ΩAr:", Aragonite_Sat, "<br> Year:", Year),
    label = ~paste(Region)) |>
  addLegend(
    position = "bottomright",  # Position of the legend
    pal = color_palette,  # Color palette
    values = ~Aragonite_Sat,  # Values to display in the legend
    title = "Aragonite Saturation",  # Title of the legend
    labFormat = labelFormat(suffix = ""),  # Format of the labels
    opacity = 1  # Opacity of the legend 
    )


### Map of ΩAr, pH, and Temp across Florida Keys (still work in progress)

carb1$Temperature_C = as.numeric(carb1$Temperature_C)

# Define colors based on Aragonite_Sat variable
aragonite_palette <- colorNumeric(palette = "magma", domain = carb1$Aragonite_Sat)

# Define colors based on pH variable
ph_palette <- colorNumeric(palette = "viridis", domain = carb1$pH_measured)

# Define colors based on Temperature variable
temperature_palette <- colorNumeric(palette = "RdBu", domain = carb1$Temperature_C)

# Create a Leaflet map
map <- leaflet(data = carb1) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 8,  # Fixed radius for all points
    color = ~aragonite_palette(Aragonite_Sat),
    fillColor = ~aragonite_palette(Aragonite_Sat),
    fillOpacity = 1,
    popup = ~paste("Region:", Region, "<br> ΩAr:", Aragonite_Sat, "<br> Year:", Year),
    label = ~paste(Region),
    group = "Aragonite Saturation"
  ) %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 8,  # Fixed radius for all points
    color = ~ph_palette(pH_measured),
    fillColor = ~ph_palette(pH_measured),
    fillOpacity = 0.8,
    popup = ~paste("Region:", Region, "<br> pH:", pH_measured, "<br> Year:", Year),
    label = NULL,
    group = "pH"
  ) %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 6,  # Fixed radius for all points
    color = ~temperature_palette(Temperature_C),
    fillColor = ~temperature_palette(Temperature_C),
    fillOpacity = 0.8,
    popup = ~paste("Region:", Region, "<br> Temperature:", Temperature_C, "<br> Year:", Year),
    label = NULL,
    group = "Temperature"
  ) %>%
  addLayersControl(
    overlayGroups = c("Aragonite Saturation", "pH", "Temperature"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(
    position = "bottomright",  # Position of the legend
    pal = color_palette,  # Color palette
    values = ~Aragonite_Sat,  # Values to display in the legend
    title = "Aragonite Saturation",  # Title of the legend
    labFormat = labelFormat(suffix = ""),  # Format of the labels
    opacity = 1  # Opacity of the legend
  )

### ΩAr across Time

# Convert date/time column to appropriate format if needed
carb1$time <- as.POSIXct(carb1$time)

# Plot time series
ggplot(carb1, aes(x = time, y = Aragonite_Sat)) +
  geom_line() +
  labs(x = "Time", y = "Aragonite Saturation") +
  ggtitle("Aragonite Saturation Over Time")


### Changes in % hard coral cover over time

# Convert time to Date format

coral1$HARDBOTTOM_P <- as.numeric(coral1$HARDBOTTOM_P)

# Changes in % Hard Coral Cover Over Time at Each Region and Overall
coral_plot1 <- ggplot(coral1, aes(x = time, 
                                  y = HARDBOTTOM_P)) +
  facet_wrap(~REGION) +
  geom_line() +
  labs(x = "Time", 
       y = "% Hard Coral Cover", 
       title = "Changes in % Hard Coral Cover Over Time by Region") +
  theme_minimal()
coral_plot1

# Changes in % Hard Coral Cover Over Time at Each Region and Overall
coral_plot2 <- ggplot(coral1, aes(x = time, 
                                  y = HARDBOTTOM_P, 
                                  color = REGION)) +
  geom_point() +
  labs(x = "Time", 
       y = "% Hard Coral Cover", 
       title = "Changes in % Hard Coral Cover Over Time by Region and Species") + facet_wrap(~COVER_CAT_NAME) +
  theme_minimal()
coral_plot2

####################################################################################
# added code: 04/15 class 
  
# rename 
# join by region and year
# Cannot use SEFCRI region in carbonate data 
# Only FLK and Torgugas!!!!
  
# EXAMPLE: 
  # linear regression: response ~ explanatory
  # sa_buoyant_coral_lm <- lm(bw ~ surface_area, data = sa_buoyant_coral)

# summary table
# summary(sa_buoyant_coral_lm)
# adjusted R-squared:  0.4291 
# p-value of regression model: 1.511e-06
# regression intercept: 7.5303 
# slope: 0.8569


