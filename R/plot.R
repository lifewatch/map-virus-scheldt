# library(ows4R)
library(sf)
library(ggplot2)
library(dplyr)
library(mregions2)

#  Define area bounding box
bbox <- st_as_sfc("MULTIPOLYGON (((2.68255017821626129 50.84377596429029467, 5.23880074587292377 50.84377596429029467, 5.23880074587292377 52.08067140025319475, 2.68255017821626129 52.08067140025319475, 2.68255017821626129 50.84377596429029467)))")
st_crs(bbox) <- 4326

# Get countries from Marine Regions and crop to content
countries <- gaz_search(c(14, 15)) |> 
  gaz_geometry() |>
  st_intersection(bbox) |> 
  st_transform(3035)

# Get preprocessed salinity based on Scheldemonitor (See ./R/salinity.R)
# salinity <- read_sf("./data/salinity.geojson") |> st_transform(3035)
# Get salinity areas from Scheldemonitor
url_salinity <- "https://geo.vliz.be/geoserver/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=Scheldemonitor%3Asaliniteit_harbasin&viewParams=&outputFormat=SHAPE-ZIP&SRSName=EPSG:4326"
download.file(url_salinity, "./data/salinity.zip")
unzip("./data/salinity.zip", exdir = "./data")
salinity <- read_sf("./data/saliniteit_harbasin.shp") |> 
  filter(salinity_1 %in% c("Oligohaline", "Mesohaline", "Polyhaline"))

# Set stations
stations <- data.frame(
  label = as.character(1:5),
  longitude = c(3.17344, 3.67789, 4.15867, 4.39829, 4.30448),
  latitude = c(51.50990, 51.40181, 51.38225, 51.23136, 51.12416)
) |> st_as_sf(
  coords = c("longitude", "latitude"),
  remove = FALSE,
  crs = 4326
) |> st_transform(3035)


# Set country labels
country_label <- data.frame(
  label = c("BEL", "NLD"),
  latitude = c(51.15, 51.3),
  longitude = c(3.72, 3.85)
) |> st_as_sf(
  coords = c("longitude", "latitude"),
  remove = FALSE,
  crs = 4326
) |> st_transform(3035)

# Plot
ggplot() +
  scale_x_continuous(breaks = seq(3, 5, by = spacing)) +
  scale_y_continuous(breaks = seq(50, 52, by = spacing)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#164863"),
    plot.background = element_rect(),
    plot.margin = margin(0, 0, 0, 0, "cm")
    ) +
  
# Add Salinity
  geom_sf(mapping = aes(fill = salinity_1), data = salinity, color = NA) +
  scale_fill_manual(values = c(
                               "#9BBEC8",  
                               "#DDF2FD",
                               "#427D9D"), 
                    name = "Salinity") + 
# Add land shapes
  geom_sf(mapping = aes(), data = countries, fill = "gray92", color = "gray30") +
  
# Add stations
  geom_sf(mapping = aes(), data = stations, fill = "gray30") +

# Add country labels
  geom_sf_text(mapping = aes(label = country_label$label, 
                             geometry = country_label$geometry),
               size = 3, color = "gray30"
  ) +
# Set frame and CRS
  coord_sf(crs = 3035,
           xlim = c(3850000, 3936000),
           ylim = c(3120000, 3180000),
           expand = F)
