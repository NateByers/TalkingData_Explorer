library(sp)
library(raster)
library(dplyr)
load("data/prepped_data.rda")

d <- full_data

d_mat <- cbind(d$longitude, d$latitude)
d_spdf <- SpatialPointsDataFrame(d_mat, d, proj4string = CRS("+proj=longlat"))

r <- raster(d_spdf, nrows = 100, ncols = 100)
r[] <- 1:ncell(r)

d_spdf@data <- data.frame(d_spdf@data, extract(r, d_spdf, cellnumbers=TRUE))
head(d_spdf)

xy <- data.frame(cells = 1:ncell(r), xyFromCell(r, 1:ncell(r)))

map_data <- left_join(d_spdf@data, xy) %>%
  group_by(cells) %>%
  summarize(events = n(), devices = length(unique(device_id)), 
            longitude = unique(x), latitude = unique(y))

max_events <- max(map_data$events)
max_devices <- max(map_data$devices)

map_data <- map_data %>%
  mutate(radius_events = (events/max_events)*15,
         radius_devices = (devices/max_devices)*15)

map_data[map_data$radius_events < 1, "radius_events"] <- 1
map_data[map_data$radius_devices < 1, "radius_devices"] <- 1

save(map_data, file = "data/map.rda")
