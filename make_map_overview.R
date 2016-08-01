library(ggmap)
myMap <- get_map(maptype = "hybrid", location=c(113.86, 30.70), zoom = 5)
events <- filter(events, latitude != 0, longitude != 0)

ggmap(myMap, extent = "device") + 
  geom_density2d(data = events, aes(x = longitude, y = latitude), size = 0.3) + 
  stat_density2d(data = events, aes(x = longitude, y = latitude, 
                                    fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


