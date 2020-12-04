# Losowanie punktow na drogach i wstepne wizualizacje

# install.packages('osmar')
# install.packages('geosphere')

library('osmar')
library('osmdata')
library('geosphere')
library('sf')


important_values <- c('residential', 'service', 'track', 'unclassified', 'path', 'tertiary',
                      'secondary', 'primary', 'living_street', 'trunk', 'motorway')


# Wspolrzedne podane na wejsciu przez uzytkownika
box <- c(21.048, 51.348, 21.235, 51.482) # xmin, ymin, xmax, ymax

qBox <- box %>% 
  opq(timeout = 300) %>%
  add_osm_feature(key = 'highway', value = important_values) %>%
  osmdata_sf()


plot(1, type="n", xlab="", ylab="",
     xlim=c(box[1], box[3]), 
     ylim=c(box[2], box[4]))

mapview::mapview(qBox$osm_lines) 


plot(qBox$osm_lines$geometry, cex = 0.05)


random_points_lat <- numeric(0)
random_points_long <- numeric(0)

for (i in 1:100) {
  random_way <- sample(qBox$osm_lines$geometry, size = 1)
  random_way <- as.vector(random_way[[1]])
  index <- sample(1:(length(random_way)/2), 1)
  random_lat <- random_way[index]
  random_long <- random_way[length(random_way)/2 + index]
  random_points_lat[i] <- random_lat
  random_points_long[i] <- random_long
}

points(x = random_points_lat, y = random_points_long, pch = 19, col = "red", cex = 1)










