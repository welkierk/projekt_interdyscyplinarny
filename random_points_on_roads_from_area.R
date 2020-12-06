# Losowanie punktow na drogach i wstepne wizualizacje

# install.packages('osmar')
# install.packages('geosphere')

library('osmar')
library('osmdata')
library('geosphere')
library('sf')

# FUNKCJA ZWRACAJACA WSPOLRZEDNE LOSOWYCH PUNKTOW LEZACYCH NA DROGACH
random_points <- function(xmin, ymin, xmax, ymax){
  
  # Wspolrzedne podane na wejsciu przez uzytkownika
  box <- c(xmin, ymin, xmax, ymax)
  
  important_values <- c('residential', 'service', 'track', 'unclassified', 'path', 'tertiary',
                        'secondary', 'primary', 'living_street', 'trunk', 'motorway')
  
  qBox <- box %>% 
    opq(timeout = 300) %>%
    add_osm_feature(key = 'highway', value = important_values) %>%
    osmdata_sf()
  
  
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
  
  df <- data.frame("Lattitude" = random_points_lat,
                      "Longtitude" = random_points_long)
  
  return(df)
  
}


# ## Fragment sluzacy prezentacji, bez znaczenia dla realizacji projektu
# box <- c(21.048, 51.348, 21.235, 51.482)
# important_values <- c('residential', 'service', 'track', 'unclassified', 'path', 'tertiary',
#                       'secondary', 'primary', 'living_street', 'trunk', 'motorway')
# qBox <- box %>%
#   opq(timeout = 300) %>%
#   add_osm_feature(key = 'highway', value = important_values) %>%
#   osmdata_sf()
# 
# mapview::mapview(qBox$osm_lines)
# 
# plot(qBox$osm_lines$geometry, cex = 0.05)
# 
# df <- random_points(21.048, 51.348, 21.235, 51.482)
# points(x = df$Lattitude, y = df$Longtitude, pch = 19, col = "red", cex = 1)
# 







