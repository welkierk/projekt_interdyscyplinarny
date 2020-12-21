source("./photo_function.R")
data <- read.csv("./model2/train.csv")

water <- c()
vegetation <- c()
for (i in 1:nrow(data)) {
  photo <- data_frame_from_photos(lat = data$latitude[i], lon = data$longitude[i])
  water[i] <- photo[1,1]
  vegetation[i] <- photo[1,2]
}

new_data <- cbind(data, water, vegetation)
write.csv(new_data, "./model2/new_train.csv")
