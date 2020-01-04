library(readr)
library(dplyr)
library(tidyr)

# OD_network
OD <- read_csv("Data/MTT.csv")

# coordinates of stations
stations <- read_csv("Data/London stations2.csv")

# Join the OD_network and stations
ODwithCoordinates <-  merge(OD,
                            stations,
                            by.x = "Origin Station Name",
                            by.y = "Station")

ODwithCoordinates <-  merge(ODwithCoordinates,
                            stations,
                            by.x = "Destination Station Name",
                            by.y = "Station")

# Drop unnecessary columns
OD_cleaned <- ODwithCoordinates[,c(2,1,7:15,18,19,24,25)]

# Renamed column
names(OD_cleaned)[12:15] <- c("O_Latitude","O_Longitude","D_Latitude","D_Longitude")


# Correcting the total number
OD_cleaned[3] <- rowSums(OD_cleaned[,4:11])

# Extract 
OD_cleaned_shrink <- OD_cleaned[OD_cleaned[3]>0,]

# Standardized 1
for(i in c(4:11)){
  OD_cleaned_shrink[i] <- OD_cleaned_shrink[i]/OD_cleaned_shrink[3]
}




                              