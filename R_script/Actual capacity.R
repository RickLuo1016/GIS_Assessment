library(readr)
library(dplyr)
library(tidyr)
library(tmap)
library(sf)
library(sp)
library(ggplot)

# Caluclate the flow
Alighter <- aggregate(OD_cleaned["Total"],by=OD_cleaned["Destination Station Name"], FUN=sum)
Boarder <- aggregate(OD_cleaned["Total"],by=OD_cleaned["Origin Station Name"], FUN=sum)
ActuFlow <- merge(Alighter,Boarder,by.x = "Destination Station Name",by.y ="Origin Station Name")
ActuFlow[4] <- ActuFlow[2]+ActuFlow[3]
ActuFlow <- ActuFlow[c(1,4)]

# Add coordinates
colnames(ActuFlow) <- c("Station Name","Total Flow")
ActuFlow <- merge(ActuFlow,stations,by.x = "Station Name",by.y = "Station")

# Create spatial object
ActuCap <- SpatialPoints(ActuFlow[,c("Longitude","Latitude")],proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ActuCap <- SpatialPointsDataFrame(ActuCap,ActuFlow[,1:2])


# Reaad in base map
London_Boundary <- st_read("Data/London boundary/ESRI/London_Borough_Excluding_MHW.shp")

# Base map
London_Base <- tm_shape(London_Boundary)+
  tm_polygons("NAME",
              legend.show = FALSE,
              palette = 'black',
              alpha = 0.05)

# Map
ActuCap_Map <- tm_shape(ActuCap)+
  tm_bubbles("Total Flow",
             size = 0.8 ,
             scale = 0.5,
             style = "jenks",
             alpha = 0.7,
             col = "Total Flow",
             border.col = "black",
             title.col = "Actual Capicity",
             border.alpha = 0.3)

# Distribution
ActuCapDis <- ggplot(ActuFlow)+
  geom_histogram(aes(x = ActuFlow$`Total Flow`),
                 fill = brewer.pal(7,"Blues")[7],
                 col =brewer.pal(7,"Greys")[2],
                 alpha = 0.8,
                 bins = 60)+
  scale_x_continuous(breaks = seq(min(ActuFlow$`Total Flow`),
                                  max(ActuFlow$`Total Flow`),
                                  (max(ActuFlow$`Total Flow`)-min(ActuFlow$`Total Flow`))/10))+
  scale_y_continuous()+
  labs(title="Distribution of Actual Demand(capita)",
       x="Actual Demand(capita)",
       y="Frequency")


# Transformed Distribution
ActuCapDis_Tran <- ggplot(ActuFlow)+
  geom_histogram(aes(x = log10(ActuFlow$`Total Flow`)),
                 fill = brewer.pal(7,"Blues")[7],
                 col =brewer.pal(7,"Greys")[2],
                 alpha = 0.8,
                 bins = 60)+
  scale_x_continuous(breaks = seq(round(min(log10(ActuFlow$`Total Flow`)),2),
                                  round(max(log10(ActuFlow$`Total Flow`)),2),
                                  round((max(log10(ActuFlow$`Total Flow`))-min(log10(ActuFlow$`Total Flow`)))/10,1)))+
  scale_y_continuous()+
  labs(title="Distribution of log(Actual Demand(capita))",
       x="log10(Actual Demand(capita))",
       y="Frequency")


ActuCapDis
ActuCapDis_Tran
London_Base+ActuCap_Map
  