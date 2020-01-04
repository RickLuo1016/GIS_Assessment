library(tmap)
library(sf)
library(maptools)
library(leaflet)
library(spatstat)
library(sp)
tmap_mode("view")

# Reaad in base map
London_Boundary <- st_read("Data/London boundary/ESRI/London_Borough_Excluding_MHW.shp")

# Function of creating cluster into different sf
Cluster_SF <- function(ODs,Clu_Num){
  ODs <- ODs[ODs["Cluster_result"]==i,]
  # Convert the cluster result into sp
  Cluster_Map <- psp(ODs[,'O_Longitude'],
                     ODs[,'O_Latitude'],
                     ODs[,'D_Longitude'],
                     ODs[,'D_Latitude'],
                     owin(range(c(ODs[,'O_Longitude'], ODs[,'D_Longitude'])),
                          range(c(ODs[,'O_Latitude'], ODs[,'D_Latitude']))))
  
  Cluster_Map <- as(Cluster_Map, "SpatialLines") 
  
  # Cleaned the data a littes
  rownames(ODs) <- 1:length(Cluster_Map)
  ODs[,17] <- paste(ODs[,1],"to",ODs[,2])
  colnames(ODs)[16:17] <- c("Cluster_result","OD")
  
  # Convert the cluster result into sf
  Cluster_Map <- SpatialLinesDataFrame(Cluster_Map,data = ODs)
  proj4string(Cluster_Map) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  
  return(Cluster_Map)
}

# Array of Cluster_Map
Cluster_Map <- list(1:6)
for(i in 1:6){
  Cluster_Map[i] <- Cluster_SF(OD_cleaned_shrink_clustered,i)
}

# Base map
London_Base <- tm_shape(London_Boundary)+
  tm_polygons("NAME",
              legend.show = FALSE,
              palette = 'black',
              alpha = 0.05)


# Map 
Mapping <- 
  tm_shape(Cluster_Map[[1]]) + 
  tm_lines("Total",
           title.col = "Travel Flow",
           lwd = "Total",
           scale = 20,
           style = "jenks",
           alpha = 0.8,
           id = "OD", 
           popup.vars=c("Total","Origin Station Name","Destination Station Name"),
           group = "Cluster1"
  )+
  tm_shape(Cluster_Map[[2]])+
  tm_lines("Total",
          title.col = "Travel Flow",
          lwd = "Total",
          scale = 20,
          palette = "Blues",
          style = "jenks",
          alpha = 0.8,
          id = "OD", 
          popup.vars=c("Total","Origin Station Name","Destination Station Name"),
          group = "Cluster2"
  )+
  tm_shape(Cluster_Map[[3]])+
  tm_lines("Total",
           title.col = "Travel Flow",
           lwd = "Total",
           scale = 20,
           palette = "Reds",
           style = "jenks",
           alpha = 0.8,
           id = "OD", 
           popup.vars=c("Total","Origin Station Name","Destination Station Name"),
           group = "Cluster3"
  )+
  tm_shape(Cluster_Map[[4]])+
  tm_lines("Total",
           title.col = "Travel Flow",
           lwd = "Total",
           scale = 20,
           palette = "Purples",
           style = "jenks",
           alpha = 0.8,
           id = "OD", 
           popup.vars=c("Total","Origin Station Name","Destination Station Name"),
           group = "Cluster4"
  )+
  tm_shape(Cluster_Map[[5]])+
  tm_lines("Total",
           title.col = "Travel Flow",
           lwd = "Total",
           scale = 20,
           style = "jenks",
           palette = "Greens",
           alpha = 0.8,
           id = "OD", 
           popup.vars=c("Total","Origin Station Name","Destination Station Name"),
           group = "Cluster5"
  )+
  tm_shape(Cluster_Map[[6]])+
  tm_lines("Total",
           title.col = "Travel Flow",
           lwd = "Total",
           scale = 20,
           style = "jenks",
           palette = "Greys",
           alpha = 0.8,
           id = "OD", 
           popup.vars=c("Total","Origin Station Name","Destination Station Name"),
           group = "Cluster6"
  )
  
London_Base+Mapping
