library(rgdal)
library(sf)
library(shp2graph)

# read in KML
EstiFlow <- readOGR("Data/London Train Lines.KML")

# Calculation with iGraph object
EstiFlow_IG1 <- readshpnw(EstiFlow,ELComputed =TRUE)

EstiFlow_IG2 <- nel2igraph(EstiFlow_IG1[[2]],EstiFlow_IG1[[3]],weight = EstiFlow_IG1[[4]])

PageRank <- page.rank(EstiFlow_IG2,weights = EstiFlow_IG1[[4]])

# Change back to dataframe
EstiFlow <- as_data_frame(EstiFlow_IG2,what ="vertices")
EstiFlow <- cbind(EstiFlow,PageRank$vector)
colnames(EstiFlow) <- c("Longitude","Latitude","PageRank")


# Convert to sp object
EstiCap <- SpatialPoints(EstiFlow[,c("Longitude","Latitude")],proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
EstiCap <- SpatialPointsDataFrame(EstiCap,EstiFlow[3])


# Base map
London_Base <- tm_shape(London_Boundary)+
  tm_polygons("NAME",
              legend.show = FALSE,
              palette = 'black',
              alpha = 0.05)

# Map
EstiCap_Map <- tm_shape(EstiCap)+
  tm_bubbles("PageRank",
             size = 0.8,
             scale = 0.5,
             style = "jenks",
             alpha = 0.7,
             col = "PageRank",
             title.col = "Estimated Capicity(PageRank)",
             breaks = c(0:6)*(max(EstiCap@data$PageRank)-min(EstiCap@data$PageRank))/6,
             border.col = "black",
             border.alpha = 0.3)


# Distribution
EstiCapDis <- ggplot(EstiFlow)+
  geom_histogram(aes(x = EstiFlow$PageRank),
                 fill = brewer.pal(7,"Blues")[7],
                 col =brewer.pal(7,"Greys")[2],
                 alpha = 0.8,
                 bins = 60)+
  scale_x_continuous()+
  scale_y_continuous()+
  labs(title="Distribution of Estimated Demand(PageRank(%))",
       x="PageRank(%)",
       y="Frequency")




#Tranformed Distribution
EstiCapDis_Tran <- ggplot(EstiFlow)+
  geom_histogram(aes(x = log10(EstiFlow$PageRank)),
                 fill = brewer.pal(7,"Blues")[7],
                 col =brewer.pal(7,"Greys")[2],
                 alpha = 0.8,
                 bins = 60)+
  scale_x_continuous(breaks = seq(round(min(log10(EstiFlow$PageRank)),2),
                                  round(max(log10(EstiFlow$PageRank)),2),
                                  round((max(log10(EstiFlow$PageRank))-min(log10(EstiFlow$PageRank)))/10,1)))+
  scale_y_continuous()+
  labs(title="Distribution of log10((PageRank(%))",
       x="log10(PageRank(%))",
       y="Frequency")


EstiCapDis
EstiCapDis_Tran
London_Base+EstiCap_Map
