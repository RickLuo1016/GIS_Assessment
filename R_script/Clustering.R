library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(e1071)
library(ggplot2)
library(RColorBrewer)
# Define the number of clusters
Clu_num <- c(2:12)

# Function using MPC to determine the optimal number of cluster
Clu_MPC <- function(data){
  
  # MPC
  MPC_Arr <- Clu_num
  
  for (i in Clu_num){
    
    # Cluster result
    Cluster_result <- cmeans(data[,4:9],i,m = 1.5)
  
    # Modified Partition Coefficient
    MPC_Arr[i-1] <- MPC(Cluster_result$membership) 
  
  print(MPC_Arr[i-1])
  }
  
  return (MPC_Arr)
}


MPC_Arr <- Clu_MPC(OD_cleaned_shrink)
MPC <- data.frame(Clu_num,MPC_Arr)

MPC_Clu <- ggplot(MPC,aes(x=Clu_num, y = MPC_Arr))+
  geom_line(color=brewer.pal(7,"Blues")[3],size=1.5)+
  geom_point(color=brewer.pal(7,"Blues")[7],size=3)+
  geom_text(color=brewer.pal(7,"Blues")[7],aes(label = round(MPC_Arr,2), hjust = -0.5))+
  scale_x_continuous(breaks=c(2:12))+
  scale_y_continuous(breaks=seq(round(min(MPC$MPC_Arr),2),round(max(MPC$MPC_Arr),2),0.02))+
  labs(title="Modified Partition Coefficient(MPC)",
       x="The number of clusters",
       y="The values of MPC")

MPC_Clu 






