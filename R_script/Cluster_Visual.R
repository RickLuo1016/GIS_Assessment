library(ggplot2)
library(reshape2)
# Store the cluster result

# Cluster result
Cluster_result <- cmeans(OD_cleaned_shrink[,4:9],6,m = 1.5)
MPC(Cluster_result$membership)

# Join in the cluster number
OD_cleaned_shrink_clustered <- cbind(OD_cleaned_shrink,data.frame(Cluster_result$cluster))

# Visualise the pattern
# Function to draw
Draw_pattern <- function(cluster_Num_sample,Clu_Num){
  cluster_Num_sample <- cbind(cluster_Num_sample,rownames(cluster_Num_sample))
  cluster_Num_sample <- melt(cluster_Num_sample,id.vars=17,measure.vars=4:9)
  names(cluster_Num_sample) <- c("X1","X2","X3")
  
  
  cluster_P <- ggplot(cluster_Num_sample,aes(x=X2,y=X3,group=X1))+
    geom_line(color=brewer.pal(7,"Blues")[7],size=1,alpha=0.2)+
    scale_y_continuous(breaks=c(0:10)/10)+
    labs(title=paste("Cluster",Clu_Num,"Sample"),
         x="Time Bands",
         y="Percentages of Travel Flows")
  
  return (cluster_P)
}
colnames(OD_cleaned_shrink_clustered)[16]="Cluster_result"


# Produce the clusters plot
# cluster 1
cluster_1 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 1,]
cluster_1_sample <- cluster_1[sample(rownames(cluster_1),1000),]
cluster_P1 <- Draw_pattern(cluster_1_sample,'1')

print(cluster_P1)

# cluster 2
cluster_2 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 2,]
cluster_2_sample <- cluster_2[sample(rownames(cluster_2),1000),]
cluster_P2 <- Draw_pattern(cluster_2_sample,'2')

print(cluster_P2)

# cluster 3
cluster_3 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 3,]
cluster_3_sample <- cluster_3[sample(rownames(cluster_3),1000),]
cluster_P3 <- Draw_pattern(cluster_3_sample,'3')

print(cluster_P3)

# cluster 4
cluster_4 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 4,]
cluster_4_sample <- cluster_4[sample(rownames(cluster_4),1000),]
cluster_P4 <- Draw_pattern(cluster_4_sample,'4')

print(cluster_P4)

# cluster 5
cluster_5 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 5,]
cluster_5_sample <- cluster_5[sample(rownames(cluster_5),1000),]
cluster_P5 <- Draw_pattern(cluster_5_sample,'5')

print(cluster_P5)

# cluster 6
cluster_6 <- OD_cleaned_shrink_clustered[OD_cleaned_shrink_clustered[16]== 6,]
cluster_6_sample <- cluster_6[sample(rownames(cluster_6),1000),]
cluster_P6 <- Draw_pattern(cluster_6_sample,'6')

print(cluster_P6)

