#' @import rgl

#' @export 
showtimeMapping=function(base,clusters,dim){
  ## READ IN PREVIOUSLY GENERATED FILES. CHANGE NAMES APPROPRIATLEY.
  
  overlay=clusters[clusters$colors != "grey",]
  
  cluster=overlay
  map=base
  map$genes=rownames(map)
  cluster$genes=rownames(cluster)
  
  ## ADD COLOR TO CLUSTER OBJECT AND PICK SPECIFIC SUBSET. N IS TOTAL NUMBER OF CLUSTERS.
  map$colors=0
  n=max(cluster$colors)
  for(i in 1:nrow(cluster)){
    map[which(cluster[i,"genes"] == map$genes),"colors"]=cluster[i,"colors"]
  }
  
  #MAP
  
  cluster.color=map
  cluster.color$colors[cluster.color$colors==0]="light grey"
  
  cluster.color.sub=cluster.color[cluster.color$colors=="light grey",]
  cluster.color=cluster.color[cluster.color$colors != "light grey",]
  
  ## PLOT
  if(dim==2){
    p=ggplot(cluster.color,aes(x,y))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(legend.position="none")+geom_point(data=cluster.color.sub,size=1.5,color=cluster.color.sub$colors)+geom_point(size=3.5, pch=21, fill=cluster.color$colors)+theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())+theme(axis.ticks.x=element_blank())+theme(axis.ticks.y=element_blank())+xlab("")+ylab("")
  print(p)
  }
  if(dim==3){
  # plot3d(cluster.color$x,cluster.color$y,cluster.color$z,col = cluster.color$colors,size = 5)
  # plot3d(cluster.color.sub$x,cluster.color.sub$y,cluster.color.sub$z,col = cluster.color.sub$colors,add = TRUE,size = 2)
  
  points3d(cluster.color$x,cluster.color$y,cluster.color$z,col=cluster.color$colors,size=5)
  points3d(cluster.color.sub$x,cluster.color.sub$y,cluster.color.sub$z,col = cluster.color.sub$colors,add = TRUE,size = 2)
  }
}
  
