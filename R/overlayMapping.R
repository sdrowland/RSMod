#' @import manipulate

#' @export 
ModOverlay=function(base,clusters,n,dim){
  
  if(dim==3){
    
    color=1:length(n)
    cluster=clusters
    map=base
    map$genes=rownames(map)
    cluster$genes=rownames(cluster)
    maxClust=length(unique(cluster$colors))
    
    cluster.split=split(cluster,cluster$colors)
    
    test.selection=as.matrix(unique(cluster$colors)); test.selection=as.data.frame(sort(test.selection)); test.selection$order=1:nrow(test.selection);
    n2=data.frame(); for(j in n){n2=rbind(n2,test.selection[which(test.selection$`sort(test.selection)`==j),])}
    
    cluster.split.sub=cluster.split[n2$order]
    
    for(i in color){
      cluster.split.sub[[i]]$colors=n2$order[i]
    }
    
    cluster.sub=do.call(rbind,cluster.split.sub)
    
    #MAP
    cluster.color=map
    cluster.color$colors="light grey"
    
    cluster.color.sub=cluster.sub
    
    ## PLOT
    
    plot3d(cluster.color$x,cluster.color$y,cluster.color$z,col=cluster.color$colors,xlab = "",ylab = "",zlab = "")
    plot3d(cluster.color.sub$x,cluster.color.sub$y,cluster.color.sub$z,col=cluster.color.sub$colors,size=12,add = TRUE)
    plot3d(cluster.color.sub$x,cluster.color.sub$y,cluster.color.sub$z,size=16,add = TRUE)
    legend3d("bottom",legend=paste("Module:", unique(n2$`sort(test.selection)`)),pch=16,col=unique(cluster.color.sub$colors),cex=1.5)
    
  }
  
  if(dim==2){
    
  overlay=clusters[which(!(clusters$colors=="grey")),]
  overlay$colors=as.numeric(overlay$colors)
  overlay=overlay[do.call(order,overlay["colors"]),]
  
  maxClust=length(unique(overlay$colors))
  
  if(n==1){  
    manipulate(singleModOverlay(base=base,overlay=overlay,n=c(mod1),color=c(col1),dim2=dim), mod1=slider(1,maxClust,label="First Module"),col1=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"))
  }
  
  if(n==2){
    manipulate(singleModOverlay(base=base,overlay=overlay,n=c(mod1,mod2),color=c(col1,col2),dim2=dim), mod1=slider(1,(maxClust),label="First Module"),col1=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod2=slider(1,(maxClust),label="Second Module"),col2=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"))
  }
  
  if(n==3){
    manipulate(singleModOverlay(base=base,overlay=overlay,n=c(mod1,mod2,mod3),color=c(col1,col2,col3),dim2=dim), mod1=slider(1,(maxClust),label="First Module"),col1=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod2=slider(1,(maxClust),label="Second Module"),col2=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod3=slider(1,(maxClust),label="Third Module"),col3=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"))
  }
  
  if(n==4){
    manipulate(singleModOverlay(base=base,overlay=overlay,n=c(mod1,mod2,mod3,mod4),color=c(col1,col2,col3,col4),dim2=dim), mod1=slider(1,(maxClust),label="First Module"), col1=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod2=slider(1,(maxClust),label="Second Module"),col2=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod3=slider(1,(maxClust),label="Third Module"),col3=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"),mod4=slider(1,(maxClust),label="Third Module"),col4=picker("Red","Orange","Yellow","Green","Blue","Purple","Violet",label="Color of Module"))
  }
  }
}


#' @export 
singleModOverlay=function(base,overlay,n,color,dim2){
  
  if(missing(color)){
    color=n
  }
  
  cluster=overlay
  map=base
  map$genes=rownames(map)
  cluster$genes=rownames(cluster)
  maxClust=length(unique(cluster$colors))
  # maxClust[is.na(maxClust)]=0
  
  cluster$colors=as.factor(cluster$colors)
  # cluster$colors=factor(cluster$colors,levels=1:maxClust)
  cluster.split=split(cluster,cluster$colors)
  cluster.split.sub=cluster.split[n]
  
  for(i in 1:length(color)){
    cluster.split.sub[[i]]$colors=color[i]
  }
  
  cluster.sub=do.call(rbind,cluster.split.sub)
  
  #MAP
  cluster.color=map
  cluster.color$colors="light grey"
  
  cluster.color.sub=cluster.sub
  
  ## PLOT

  p=ggplot(cluster.color,aes(x,y))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(legend.position="none")+geom_point(data=cluster.color,size=1.5,color=cluster.color$colors)+geom_point(data=cluster.color.sub,size=3.5, pch=21, fill=cluster.color.sub$colors)+theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())+theme(axis.ticks.x=element_blank())+theme(axis.ticks.y=element_blank())+xlab("")+ylab("")
  return(p)
 
}
