#' @import dbscan

#' @export 
 
clusteringOptics=function(cluster,cutoff,dim){
  ## EXTRACT THE X AND Y COORDINATES FROM THE BH-SNE RESULT.
  
  
  
  
  if(dim==2){
    rdis.sne.xy=as.matrix(cluster[,c("x","y")])
    set.seed(0)
    
    test.hd=hdbscan(rdis.sne.xy,minPts = cutoff)
    
    cluster.test=data.frame(matrix(ncol=0,nrow=0)); cluster.test=as.data.frame(cbind(rdis.sne.xy,scan.sne2$cluster)); colnames(cluster.test)=c("x","y","colors");rownames(cluster.test)=rownames(cluster)
    
    renum=data.frame(colors=as.numeric(unique(cluster.test$colors)))

    
    n=nrow(renum)-1
    palette(c(rainbow(n)))
    plot(0:n, pch=CIRCLE<-16, cex=3, col=rainbow(n))
    
    cluster.test$colors[cluster.test$colors==0]="grey"
    
    p=ggplot(cluster.test,aes(x,y))+theme_bw()+geom_point(size=4,pch=21,fill=cluster.test$colors)+theme(legend.position="none")
    print(p)
    print("Number of modules:")
    print(n)
  }
  
  if(dim==3){
    rdis.sne.xyz=as.matrix(cluster[,c("x","y","z")])
    set.seed(0)
    
    
    test.hd=hdbscan(rdis.sne.xyz,minPts = cutoff)
    # plot(test.hd,scale="suggest",show_flat=T)
    
    cluster.test=data.frame(matrix(ncol=0,nrow=0)); cluster.test=as.data.frame(cbind(rdis.sne.xyz,test.hd$cluster)); colnames(cluster.test)=c("x","y","z","colors");rownames(cluster.test)=rownames(cluster)
    renum=data.frame(colors=as.numeric(unique(cluster.test$colors)))
    
    n=nrow(renum)-1
    palette(c(rainbow(n)))
    
    
    cluster.test$colors[cluster.test$colors==0]="grey"
    plot3d(cluster.test$x,cluster.test$y,cluster.test$z,col=cluster.test$colors)
    
    
    print("Number of modules:")
    print(n)
  }
  
  return(cluster.test)
}
