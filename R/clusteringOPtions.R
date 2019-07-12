#' @import dbscan

#' @export 

clusteringOPtions=function(cluster,dim){
  
  if(dim==2){
    rdis.test=as.matrix(cluster[,c("x","y")])
    set.seed(0)
    chec=data.frame(matrix(ncol=0,nrow=0))
    
    for(i in seq(5,100,5)){
      test.hd=hdbscan(rdis.test,minPts = i)
      
      
      
      che=as.data.frame(cbind(max(test.hd$cluster),i))
      chec=rbind(chec,che)
    }
    colnames(chec)=c("Modules","Cutoff Value")
  }
  
  if(dim==3){
    rdis.test=as.matrix(cluster[,c("x","y","z")])
    set.seed(0)
    chec=data.frame(matrix(ncol=0,nrow=0))
    
    for(i in seq(5,100,5)){
      test.hd=hdbscan(rdis.test,minPts = i)
      
      
      
      che=as.data.frame(cbind(max(test.hd$cluster),i))
      chec=rbind(chec,che)
    }
    colnames(chec)=c("Modules","Cutoff Value")
  }  
  
  return(chec) 
} 
