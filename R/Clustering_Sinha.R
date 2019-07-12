#' @importFrom clusterSim data.Normalization

#' @export 
data_norm=function(trans,sub){
  
  names=colnames(trans[2:ncol(trans)]) ##Pull column names
  colnames(trans)=c("itag",names) ##Rename columns
    
  if(!(missing(sub))){
    # names=colnames(sub[2:ncol(sub)]) ##Pull column names
    colnames(sub)=c("itag") ##Rename columns
  
    ## Subset
    data3=trans[which(trans$itag %in% sub$itag),]
    rownames(data3)=data3$itag
    data3=data3[,c(2:ncol(data3))]
  }


  if(missing(sub)){
    data3=trans
    rownames(data3)=data3$itag #data.eqtl=data.eqtl[,c(5:19)] ## STILL NEEDS SET TO DATA POINT LENGTH (COLUMNS)
    data3=data3[,c(2:ncol(data3))] ## LENGTH OF DATA POINTS (COLUMNS)
  }
  
  ## LOG2 TRANSFORMATION / NORMALIZATION OF DATA POINTS BY GENE / NON-NUMERIC VALUE REMOVAL
  countsadj=log2(data3)
  countsadj=as.matrix(countsadj)
  countsadj[is.infinite(countsadj)]=0
  countsadj[is.na(countsadj)]=0
  
  norm.data=data.Normalization (countsadj,type="n1",normalization="row")
  norm.data[is.na(norm.data)]=0; norm.data[is.infinite(norm.data)]=0
  
  return(norm.data)
}



clusterGenes=function(map){
  mapping=map[map$colors != "grey",]
  return(mapping)
}



indOver=function(set,clusters){
  
  overlay=clusters[clusters$colors != "grey",]
  
  cluster=overlay
  ind.genes=set
  
  cluster$genes=rownames(cluster)
  cluster$genes=substr(cluster$genes,1,14)
  
  names=colnames(ind.genes[2:ncol(ind.genes)]) ##Pull column names
  colnames(ind.genes)=c("itag",names) ##Rename columns
  ind.genes$itag=substr(ind.genes$itag,1,14)
  
  #MAP
  cluster.color=cluster
  
  # INDEPENDENT GENES
  cluster.color.number=as.data.frame(cluster.color[which(cluster.color$genes %in% ind.genes$itag),])
  cluster.color$colors=NA
  cluster.color.number$colors="red"
  
  #NA's
  cluster.color$colors[is.na(cluster.color$colors)]="grey"
  cluster.grey=cluster.color[cluster.color$colors=="grey",]
  cluster.red=cluster.color.number
  
  #Plot
  p=ggplot(cluster.grey,aes(x,y))+theme_bw()+geom_point(size=1.5,colour=cluster.grey$colors)+geom_point(data=cluster.red,fill=cluster.red$colors,pch=21,size=5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(legend.position="none")+theme(legend.position="none")+theme(axis.text.x=element_blank(),axis.text.y=element_blank())+xlab("")+ylab("")
  print(p)
}


