#' @import Rtsne

#' @export 
clusterSNE=function(data.set,perp,dim,tha,lying,pc,iter,seed){       			
  ## SET SEED
  
  if(missing(tha)){
    tha=0.3
  }
  
  if(missing(pc)){
    pc=FALSE
  }
  
  if(missing(iter)){
    iter=1000
  }
  
  if(missing(seed)){
    seed=0
  }
  
  if(missing(lying)){
    lying=250
  }
  
  set.seed(seed)
  
  Rdis.sne=Rtsne(data.set,perplexity=perp,dims=dim,theta=tha,initial_dims=ncol(data.set),check_duplicates=FALSE,pca=pc,verbose=TRUE,max_iter=iter,stop_lying_iter = lying)
  
  print(Rdis.sne$perplexity)
  
  if(dim==2){
  Rdis.sne=as.data.frame(Rdis.sne["Y"])
  colnames(Rdis.sne)=c("x","y")
  rownames(Rdis.sne)=rownames(data.set)

  p=ggplot(Rdis.sne,aes(x,y))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+geom_point(size=1.5,pch=21,fill="grey")+theme(legend.position="none",axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.length=unit(0,"cm"))+xlab("")+ylab("")
  print(p)
  }
  
  if(dim==3){
    Rdis.sne=data.frame(Rdis.sne["Y"])
    colnames(Rdis.sne)=c("x","y","z")
    rownames(Rdis.sne)=rownames(data.set)
    
    plot3d(Rdis.sne$x,Rdis.sne$y,Rdis.sne$z,size=4)
    
  }
  
  
  return(Rdis.sne)
}