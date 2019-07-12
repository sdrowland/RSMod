#' @import clusterSim

#' @export 

## Boxplot of single module
module.patterns=function(modules,data){
mod.num=length(unique(modules$colors))-1

for(k in 1:mod.num){
  list=c(k)
  
  Rdis.data.sne=modules
  sub=as.data.frame(data$itag)
  
  Rdis.data.sne.sub=Rdis.data.sne[which(Rdis.data.sne$colors %in% list),]
  Rdis.data.sne.sub$itag=rownames(Rdis.data.sne.sub)
  
  colnames(sub)=c("itag")
  Rdis.data.sne.sub=Rdis.data.sne.sub[Rdis.data.sne.sub$itag %in% sub$itag,]
  pattern.sne=data[which(data$itag %in% Rdis.data.sne.sub$itag),]
  
  rownames(pattern.sne)=pattern.sne$itag
  pattern.sne=pattern.sne[,c(2:ncol(pattern.sne))]
  
  pattern.sne=log2(pattern.sne);pattern.sne=as.matrix(pattern.sne)
  pattern.sne[is.infinite(pattern.sne)]=0
  
  pattern.sne=data.Normalization (pattern.sne,type="n1",normalization="row")
  pattern.sne=as.matrix(pattern.sne);pattern.sne[is.nan(pattern.sne)]=0

  pattern.melt=melt(pattern.sne)
  order=colnames(pattern.sne)

  svg(file=paste("Module",list," Genes Line Expression.svg"),height=720,width=1280)
  p=  ggplot(pattern.melt,aes(Var2))+theme_bw()+geom_path(aes(y=value,group=Var1),alpha=0.5,colour="darkblue")+xlab("Genotype")+scale_x_discrete(limit=order)+ylab("Relative Expression")+ggtitle("")+theme(axis.text.x=element_text(angle=-45,vjust=1,hjust=0,size=16),axis.title.x=element_text(size=18),axis.title.y=element_text(size=18),title=element_text(size=20))+ggtitle(k)
  print(p)
  #+annotate(geom="text",x=5,y=5,label="A",size=10)
  dev.off()
  # 
  svg(file=paste("Module",list,"Boxplot Expression.svg"),height=720,width=1280)
  p=ggplot(pattern.melt,aes(x=Var2))+theme_bw()+geom_boxplot(aes(y=value),outlier.size=1)+theme(axis.text.x=element_text(angle=90))+xlab("Introgression Line")+ylab("Relative Transcript Level")+ggtitle("")+scale_x_discrete(limit=order)+geom_hline(yintercept=c(-1,1),linetype=2,size=1)+theme(axis.text.x=element_text(angle=-90,vjust=.15,hjust=0,size=14))+theme(axis.text.y=element_text(size=14))+theme(axis.title.x=element_text(size=16))+theme(axis.title.y=element_text(size=16,vjust=1.3))+ggtitle(k)
  print(p)
  # #+annotate(geom="text",x=5,y=5,label="A",size=10)
  dev.off()
  
  k=k+1
}
}