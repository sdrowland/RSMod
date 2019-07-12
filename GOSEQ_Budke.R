# source("http://bioconductor.org/biocLite.R")
# # biocLite("rtracklayer")
# biocLite("goseq")
# biocLite("GO.db")
# # biocLite("AnnotationDbi")

library(goseq)
library(GO.db)

#Need to first start with all the genes that had enough reads to analyze - as the background expected values
All.genes.analyzed <- read.table("All-Weeks-Expression-Final.csv", header=TRUE, sep = ",", check.names = FALSE, stringsAsFactors=FALSE)
head(All.genes.analyzed) 
nrow(All.genes.analyzed)
ncol(All.genes.analyzed)

All.genes.analyzed.reduced <- as.data.frame(All.genes.analyzed$itag)
colnames(All.genes.analyzed.reduced) <- c("itag")
head(All.genes.analyzed.reduced) 
nrow(All.genes.analyzed.reduced)
ncol(All.genes.analyzed.reduced)

#Need to have the length values for all of them.
PlantCellSuppData4 <- read.table("PlantCell_tpc112391_SupplementalDataset4_subset4.txt", header=TRUE, sep = "\t", check.names = FALSE)

names(PlantCellSuppData4)
nrow(PlantCellSuppData4)
ncol(PlantCellSuppData4)
head(PlantCellSuppData4, n = 10L)
tail(PlantCellSuppData4, n = 10L)
PlantCellSuppData4$length <- PlantCellSuppData4$end - PlantCellSuppData4$begin #Thise calculates the length of every gene analyzed
head(PlantCellSuppData4)
tail(PlantCellSuppData4)

PlantCellSuppData4.reduced <- as.data.frame(PlantCellSuppData4$itag)
colnames(PlantCellSuppData4.reduced) <- c("itag")

PlantCellSuppData4.reduced$length <- PlantCellSuppData4$length
head(PlantCellSuppData4.reduced)
nrow(PlantCellSuppData4.reduced)
ncol(PlantCellSuppData4.reduced)

PlantCellSuppData4.reduced$itag=substr(PlantCellSuppData4.reduced$itag,1,14)
All.genes.analyzed.reduced$itag=substr(All.genes.analyzed.reduced$itag,1,14)
# Now need to put together these two pieces of data 20,795 genes analyzed with their lengths. 
eQTL.All.length <- merge(All.genes.analyzed.reduced, PlantCellSuppData4.reduced, by.x = "itag", by.y = "itag", sort = FALSE, all.x=TRUE)
head(eQTL.All.length)
nrow(eQTL.All.length)
ncol(eQTL.All.length)
eQTL.All.length$itag=substr(eQTL.All.length$itag,1,14)
##-----------------------------------------------------------##

# ##-----------------------------------------------------------##
# ## FOR COMMUNITY MEMBERSHIP OF NETWORKING:
# lits=c(3)
# genesSig.eQTL.Large <- read.csv("ShadeHeteroboth_up.csv", header=TRUE)
# genesSig.eQTL.Large=genesSig.eQTL.Large[which(genesSig.eQTL.Large$communities %in% lits),]
# genesSig.eQTL.Large$x=0;genesSig.eQTL.Large$y=0
# colnames(genesSig.eQTL.Large)=c("itag","ave.rank","sd.rank")#"row",
# ##---------------------------------------

# FOR MODULE LOOPING:----
modules2=as.numeric(unique(modules$colors[which(!(modules$colors=="grey"))]))

for(i in modules2){
genesSig.eQTL.Large=modules
genesSig.eQTL.Large=genesSig.eQTL.Large[which(genesSig.eQTL.Large$colors==i),]
genesSig.eQTL.Large$itag=rownames(genesSig.eQTL.Large)
genesSig.eQTL.Large$itag=substr(genesSig.eQTL.Large$itag,1,14)

##--------------------

nrow(genesSig.eQTL.Large)

genesSig.eQTL <- as.data.frame(unique(genesSig.eQTL.Large$itag)) # Pull out just the column with the gene names & eliminate duplicates
colnames(genesSig.eQTL ) <- c("itag")     # Change the name of the column
genesSig.eQTL$Sig.eQTL <- "1" #Add a column full of ones to tell the computer that these are the genes with sig eQTL
head(genesSig.eQTL)
nrow(genesSig.eQTL)
ncol(genesSig.eQTL) 

GO.eQTL <- merge(eQTL.All.length, genesSig.eQTL, by.x = "itag", by.y = "itag", all.x=TRUE)
head(GO.eQTL)
nrow(GO.eQTL)
ncol(GO.eQTL) 

#This replaces all the NAs in the sig eQTL column with zeros
GO.eQTL$Sig.eQTL[is.na(GO.eQTL$Sig.eQTL)] <- 0 

# write.csv(GO.eQTL, "No7.2.eQTL.All.SigEQTL.length.csv")

#Assigning a new name and checking the data
AllSig.GO.eQTL <- GO.eQTL
head(AllSig.GO.eQTL)
nrow(AllSig.GO.eQTL)
ncol(AllSig.GO.eQTL) 

#This sets which genes are part of the analyses compared to the subset of significant genes.
genes = as.integer(AllSig.GO.eQTL$Sig.eQTL)
names(genes) = AllSig.GO.eQTL$itag
table(genes)
length(genes)

#Calculates a Probability Weighting Function for a set of genes based on a given set of biased data 
#(usually gene length) and each genes status as differentially expressed or not.
pwf = nullp(genes,bias.data=AllSig.GO.eQTL$length)
head(pwf)

cate <- read.table("melted.GOTable.txt",header=TRUE)
cate$itag=substr(cate$itag,1,14)
cate.SLIM <- read.table("melted.GOSlim.txt",header=TRUE)
cate.SLIM$itag=substr(cate.SLIM$itag,1,14)

#Does selection-unbiased testing for category enrichment amongst differentially expressed (DE) genes for RNA-seq data. 
#From GO Table
GO.wall = goseq(pwf,gene2cat = cate)
head(GO.wall)

#From Go Slim Table
GO.wall.SLIM = goseq(pwf,gene2cat = cate.SLIM)
head(GO.wall.SLIM)

#Adjusting the p.value for multiple tests (either due to multiple GO categories or multiple genes)
#And then after this correction pulling out the categories that are signigficantly enriched compared to background
enriched.GO = GO.wall$category[p.adjust(GO.wall$over_represented_pvalue, method = "BH") < 0.05]
head(enriched.GO)
enriched.GO.SLIM = GO.wall.SLIM$category[p.adjust(GO.wall.SLIM$over_represented_pvalue, method = "BH") < 0.05]
head(enriched.GO.SLIM)

#Take these enriched categories
my.GO <- as.character(enriched.GO)
my.GO.SLIM <- as.character(enriched.GO.SLIM)

#Ask what their go terms are from the tables
my.GO.table <- Term(my.GO)
my.GO.SLIM.table <- Term(my.GO.SLIM)


#Arrange them as a matrix
t <- as.matrix(my.GO.table)
colnames(t) <- c("EnrichedCategories")
t.SLIM <- as.matrix(my.GO.SLIM.table)
colnames(t.SLIM) <- c("EnrichedCategories.SLIM")

if(length(my.GO.table)>0){
write.table(t, file=paste("3D-Module-whatever ",i," GO.txt"))
}

if(length(my.GO.SLIM.table)>0){
write.table(t.SLIM, file=paste("3D-Module-whatever ",i," GO_SLIM.txt"))
}
}



# write.table(t, "Shade_Knox_Up.txt")
# write.table(t.SLIM, "Shade_Knox_Up_Slim.txt")
# 
# 
# 
# # Extra added that I need to put into the loop
# 
# #Need to get the headings into a separate column
# t.GOCat <- as.matrix(rownames(t))
# colnames(t.GOCat) <- c("GoCategories")
# t.GOCat.SLIM <- as.matrix(rownames(t.SLIM))
# colnames(t.GOCat.SLIM) <- c("GoCategories.SLIM")
# 
# 
# write.table(t.GOCat, "No7.4.GOs_ALLeQTL_genesGoCat.txt", sep="\t", quote = FALSE, row.names = FALSE)
# write.table(t.GOCat.SLIM, "No7.4.GOSLIMs_ALLeQTL_genesGoCat.txt", sep="\t", quote = FALSE, row.names = FALSE)
# 
# # Load up files from the saved versions. Could not combine before saving
# t <- read.csv("No7.3.GOs_ALLeQTL_genes.csv", header=TRUE)
# t.SLIM <- read.csv("GO_3D_Slim.csv", header=TRUE)
# t.GOCat <- read.table("No7.4.GOs_ALLeQTL_genesGoCat.txt", header=TRUE, sep = "\t", check.names = FALSE)
# t.GOCat.SLIM <- read.table("No7.4.GOSLIMs_ALLeQTL_genesGoCat.txt", header=TRUE, sep = "\t", check.names = FALSE)
# 
# #All of the other annotations go with the genes. Not the GO categories
# 
# #After reloading the files need to combine them together into a single one
# t.GOCat[2] <- t[1]
# t.GOCat
# write.table(t.GOCat, "No7.5.ALLeQTL_GO.txt", sep="\t", quote = FALSE, row.names = FALSE)
# 
# t.GOCat.SLIM[2] <- t.SLIM[1]
# t.GOCat.SLIM
# write.table(t.GOCat.SLIM, "No7.5.ALLeQTL_GO.SLIM.txt", sep="\t", quote = FALSE, row.names = FALSE)
# 
# #No idea how to combine these two lists together. Something about the way they are saved as a matrix. Will not work 
# 
# # Once this is working on the entire dataset then run it on each of the SOMs individually
# # Setup a loop to get this to run on all 9 in series
# # Need to finish the figures for the Spearman and put them into the powerpoint. 
# 
# 
