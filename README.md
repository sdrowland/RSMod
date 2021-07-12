# RSMod

BH-SNE clustering of RNA-Seq data.

Welcome! This is repository for RSMod package.
This R package was used in the published project: [Lignin-based resistance to Cuscuta campestris parasitism in Heinz resistant tomato cultivars](https://doi.org/10.1101/706861)

## Package description

- Package Name: RSMod
- Type: Package
- Title: Creates a 2D or 3D mapping of RNA-Seq Data
- Version: 0.8.30
- Date: 2018-12-10
- Author: Steven D. Rowland
- Maintainer: "Steven D. Rowland" <srowland@ucdavis.edu>
- Description: Create a 2D or 3D mapping of normalized RNA-Seq data using BH-SNE and Optics
- License: GPL >= 2
- LazyLoad: yes
- Imports:
    Rtsne,
    dbscan,
    clusterSim,
    manipulate
- RoxygenNote: 6.1.0

## Installation

You can install it using the following command:

```{r}
devtools::install_github("sdrowland/RSMod")
```

You can also simply download the "RSMod_0.8.30.tar.gz" file and then install it with:

```{r}
install.packages("RSMod_0.8.30.tar.gz", repos=NULL, type="source")
```

Please make sure you put the correct path to your tar.gz file in install.packages().

## Usage

1. The script "Generalized_Clustering.Rmd" is for clustering analysis. *Note: All files should be trimmed to exclude unwanted data and characters. See sample data sets for examples of formatting. (genes by row, replicates by column).

2. The script "Annotation.Rmd" is for gene annotation. *Note: The default setting is using tomato genome annotation ITAG3.0:

```{r}
ano.list=read.csv("ITAG3.0-annotation.csv",header=T)
```

Please replace "ITAG3.0-annotation.csv" with the gene annotation file for the species that you are using.

## Author Contact Info

- Package Author:
[Steven D. Rowland](mailto:sdrowland@ucdavis.edu)
- README Author:
[Min-Yao Jhu](mailto:minjhu@ucdavis.edu) 