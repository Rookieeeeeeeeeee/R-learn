## write environment :R(4.1.1) and Rstudio 
## 

## install packages ------------------------------------------------------------
if (!requireNamespace("BiocManager")){
  install.packages("BiocManager")
}
if (!requireNamespace("edgeR")){
  BiocManager::install("edgeR")
}
if (!requireNamespace("clusterProfiler")){
  BiocManager::install("clusterProfiler")
}
if (!requireNamespace("org.Hs.eg.db")){
  BiocManager::install("org.Hs.eg.db",'enrichplot','GOSemSim')
}
BiocManager::install('enrichplot')
BiocManager::install('GOSemSim')
BiocManager::install('ggnewscale')
install.packages('cowplot')

## library packages ------------------------------------------------------------
library(edgeR)
library(clusterProfiler)
library(org.Hs.eg.db)
library(enrichplot)
library(GOSemSim)
library(ggnewscale)
library(cowplot)

## uniprot ID transform---------------------------------------------------------
keytypes(org.Hs.eg.db)## see the  types that 'org.Hs.eg.db' can transform
uniprotid <- read.csv('D:/R/142/1410uniprotID.csv') ## one col excel
id <- as.matrix(uniprotid)
class(uniprotid)
gene <- bitr(
  id,
  fromType = 'UNIPROT',
  toType = c('UNIPROT','GENENAME','ENSEMBL','SYMBOL','ENTREZID'), ## 'ENTREZID' = 'geneID'
  OrgDb = org.Hs.eg.db
)

gene$UNIPROT[which(duplicated(gene$UNIPROT))]
library(dplyr)
gene_unique <- gene %>% dplyr::distinct(UNIPROT)  
#3 get the unique mapping result , will find 1861/2046 = 0.9095 ,
# in the mapping result,9.04% of input gene IDs are fail to map...  it can be validate 


## the needed excel colname :"gene"   "logFC"  "logCPM" "PValue" "FDR"    "symbol" "geneID" 
##  "logCPM" can be delete , identical(ego,ego2) #[1] TRUE
# identical(ekegg,ekegg2) #[1] TRUE

## the needed excel colname :"ID"   "logFC"  "logCPM" "PValue" "FDR"    "symbol" "geneID" 

## GO---------------------------------------------------------------------------
## filter up gene 
flt <- deg_df[deg_df$logFC > 1 & deg_df$FDR < 0.05,]
up_gene <- flt$geneID ## get the ENTREZID

ego <- enrichGO(up_gene,
                OrgDb = org.Hs.eg.db,
                keyType = "ENTREZID",
                ont = "BP")
## KEGG-------------------------------------------------------------------------
ekegg <- enrichKEGG(up_gene,
                    organism = "hsa",
                    keyType = "kegg")

## GOSemSim demo ---------------------------------------------------------------
data(geneList, package="DOSE")
de <- names(geneList)[abs(geneList) > 2]
bp <- enrichGO(de, ont="BP", OrgDb = 'org.Hs.eg.db')
bp <- pairwise_termsim(bp)  ## start 
bp2 <- simplify(bp, cutoff=0.7, by="p.adjust", select_fun=min)
p1 <- emapplot(bp)
p2 <- emapplot(bp2)
cowplot::plot_grid(p1, p2, ncol=2, labels = LETTERS[1:2])


## Visualization of functional enrichment result--------------------------------
## Bar Plot---------------------------------------------------------------------
library(DOSE)
data(geneList)
de <- names(geneList)[abs(geneList) > 2]
ego <- enrichGO(up_gene,
                OrgDb = org.Hs.eg.db,
                keyType = "ENTREZID",
                ont = "BP")

edo <- enrichDGN(de)
library(enrichplot)
barplot(edo, showCategory=20) 
mutate(edo, qscore = -log(p.adjust, base=10)) %>% 
  barplot(x="qscore")

## Dot plot
edo2 <- gseDO(geneList) ## GSEA analysis(Gene Set Enrichment Analysis)
p3 <- dotplot(edo, showCategory=30) + ggtitle("dotplot for ORA")
p4 <- dotplot(edo2, showCategory=30) + ggtitle("dotplot for GSEA")
cowplot::plot_grid(p3, p4, ncol=2, labels = LETTERS[1:2])


## Gene-Concept Network
## convert gene ID to Symbol


edox <- setReadable(edo, 'org.Hs.eg.db', 'ENTREZID')
p5 <- cnetplot(edox, foldChange=geneList)
## categorySize can be scaled by 'pvalue' or 'geneNum'
p6 <- cnetplot(edox, categorySize="pvalue", foldChange=geneList)
p7 <- cnetplot(edox, foldChange=geneList, circular = TRUE, colorEdge = TRUE) 
cowplot::plot_grid(p5, p6, p7, ncol=3, labels=LETTERS[1:3], rel_widths=c(.8, .8, 1.2))
