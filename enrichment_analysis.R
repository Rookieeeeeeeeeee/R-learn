## write environment :R(4.1.1) and Rstudio 
## Reference :: https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html

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
  BiocManager::install("org.Hs.eg.db") ##  update 
}
BiocManager::install('enrichplot')
BiocManager::install('GOSemSim')
BiocManager::install('ggnewscale')
install.packages('cowplot')
BiocManager::install('ggupset')
BiocManager::install('ggridges')

## library packages ------------------------------------------------------------
library(edgeR)
library(clusterProfiler)
library(org.Hs.eg.db)
library(enrichplot)
library(GOSemSim)
library(ggnewscale)
library(cowplot)
library(ggupset)
library(ggridges)
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
deg_df <- read.csv('D:/BaiduNetdiskDownload/富集分析/enrichment_analysis/edgeR_DEG.csv')
deg_df$symbol <- do.call(rbind, strsplit(deg_df$gene, "|", fixed = TRUE))[,1]
deg_df$geneID <- do.call(rbind, strsplit(deg_df$gene, "|", fixed = TRUE))[,2]
head(deg_df)

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

## GOSemSim simplify demo ---------------------------------------------------------------
data(geneList, package="DOSE")
de <- names(geneList)[abs(geneList) > 2]   ## de == up_gene
bp <- enrichGO(de, ont="BP", OrgDb = 'org.Hs.eg.db')  ## ego == bp 
bp2 <- pairwise_termsim(bp)  ## start 
bp3 <- simplify(bp2, cutoff=0.7, by="p.adjust", select_fun=min)

p1 <- emapplot(bp2)
p2 <- emapplot(bp3)
cowplot::plot_grid(p1, p2, ncol=2, labels = LETTERS[1:2])

ego2 <- pairwise_termsim(ego) 
ego3 <- simplify(ego2, cutoff=0.7, by="p.adjust", select_fun=min)

p11 <- emapplot(ego2)
p22 <- emapplot(ego3)
cowplot::plot_grid(p11, p22, ncol=2, labels = LETTERS[1:2])

## Visualization of functional enrichment result--------------------------------

## Bar Plot---------------------------------------------------------------------
library(DOSE)
data(geneList)
de <- names(geneList)[abs(geneList) > 2]
edo <- enrichDGN(de)  ## edo == ego    edo == ego_test(enrichDGN(de))

barplot(edo, showCategory=20) 
mutate(edo, qscore = -log(p.adjust, base=10)) %>% 
  barplot(x="qscore")

barplot(ego, showCategory=20) 
mutate(ego, qscore = -log(p.adjust, base=10)) %>% 
  barplot(x="qscore")

## Dot plot---------------------------------------------------------------------
edo2 <- gseDO(geneList) ## GSEA analysis(Gene Set Enrichment Analysis)
p3 <- dotplot(edo, showCategory=30) + ggtitle("dotplot for ORA")
p4 <- dotplot(edo2, showCategory=30) + ggtitle("dotplot for GSEA")
cowplot::plot_grid(p3, p4, ncol=2, labels = LETTERS[1:2])


## Gene-Concept Network---------------------------------------------------------
## convert gene ID to Symbol


edox <- setReadable(edo, 'org.Hs.eg.db', 'ENTREZID')  ## mapping geneID to gene Symbol
p5 <- cnetplot(edox, foldChange = geneList)
## categorySize can be scaled by 'pvalue' or 'geneNum'
p6 <- cnetplot(edox, categorySize="pvalue", foldChange=geneList)
p7 <- cnetplot(edox, foldChange=geneList, circular = TRUE, colorEdge = TRUE) 
cowplot::plot_grid(p5, p6, p7, ncol=3, labels=LETTERS[1:3], rel_widths=c(.8, .8, 1.2))

edox2 <- setReadable(ego, 'org.Hs.eg.db', 'ENTREZID')  ## mapping geneID to gene Symbol
p55 <- cnetplot(edox2, foldChange = 2 ^ deg_df$logFC)
## categorySize can be scaled by 'pvalue' or 'geneNum'
p66 <- cnetplot(edox, categorySize="pvalue", foldChange=2 ^ deg_df$logFC)
p77 <- cnetplot(edox, foldChange=2 ^ deg_df$logFC, circular = TRUE, colorEdge = TRUE) 
cowplot::plot_grid(p5, p6, p7, ncol=3, labels=LETTERS[1:3], rel_widths=c(.8, .8, 1.2))


## Heatmap-like functional classification---------------------------------------
p8 <- heatplot(edox, showCategory=5)
p9 <- heatplot(edox, foldChange=geneList, showCategory=5)
cowplot::plot_grid(p8, p9, ncol=1, labels=LETTERS[1:2])

p88 <- heatplot(edox2, showCategory=5)
p99 <- heatplot(edox2, foldChange=geneList, showCategory=5)
cowplot::plot_grid(p88, p99, ncol=1, labels=LETTERS[1:2])

## Tree plot--------------------------------------------------------------------
edox3 <- pairwise_termsim(edox)
p011 <- treeplot(edox3)
p012 <- treeplot(edox3, hclust_method = "average")
aplot::plot_list(p011, p012, tag_levels='A')


## enrichment result -----------------------------------------------------------

edo <- pairwise_termsim(edo)
p013 <- emapplot(edo)
p014 <- emapplot(edo, cex_category=1.5)
p015 <- emapplot(edo, layout="kk")
p016 <- emapplot(edo, cex_category=1.5,layout="kk") 
cowplot::plot_grid(p013, p014, p015, p016, ncol=2, labels=LETTERS[1:4])

p0133 <- emapplot(ego2)
p0144 <- emapplot(ego2, cex_category=1.5)
p0155 <- emapplot(ego2, layout="kk")
p0166 <- emapplot(ego2, cex_category= 0.8,layout="kk") 
cowplot::plot_grid(p013, p014, p015, p016, ncol=2, labels=LETTERS[1:4])


## UpSet Plot-------------------------------------------------------------------

upsetplot(ego)    ## GO 
upsetplot(edo2)  ## GSEA

## ridgeline plot for expression distribution of GSEA result--------------------
ridgeplot(edo2)

## running score and preranked list of GSEA result------------------------------
p017 <- gseaplot(edo2, geneSetID = 1, by = "runningScore", title = edo2$Description[1])
p018 <- gseaplot(edo2, geneSetID = 1, by = "preranked", title = edo2$Description[1])
p019 <- gseaplot(edo2, geneSetID = 1, title = edo2$Description[1])
cowplot::plot_grid(p017, p018, p019, ncol=1, labels=LETTERS[1:3])

gseaplot2(edo2, geneSetID = 1:3, title = edo2$Description[1],pvalue_table = TRUE,
          color = c("#E495A5", "#86B875", "#7DB0DD"), ES_geom = "dot")## another method 
