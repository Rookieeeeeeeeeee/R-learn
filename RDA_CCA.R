install.packages('vegan')
library(vegan)
library(ggplot2)
setwd('D:/R')

contaminants_raw_data <- read.csv('142_result.csv',row.names = 1)
##data clean================================================================
contaminants_data <- contaminants_raw_data[,-c(1:45)]
contaminants_data <- contaminants_data[,-c(6,8,9,11)]
contaminants_data$t_temp <-contaminants_data$t_temp-273.15 
#_matrix <- otu_table[, 2:(ncol(otu_table)-1)]
#row.names(otu_matrix) <- otu_table$OTU
contaminants_data_matrix <- as.matrix(contaminants_data)


proteomics_raw <- read.csv('proteomics_142.csv', row.names = 1)
proteomics_raw <- t(proteomics_raw)
proteomics_data <- proteomics_raw[match(row.names(contaminants_data_matrix), row.names(proteomics_raw)),]##match two dataset rowname 
proteomics_data_matrix <- as.matrix(proteomics_data)

print(rownames(contaminants_data) == rownames(proteomics_data_matrix))## view whether two data's rownames are the same 

##data analysis==========================================================================
decorana(proteomics_data_matrix) ##  if DCA1's Axis lengths <4  use RDA  ,>4  use CCA

##RDA--------------------------------------------------------------------------
res <- rda(proteomics_data_matrix ~ ., contaminants_data) ## notice ~ .
plot(res)

xxxx <- summary(res)
aa <- xxxx$concont$importance
aa <- round(aa, 4)

pdat <- res$CCA
samples<-data.frame(sample = row.names(pdat$u),RDA1 = pdat$u[,1],RDA2 = pdat$u[,2])  ## sample
species<-data.frame(spece = row.names(pdat$v),RDA1 = pdat$v[,1],RDA2 = pdat$v[,2])   ##proteomics
envi<-data.frame(en = row.names(pdat$biplot),RDA1 = pdat$biplot[,1],RDA2 = pdat$biplot[,2])   ##contaminants

samples$season <- as.factor(envdat_raw$season)## add sample 's seson and foreststructure to plot
samples$foreststructure <- as.factor(envdat_raw$foreststructure)

p <- ggplot() + 
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed")  +
  
  geom_point(data = species, aes(x=RDA1, y=RDA2),size = 0.5, colour = 'pink', pch = 3) +
  geom_segment(data = species,aes(x=0, xend= RDA1, y=0, yend= RDA2 ), arrow = arrow(length = unit(0.3, "cm")), colour = 'blue') +
  geom_text(data = species,aes(x = RDA1*1.1, y = RDA2*1.1, label = spece), size = 3, colour = 'blue') +
  
  geom_segment(data = envi,aes(x=0, xend= RDA1, y=0, yend= RDA2 ), arrow = arrow(length = unit(0.3, "cm")), colour = 'red') +
  geom_text(data = envi,aes(x = RDA1*1.1, y = RDA2*1.1, label = en), size = 3, colour = 'red', check_overlap = TRUE) +
  
  geom_point(data = samples, aes(x=RDA1, y=RDA2),size = 3)  +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) + 
  
  xlab(paste('RDA1 (', aa[2,1]*100, '%)', sep ='')) + ylab(paste('RDA2 (', aa[2,2]*100, '%)', sep ='')) 

print(p)

##CCA-------------------------------------------------------------
res <- cca(proteomics_data_matrix ~ ., contaminants_data)
plot(res)

xxxx <- summary(res)
aa <- xxxx$concont$importance
aa <- round(aa, 4)

pdat <- res$CCA
samples<-data.frame(sample = row.names(pdat$u),CCA1 = pdat$u[,1],CCA2 = pdat$u[,2])
species<-data.frame(spece = row.names(pdat$v),CCA1 = pdat$v[,1],CCA2 = pdat$v[,2])
envi<-data.frame(en = row.names(pdat$biplot),CCA1 = pdat$biplot[,1],CCA2 = pdat$biplot[,2])

#samples$season <- as.factor(envdat_raw$season)
#samples$foreststructure <- as.factor(envdat_raw$foreststructure)

p <- ggplot() + 
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed")  +
  
  geom_point(data = species, aes(x=CCA1, y=CCA2),size = 0.5, colour = 'pink', pch = 3) +
  #  geom_segment(data = species,aes(x=0, xend= CCA1, y=0, yend= CCA2 ), arrow = arrow(length = unit(0.3, "cm")), colour = 'blue') +
  #  geom_text(data = species,aes(x = CCA1*1.1, y = CCA2*1.1, label = spece), size = 3, colour = 'blue') +
  
  geom_segment(data = envi,aes(x=0, xend= CCA1, y=0, yend= CCA2 ), arrow = arrow(length = unit(0.3, "cm")), colour = 'red') +
  geom_text(data = envi,aes(x = CCA1*1.1, y = CCA2*1.1, label = en), size = 3, colour = 'red', check_overlap = TRUE) +
  
  geom_point(data = samples, aes(x=CCA1, y=CCA2),size = 3)  +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) + 
  
  xlab(paste('CCA1 (', aa[2,1]*100, '%)', sep ='')) + ylab(paste('CCA2 (', aa[2,2]*100, '%)', sep ='')) +
  stat_ellipse(data = samples, aes(x = CCA1, y = CCA2),type ="t", linetype =2)
print(p)

