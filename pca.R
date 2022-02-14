states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

##
pr.out=prcomp(USArrests, scale=TRUE)
##pr.out$center 数据的原始均值
##pr.out$scale  数据的标准差
##pr.out$rotation 
## pr.out$x 变量的pca的权重
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)

##画图
biplot(pr.out, scale=0)
summary(pr.out)##显示主成分能解释的方差

##利用ggplot画pca图
install.packages('ggforce')
library(ggplot2)
library(ggforce)

plotdat <- as.data.frame(pr.out$x[,1:2])  ##获取pc1 pc2 的权重数据
plotdat$state <- rownames(plotdat) ## 加上名字

rotdat <- as.data.frame(pr.out$rotation[,1:2])##获取pc1 pc2 的载荷数据
rotdat$crime <- rownames(rotdat)

ggplot() + geom_text(data = plotdat, aes(x = PC1, y = PC2, label = state),size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./4)) + scale_x_continuous(sec.axis = sec_axis(~./4)) +
  geom_segment(data = rotdat,aes(x=0, xend= PC1*4, y=0, yend= PC2*4), arrow = arrow(length = unit(0.03, "npc")), colour = 'red') +
  geom_text(data = rotdat,aes(x = PC1*4.4, y = PC2*4.4, label = crime), size = 4, colour = 'red')
##  





##导入数据 数据航名为样本  列名为基因
setwd('D:/R/pca')
my_data <- read.csv('semen_var.csv',header = T ,row.names = 1)
dim(my_data)
my_pca_data <- my_data[,-1]
pcdat <- prcomp(my_pca_data , scale = T) 
summary(pcdat)
biplot(pcdat,scale = 0)

plotdat <- as.data.frame(pcdat$x[,1:2])
plotdat$class <- as.factor(my_data[,1])
class(plotdat$class)

ggplot(plotdat,aes(x = PC1 , y = PC2 ,colour = class, group = class))+ 
  geom_point(size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  stat_ellipse(type ="t", linetype = 2) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") 
  

## ggfortifiy
install.packages('ggfortify')
library(ggfortify)
library(magrittr)
pca1 <- my_data %>% prcomp()
autoplot(pca1,data = my_data,col = 'class',size = 2,
         loading = T,loading.label = T,
         frame = T, frame.type = 'norm',
         label = T,label.size = 3)+
  theme_classic()

my_data$group <- as.factor(my_data$group)
class(my_data$group)


## 多边形
install.packages('ggord')
library(ggord)
ggplot(plotdat,aes(x = PC1 , y = PC2 ,colour = class, group = class))+ 
  geom_point(size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed")

a <- ggord(pcdat, plotdat$class,coord_fix=F,
      cols = c(), size=2,alpha=0.7,
      arrow=0, vec_ext =0,txt=NULL,ellipse = FALSE, hull = TRUE)
a






















pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
summary(pr.out)

plotdat <- as.data.frame(pr.out$x[,1:2])
plotdat$state <- rownames(plotdat)
rotdat <- as.data.frame(pr.out$rotation[,1:2])
rotdat$crime <- rownames(rotdat)
library(ggplot2)
library(ggforce)
ggplot() + geom_text(data = plotdat, aes(x = PC1, y = PC2, label = state),size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./4)) + scale_x_continuous(sec.axis = sec_axis(~./4)) +
  geom_segment(data = rotdat,aes(x=0, xend= PC1*4, y=0, yend= PC2*4), arrow = arrow(length = unit(0.03, "npc")), colour = 'red') +
  geom_text(data = rotdat,aes(x = PC1*4.4, y = PC2*4.4, label = crime), size = 4, colour = 'red')


dat <- read.table('trans.txt', header = TRUE, row.names = 1, sep = '\t')
dat <- t(dat)
pcadat <- prcomp(dat)
biplot(pcadat, scale = 0)

plotdat <- as.data.frame(pcadat$x[,1:2])
rownames(plotdat)
plotdat$genotype <- rep(c(rep('rht12',6),rep('Rht12',6)),2)
plotdat$treatment <- rep(c(rep('H2O', 3), rep('GA',3)),4)

library(ggplot2)
library(ggforce)
ggplot(plotdat, aes(x = PC1, y = PC2, colour = genotype, shape = treatment, group = genotype)) + geom_point(size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  stat_ellipse(type ="t", linetype =2) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") 