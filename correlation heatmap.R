install.packages("corrplot")
library(corrplot)
setwd('D:/R')
data_142 <- read.csv("142_result.csv")
data_142 <- data_142[,47:57]
data_142 <- data_142[,-6]
data_142 <- data_142[,-7]
data_142 <- data_142[,-7]
data_142 <- data_142[,-8]

my_cor_data_142 <- as.matrix(data_142)
corr_142 <- cor(my_cor_data_142)
corrplot(corr_142,tl.col = 'black',order = 'hclust')
corrl_142 <- cor.mtest(my_cor_data_142)##caculate p value 

#corrplot(corr,tl.col = 'black',order = 'hclust',p.mat = corrl$p,insig = "blank",title = 'The Correlation Of 6 Air Contaminants And Temp',) ##remove the p value insig

#corrplot(corr,tl.col = 'black',order = 'hclust',p.mat = corrl$p,
         #insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 0.9,pch.col = "black")

##final ----------- hclust 
cor_plot_142 <- corrplot(corr_142,tl.col = 'black',order = 'hclust',p.mat = corrl_142$p,
         insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 0.8,pch.col = "black",method = 'color'
         ,tl.cex = 0.7,
         diag = T,title = 'The Correlation Of 6 Air Contaminants And Temp(142)',mar=c(0, 0, 2, 0),tl.pos="lt",
         col = colorRampPalette(c("blue","white","red"))(50),
         cl.ratio=0.2,
         tl.srt = 45,
         type = "upper"
        )

cor_plot_142 <- corrplot(corr_142,method = "number",
                     type="lower",add=TRUE,tl.cex = 0.7,order = 'hclust',
                     tl.pos = "n",cl.pos = "n",diag=FALSE,tl.col = 'black',
                     number.digits = 3,number.cex = 0.9,number.font = NULL,
                     col = colorRampPalette(c("blue","white","red"))(50)
) 
                     

data_1463 <- read.csv("t_correlation_1463.csv")                     
data_1463 <- data_1463[,-1]  
my_cor_data_1463 <- as.matrix(data_1463)
corr_1463 <- cor(my_cor_data_1463)
corrl_1463 <- cor.mtest(my_cor_data_1463)##caculate p value 
cor_plot_1463 <- corrplot(corr_1463,tl.col = 'black',order = 'hclust',p.mat = corrl_1463$p,
                         insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 0.8,pch.col = "black",method = 'color'
                         ,tl.cex = 0.7,
                         diag = T,title = 'The Correlation Of 6 Air Contaminants And Temp(1463)',mar=c(0, 0, 2, 0),tl.pos="lt",
                         col = colorRampPalette(c("blue","white","red"))(50),
                         cl.ratio=0.2,
                         tl.srt = 45,
                         type = "upper"
)

cor_plot_1463 <- corrplot(corr_1463,method = "number",
                         type="lower",add=TRUE,tl.cex = 0.7,order = 'hclust',
                         tl.pos = "n",cl.pos = "n",diag=FALSE,tl.col = 'black',
                         number.digits = 3,number.cex = 0.9,number.font = NULL,
                         col = colorRampPalette(c("blue","white","red"))(50)
) 
