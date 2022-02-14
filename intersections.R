setwd('D:/R/WGCNA/final')
my_df <- read.csv('veen_plot.csv')
my_df <- my_df[,1:12]

write.csv(my_pm10,'pm10_veen.csv')
co_conc <- intersect(my_df$co,my_df$conc)
co_total_mor <- intersect(my_df$conc,my_df$total_mor)
co_p_mor <- intersect(my_df$co,my_df$p_mor)







library(dplyr)
library(stringr)


library(plyr)
result <- c(1:1000)

for(i in 1:12){
  for(j in 1:12){
    #i=1
    #j=2
    names = colnames(my_df)
    name1 = names[i]
    name2 = names[j]
    name1_2 = paste(name1,name2,sep = '|')
    intersections = intersect(my_df[,i],my_df[,j])
    x = c(intersections,rep(c(1:1),times = 1000-length(intersections)))
    a = as.data.frame(x)
    names(a) <- c(name1_2)
                      result = cbind(a,result)
    
    
    }
}
write.csv(result,'intersections.csv')

