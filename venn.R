install.packages('VennDiagram')
library(VennDiagram)
my_data <- read.csv('D:/R/WGCNA/final/veen_plot.csv')
venn_list <- list(temp = my_data$temp, 
                  total_mor = my_data$total_mor, 
                  total_num = my_data$total_num, 
                  volume = my_data$volume,
                  conc = my_data$conc)
                  #p_mor = my_data$p_mor)
venn.diagram(venn_list, filename = 'D:/R/WGCNA/final/venn_temp.png', imagetype = 'png', 
             fill = c('red', 'blue', 'green', 'orange','yellow'), alpha = 0.50, 
             cat.col = c('red', 'blue', 'green', 'orange','yellow'), cat.cex = 1.5, cat.fontfamily = 'serif',
             col = c('red', 'blue', 'green', 'orange','yellow'), cex = 1.5, fontfamily = 'serif')

# extract the intersections
inter <- get.venn.partitions(venn_list)
for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
write.csv(inter[-c(6, 7)], 'D:/R/WGCNA/final/venn_temp_inter.csv', row.names = FALSE, quote = FALSE)


interactions <- intersect(my_data$temp,my_data$total_num)
a <- intersect(interactions,my_data$conc)
aaa <- intersect(my_data$temp,my_data$conc)
