## write by Rstudio 
## based on R 4.1.1 
## @ "2021-12-24 10:51:30 CST"

## cycle based on the col 
states <- as.data.frame(state.x77)
result1 <- c()  ## construct a var to save result 
for (i in 1:4){
  i=1
  fit <- lm(Murder ~ states[,i] + Frost + Area, data = states)
  result1 <- rbind(result1,
                   c(colnames(states)[i],coef(summary(fit))[2,c(1,2,4)]))
  
  
}
## i <- 1 
result2 <- c()
fit2 <- lm(Murder ~ states[,1] + Frost + Area, data = states)
result2 <- rbind(result2,
                 c(colnames(states)[1],coef(summary(fit2))[2,c(1,2,4)]))


## cycle based on the var-name
vars <- c('Population','Income','Illiteracy','Life Exp')
result3 <- c()
for (i in 1:4){
fit3 <- lm(substitute(Murder ~ x + Frost + Area,list(x = as.name(vars[i]))),
          data = states)   
  result3 <- rbind(result3 ,c(vars[i],coef(summary(fit3))[2,c(1,2,4)]))
  
  
}



## cycle based on colname  use apply 
linear_reg <- function(x){
  coef(summary(lm(Murder ~ x + Frost + Area,data = states)))[2,c(1,2,4)]
}
result4 <- t(apply(states[,c(1:4)],2,linear_reg))


## cycle based on purrr(map)
library(purrr)
linear_re <- function(x){
  coef(summary(lm(Murder ~ x + Frost + Area, data = states)))[2,c(1,2,4)]
}
result5 <- map(states[,c(1:4)],linear_re)
result5 <- t(as.data.frame(result5))


###    logistic regression =====================================================
demo_data2 <- read.csv('D:/R/demo_data_logistic.csv',header  = T,row.names = 1)
logistic_reg <- function(x){
  coef(summary(glm(outcome2 ~ x + Frost + Area,data = demo_data2,family = binomial())))[2,c(1,2,4)]
}
result_logi_1 <- t(apply(demo_data2[,c(1:4)],2,logistic_reg))
### 需要修改的是glm方程中的 outcome2 ~ x + Frost + Area  ，
##frost 和area 可以替换成校正变量 
###当outcome为二分类变量时family选择binomial
###后面的 [2,c(1,2,4)]不用修改
###apply(states[,c(1:4)],2,logistic_reg)中 ，states[,c(1:4)]意思是需要进行回归的是数据的第1至4列 ,logistic_reg 是上面的function，2是指数据表格最终是二维数据表
result_logi_1

#加载ggm包
#install.packages("ggm")
demo_data2 <- read.csv('D:/R/WGCNA/data_136_pcor.csv',header  = T,row.names = 1)
#demo_data <- demo_data2[,c(2,3,4,6,7,10,17,18,c(20:1434))]
demo_data <- rename(demo_data2,Abstinence = Abstinence.1)
library(ggm)
##偏相关系数的计算
#函数调用格式：pcor(u,s)
#其中u是一个数值向量，前两个数值表示要计算相关系数的变量下标，其余数值为条件变量的下标
jsbl <- c(9:1423)   #要计算的相关系数的变量下标  83:87
tjbl <- c(1:8)  #条件(控制)变量的下标，即要排除影响的变量的下标
u <- c(jsbl,tjbl)
s <- cov(demo_data)  #变量的协方差（输出序号1）
r <- pcor(u,s)  #偏相关系数（输出序号2）

merge_data <- result_pcor_test
  for (j in 9:82) {
    i = 83
    j = 9
    jsbl <- c(i,j)
    tjbl <- c(1:8)
    u <- c(jsbl,tjbl)
    s <- cov(demo_data)  #变量的协方差（输出序号1）
    r <- pcor(u,s)  #偏相关系数（输出序号2）
    q <- length(tjbl)   #计算要控制的变量数
    n <- dim(demo_data)[1]  #计算样本量
    pcor_test <- pcor.test(r,q,n) #偏相关系数显著性检验结果（输出序号3）
    cor <- pcor_test[[1]]
    pvalue <- pcor_test[[3]]
    result_pcor <- data.frame(
      cor <- cor,
      p <- pvalue
    )
    #result_pcor_test <- result_pcor
    result_pcor_final <- rbind(merge_data,result_pcor)
  }



#加载psych包
install.packages("psych")
library(psych)

##偏相关系数显著性检验
#函数调用格式：pcor.test(r,q,n)
#其中r是由pcor()函数计算得到的偏相关系数，q为要控制的变量数(以数值表示位置)，n为样本大小
q <- length(tjbl)   #计算要控制的变量数
n <- dim(pcordata)[1]  #计算样本量
pcor_test <- pcor.test(r,q,n) #偏相关系数显著性检验结果（输出序号3）


