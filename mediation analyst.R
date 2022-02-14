gc()
rm(list = ls())
library(mediation)
library(dplyr)
setwd('D:/R/mediation')
my_dat <- read.csv('D:/R/WGCNA/temp/all_cor_imconn.csv',row.names = 1)## data from the WGCNA that finished the cor and IMconn matched 
my_vector <- as.vector(as.matrix(my_dat))
my_vector <- my_vector[-which(my_vector == '')]
my_v <- my_vector[ - which(my_vector %in% my_vector[1])]

my_all_data <- read.csv('D:/R/WGCNA/136_alldata.csv')
my_mediation1 <- my_all_data[,which(colnames(my_all_data) %in% my_v)]
my_mediation2 <- my_all_data[,c(c(20:24),c(1479:1483),1485,1488)]
my_mediation3 <- my_all_data[,c(3,4,5,7,8,11,18,19)]
my_mediation3 <- rename(my_mediation3, Abstinence = Abstinence.1)
my_mediation_data <- cbind(my_mediation1,my_mediation2,my_mediation3)

colnames(my_mediation3)

## notice 
## for my_mediation data
## air pollutants    80:86
## proteins     1:74 
## semen parameter  75 :79 

## get every-phenotype's mediation data colname 
conc_col <- which(colnames(my_mediation_data) %in% my_dat$conc)
total_num_col <- which(colnames(my_mediation_data) %in% my_dat$total_num)
pm10_col <- which(colnames(my_mediation_data) %in% my_dat$t_pm10)
so2_col <- which(colnames(my_mediation_data) %in% my_dat$t_so2)
no2_col <- which(colnames(my_mediation_data) %in% my_dat$t_no2)
co_col <- which(colnames(my_mediation_data) %in% my_dat$t_co)
o3_col <- which(colnames(my_mediation_data) %in% my_dat$t_o3)
pm25_col <- which(colnames(my_mediation_data) %in% my_dat$t_pm25)
temp_col <- which(colnames(my_mediation_data) %in% my_dat$t_temp)
all_col <- which(colnames(my_mediation_data) %in% my_v)
## 建立 暴露与中介的方程
## treat emo cong 
## pm10  P38  conc 
##med.fit <- lm(emo ~ treat + age + educ + gender + income, data = bc)
med.fit_6 <- lm(P815 ~ t_pm10 + Age + Education + Marrige + income + smoke + drk + BMI + Abstinence, data = my_med_dat)

## 建立 暴露 中介 结局 的方程
## out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
#data = bc, family = binomial("probit"))

out.fit_6 <- glm(conc ~ t_pm10 + P815 + Age + Education + Marrige + income + smoke + drk + BMI + Abstinence,
               data = my_med_dat, family = gaussian)
## 进行中介效应分析，treat填自变量，mediator填中介变量，robustSE为显示可信区间，sims为重复100次，也可以自己调整
## med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
#robustSE = TRUE, sims = 100)###treat填自变量，mediator填中介变量


med.out_8 <- mediate(med.fit_6, out.fit_6, treat = "t_pm10", mediator = "P815",
                   robustSE = TRUE, sims = 5000)###treat填自变量，mediator填中介变量
#summary(med.out)## ADE 表示直接效应    ACME表示中介效应    
med.analyst_result <- data.frame(
  ADE = med.out$z.avg,
  ADE.P = med.out$z.avg.p,
  ACME = med.out$d.avg,
  ACME.P = med.out$d.avg.p,
  total_effect = med.out$tau.coef,
  total_effect.P = med.out$tau.p
)
summary(med.out_8)


library(dplyr)
## for cycle  ## from baolu 
for (j in 80:86) {
  for (i in 1:length(all_col)) {
    #i = 1
    j = 80
    my_med_dat <- select(my_mediation_data,all_col[i],j,87:94,75:79)
    my_med_data <- rename(my_med_dat,Px = colnames(my_med_dat)[1],Air = colnames(my_med_dat)[2])
    med.fit2 <- lm(Px ~ Air + Age + Education + Marrige + income + smoke + drk + BMI + Abstinence, data = my_med_data)
    out.fit2 <- glm(conc ~ Air + Px + Age + Education + Marrige + income + smoke + drk + BMI + Abstinence,
                    data = my_med_data, family = gaussian)
    med.out2 <- mediate(med.fit2, out.fit2, treat = "Air", mediator = 'Px',
                        robustSE = TRUE, sims = 5000)
    med.analyst_result1[i,] <- data.frame(
      ADE = med.out2$z.avg,
      ADE.P = med.out2$z.avg.p,
      ACME = med.out2$d.avg,
      ACME.P = med.out2$d.avg.p,
      total_effect = med.out2$tau.coef,
      total_effect.P = med.out2$tau.p)
  }
  row.names(med.analyst_result1) <- colnames(my_mediation_data)[all_col]
  
  write.csv(med.analyst_result1, paste("Mediation-", paste(colnames(my_mediation_data)[j],'vs','conc', collapse =""), ".csv", sep=""))
  
}

summary(med.out2)
