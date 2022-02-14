install.packages('lme4') # 安装lme4包
library(lme4) #载入包
politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
#politeness = read.csv(file.choose( )) #手动选择读取数据
# 检查数据politeness的frequency一列是否有NA值。
which(is.na(politeness$frequency))
# 检查数据politeness是否有NA值。
which(!complete.cases(politeness))

par(mfrow=c(2,2))

boxplot(frequency~subject, politeness, main='区域')

boxplot(frequency~scenario, politeness, main='情景')

boxplot(frequency~attitude, politeness, main='态度')

boxplot(frequency~gender, politeness, main="性别")
# 线性混合效应模型LME分析
## 固定效应：attitude 和 gender
## 随机效应：1. subject 和 scenario的截距（intercepts for subjects and items）
## 这里截距用1表示。
## 其实，应该就是，不同subject 和 item基线的不同 作为随机效应。
## 随机效应：2. by-subject and by-item random slopes for the effect of politeness
## 随机效应2的意思是，针对politeness效应的按subject和按item的随机斜率。
## 其实，应该就是，不同的subject和item对politeness影响的随机效应。
politeness.fullmodel = lmer(frequency ~ attitude + gender + 
                              (1 + attitude|subject) + (1 + attitude|scenario), 
                            data=politeness, 
                            REML=FALSE) 
# REML=FALSE，是为了后面使用似然比检验比较模型。
# 输出模型的结果
summary(politeness.fullmodel)
# 建立没有attitude的模型。
politeness.nullmodel = lmer(frequency ~ gender +
                              (1+attitude|subject) + (1+attitude|scenario), 
                            data=politeness, 
                            REML=FALSE)
summary(politeness.nullmodel)
# 比较有attitude的fullmodel和没有attitude的nullmodel
# 如果结果差异显著，则代表attitude对结果有显著影响。
# 这里的比较方法是：似然比检验 Likelihood Ratio Test
anova(politeness.nullmodel, politeness.fullmodel)

# 如果要检验交互作用。则需要以下代码，比较以下两个模型：
# 就是把+变成*，*表示交互。
politeness.fullmodel = lmer(frequency ~ attitude * gender + 
                              (1 + attitude|subject) + (1 + attitude|scenario), 
                            data=politeness, 
                            REML=FALSE) 
politeness.nullmodel = lmer(frequency ~ attitude + gender + 
                              (1 + attitude|subject) + (1 + attitude|scenario), 
                            data=politeness, 
                            REML=FALSE) 
anova(politeness.nullmodel, politeness.fullmodel)
