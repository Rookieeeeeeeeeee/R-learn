#Lesson1 R包的安装
#1、安装github上的包
library(devtools)
install_github('') 

#----------------------------------------------------------------------
#Lesson2 向量

#一、数值型向量
#1、检查A、B的一致性
identical(A,B)
#2、检查变量类型
class()
#3、seq()生成数据
#从-到-步长-；从-到-共-个数；从-到-按b的形式进行分布
seq(from,to,by)
seq(from = 1, to = 3, by = 0.3)
seq(from,to,len=)#向量长度
seq(from,to,along.with=b)#跟随b的向量模板生成向量
b <- c(1:4)
seq(from=10, to=3,along.with=b)
#4、rep()生成数据
#把a重复n次；把a中每个数重复n次；把a重复到共l次；把a重复d次、b重复e次
rep(a,n)
rep(a,each=)
rep(a,len=)
rep(c(a,b),c(d,e))
rep(c(1,3),times=6)
rep(c(1),times=100)
rep(c(1,3),each=100)
rep(c(1,3),len=9)#向量元素个数限定为9
v1 <- rep(c(1,4,5),times=100)
length(v1)#查看向量长度（元素个数）
#二、逻辑型向量
#TRUE FALSE  TRUE=1 FALSE=0
logit <- rep(c(TRUE,FALSE),times=100)
logit <- c(100==1,200==200) #double"="
#!=  不等于
#>=   =<     &   |"or"
logit <- c(100!=1)
logit <- c(100==1|100>=2)
logit <- c(100==1&100>=2)
#[]提取子集
#1、 which()进行筛选
x <- c(1:100)
index <- x>80
x[index]#[]自动过滤FALSE值
#找到X中大于n的数
which(X>n)
A[which(X>n)]
x <- x[which(x>80)]
index <- x > 80 & x < 90
x[index]
#三、字符串
#1、字母a-z，A-Z
string <- rep(c("nihao","haoma",1,2),times=100)
letters#字母a-z
LETTERS#A-Z

#四、因子 factor  gl  reorder   
my_fac <- factor(x=rep(c(1,4),times=5),levels = c(1,4), labels = c("A","B"))
my_fac2 <- factor(x=1:5,labels = letters[1:5])


x <- rep(c('2021-01-01','2021-02-02'),times = c(99799,99799))

#1、gl()生成因子
#n个水平，每个重复k次，长度共计l个，标签，有序无序
gl(n,k,len=,labels=,ordered=)
my_fac2 <- gl(2,5,labels = c("control","treatment"))
my_fac3 <- gl(2,1,length = 10,labels = c("c","t"))

string1 <- c("a","b","c","d")
my_fac4 <- as.factor(string1)
#2、检查因子的水平
#有几个水平；有哪些水平
nlevels()
nlevels(my_fac4)
levels()
#3、设置哑变量
#把b设为因子A的哑变量，ref仅适合无序因子
relevel(A,ref='b')
my_fac5 <- relevel(my_fac4,ref = "d")#设置d为哑变量
#4、有序分子
factor(X,ordered = T)
#5、设定因子的顺序
#按X顺序进行排序
library(DescTools)
reorder.factor(A,new.order = X)

x <- c("p","5mg","10mg","15mg","25mg")
my_order_fac <- factor(x,ordered = T)
install.packages("DescTools")
library(DescTools)
my_order_fac2 <- reorder.factor(my_order_fac,new.order = x)#按照x的顺序重新排列有序变量

#五、列表
#可以同时容纳多中类型的数据，其他类型则会把数字、逻辑型转为字符型
#1、list()创建列表
my_list <- list(1,2,3,"r","nihao",TRUE,FALSE)#生成list
my_list2 <- list(1:10,letters[1:5])
my_list2[[2]][2]#提取列表2种的第二个元素
my_list3 <- list(1:10,letters[2:10],list(11:15,LETTERS[20:25]))#list 中包含list
my_list3[[3]][[2]][1]
#为每一部分命名
list(a=,b=,c=)
#2、选取列表的内容(是否命名)
list[[2]][1] 
list$b[1]

#六、矩阵
#1、matrix()创建矩阵
my_matrix <- matrix(data = 1:10, nrow = 2, byrow = T)#byrow 根据行排列数据

my_matrix3 <- matrix(1:16,nrow = 4)
mymatrix3[4,4]  #提取坐标为4，4 的数值   前面是行 后面是列 
my_matrix3[4,]  #提取第四行



my_matrix2 <- matrix(data = letters[1:5],nrow = 2,ncol = 5,dimnames = list(c("A","B"),c("v1","v2","v3","v4","v5")))
#dimnames后需要用列表表示，第一个是行名，第二个是列名
matrix(data = ,nrow = ,byrow = ,dimnames=list(c(),c()))


#2、t()转置
t(matrix)
t(my_matrix2)
 
#七、数组

my_array <- array(data=1:16,dim = c(1,8,2))
dim(my_array)#展示数组的维度  分别为 行 列 维度 
dim(my_array) <- c(4,2,2)#修改数组的维度

my_array[3,2,1]  #提取数组的第3行 第二列 第一层 
my_array[,2,1]   #提取数组的第二列第一层数据 

my_array2 <- array(1:16,dim = c(4,2,2),dimnames = list(c(letters[1:4],c(LETTERS[3:4],c(1:2)))))
#1、显示/改变数组维度
dim()
dim(A)<-c()
#2、array()的dimnames参数:行、列、维
dimnames=list(c(),c(),c())

#八、数据框
my_df <- data.frame(name = c("tom","amay","marry"),age = c(24,25,26),height = c(11,12,13))
#1、查看数据、显示行、列
View()  #注意V要大写  
nrow()
ncol()

my_df2 <- data.frame(name = c("a","b","c","d","e"),age = c(11:15),ir = c(21:25))
my_df2 <- data.frame(name = c(letters[1:5]),age = c(11:15),ir = c(21:25))# 两者等价

my_df3 <- data.frame(number = c(1:2017), age = seq(from= 1,to = 100,len=2017),stringsAsFactors = F)  #stringsas factor  防止字符串被修改为因子型变量

str(my_df3)  #查看数据框的格式

#2、删掉行、列
A[,-c()]
A$b<-A$c<-NULL

my_df3[,2] # aq col2 
my_df3[,-2] # delete col 2

#equal to 
my_df3$age <- NULL

#在dataframe 中新增一个变量 
my_df3$f <- seq(from = 2017 ,to = 1, by =-1)

#3、修改某一个数据
A<-edit(A)
fix(A)

edit(my_df3)
my_df4 <- edit(my_df3)  #  将修改值赋值给df4  再查看 df4 即可

fix(my_df3)#一次性修改数据框  

#4、看数据头尾===========================================
head(iris,n=5)#查看头5行
tail(iris,n=6)#查看后6行
head(A,5)
tail(B)

#5、描述数据===============================================
install.packages("psych")
library(psych)
describe()
describe(iris)##获取均值标准差等

#命名数据集的变量名
names(iris) <- c("sepal_l","sepal_w","petal_l","petal_w","species")
#6、支持中文===============================================
Sys.setlocale(locale = 'chinese')
##=========================================================


#7、数据框合并=============================================
my_df5 <- data.frame(one = c(1:6),
                     two = c(letters[1:6]),
                     three = c(T,F,T,F,T,F)
                     )
my_df6 <- data.frame(four = c(11:16),
                     five = c(letters[11:16]),
                     six = c(T,F,T,F,T,F)
)

my_df7 <- cbind(my_df5,my_df6)
my_df7
cbind()##两个数据框的行数不同会报错 =======================

my_df8 <- data.frame(one = c(1:6),
                     tw = c(letters[11:16]),
                     thre = c(T,F,T,F,T,F)
)

my_df9 <- rbind(my_df5,my_df8)

rbind()##列名不同会报错======================================

#根据by进行合并
merge(A,B,by=)
my_df10 <- merge(my_df5,my_df8,by = "one")##merge函数根据相同的列智能合并（提取相同列）

#8、数据框的切分=============================================
A[sample(1:nrow(A),n,replace=),]
#按因子切分：把A按照A中变量b进行切分，形成列表，再进行提取
iris_sub <- iris[sample(1:150,30),]##从iris数据集中随机选择30个样本


#抽样切分：设定种子；从1到A行数中抽取n个，是否放回
set.seed(123456)

##split 函数
iris_sub2 <- split(iris,f = iris$Species) ##根据Species切分
setosa <- as.data.frame(iris_sub2[1])  ##提取出setosa花种的数据，写成数据框

listB<-split(A,f=A$b)
B<-as.data.frame(listB[1])


#精确切分:从A中把b为'abc'且c>n的行、1:2列的内容提取
A[A$b=='abc' & A$c>n, 1:2]
subset(A,A$b=='abc' & A$c>n,select=1:2)

iris_sub3 <- iris[iris$Species=="setosa" & iris$Sepal.Length >= 5,1:2]
##选择iris数据集中花种为setosa 且 花瓣长度大于等于5 且 只要前两列数据 

iris_sub4 <- subset(iris,iris$Species=="setosa" & iris$Sepal.Length >= 5,select = 1:2)

##以上两种数据切分函数效果相同
identical(iris_sub3,iris_sub4)##识别两个数据集是否相同 
#----------------------------------------------------------------------
#Lesson3 条件与循环

#一、条件
#1、if-else语句
#仅if+执行；if执行1 else执行2；if内含多个条件；执行多个语句
if(i>0) print('')
if(i>0) print('') else
  print('')
if(i>0&i<2) print('') else
  print('')
if(x>2) {1;2}else
 {3
  4}

#二、循环
#1、repeat语句
#break代表终止重复
repeat {if (i>10) break else 
{print(i)
  i <- i + 3}
}
#2、while语句
while (i <= 23) {
  print(i) 
  i <- i + 3
}
#3、for语句
for(i in 1:10){print(i)}
#4、append()为向量在后面添加元素
append(a,b)
#5、多重循环
for(i in 1:4){
  for(j in 1:3){mat[i,j] <- 2}
}

#----------------------------------------------------------------------
#Lesson4 函数

#一、自定函数
#function()x、y是参数，y默认为2
function(x, y=2) {x + y}

#二、缺省参数：使用其他函数的参数...
function(x,...) {
  print(x) 
  summary(...)} 

#三、参数数量不限制：把列表的无限性传给函数
function(x,...) {
  l1 <- list(...)
  for (a in l1) x <- x + a 
  x
}

#四、直接在参数位置写下参数的意义，...会继承函数中...
function(x, m = mean(x, ...), s = sd(x, ...), ...) {
  (x - m) / s 
}

#----------------------------------------------------------------------
#Lesson5 数据的读取与写出

#一、数据的读取
#1、读取csv文件：文件完整路径；第一行为变量名；自定义的缺失值；因子化
read.csv(file='.csv', header=T,sep='',na.strings=c(),stringsAsFactors=F)
#2、读取txt文件：
read.table(file = '.txt',header = TRUE)
#3、读取xls文件
library(readxl)
read_excel(path = '.xls')
#4、读取SPSS、SAS、STATA数据
#SPSS：文件完整路径、转为数据框、使用SPSS的标签；STATA
library(foreign)
read.spss(file = '.sav',to.data.frame = T,use.value.labels =T)
read.dta('')
#5、读取文本
#读取普通文本
readLines('.txt')
#读取基因序列
readLines('.fasta')

library(stringi)
stri_read_lines('.fasta')

#二、数据的写出
#1、生成数据
scan()
#2、导出数据(需要写file=,否则会把文件路径当成一个字符串向量)
cat(a, file = '.txt')
#3、写出csv
write.csv(a,file = '.csv')
#4、写出文本
writeLines(a,'.txt')

#----------------------------------------------------------------------
#Lesson6 数据的排序、长宽型转换、因子化

#一、数据的排序
#1、sort()：是否倒序，可用于数字和字符串
sort(x, decreasing=T)
#2、rank()与秩次
rank(x)
#3、order()
#返回顺序的下标：是否倒序；排序；先按A中a正序排序，再按A中b倒序排序
order(x,decreasing=T)
x[order(x)]
A[order(A$a, -A$b),]

#二、数据的长宽型转换
#1、长型数据与宽型数据
#长：变量少而观察值多
#宽：变量多而观察值少
#2、stack()转为长型数据
stack()
#3、reshape()转为长/宽型数据
#需转换的变量名；标识变量；时间变量；转为宽型
reshape(A,v.names="",idvar="",timevar="",direction="wide")
#标识变量；需转换的部分；需转换的变量名；转为长型
reshape(A,idvar="",varying=list(2:n),v.names="",direction="long")
#4、reshape2利用melt和dcast进行转换
library(reshape2)
melt(data=, id.vars = '')
#需要处理的分类(.为占位符)；需要使用的功能；计算的变量
dcast(data=,formula=a~.,fun.aggregate=mean, value.var = '')
dcast(data=,formula=a~b,fun.aggregate=mean, value.var = '')

#三、变量的因子化
#1、公式法
age <- 1 + (age > 30) + (age>=40) + (age>=50)
age <- 1*(age<30) + 2*(age>=30 & age < 40) + 3*(age>40) 
#2、cut():breaks为均分为几份或a至b中分为len部分；包含最小值、最大值
cut(age, breaks = 4, labels = c(),include.lowest = TRUE,right = TRUE)
cut(age, breaks = seq(a,b,len=), labels = c())
#3、ifelse():条件；满足则执行；不满足则执行
ifelse(a>n, 'old', 'young')
ifelse(a>n, 'old', ifelse(a<m, 'young', 'middle'))
#4、recode():重新编码：最小值、最大值
library(car)
recode(var=, recodes = 'lo:29 = 1; 30:39 = 2; 30:hi = 3')

#----------------------------------------------------------------------
#Lesson7 apply函数家族

#一、apply家族
#1、apply():汇总矩阵、数据框的行/列
apply(matrix, 1/2, sum)
#2、lapply():实现遍历，返回列表:适合表示回归这种列表形式
lapply(X, FUN = )
#3、sapply():返回非列表 返回向量
sapply(X, FUN = )
#4、tapply(): 仅使用数据框
tapply(X = , INDEX = , FUN = )
#5、mapply():把自定函数应用于向量(if条件不适合向量，可通过此函数实现)
mapply(fun,x,y)

#apply 家族  数据汇总 ----------------------------------------------------------------------
#LECTURE13:

apply()

lapply()

sapply()

tapply()

mapply() 适用于向量化操作

mat <- matrix(1:24, nrow = 4, ncol = 6)
apply(X = iris[,1:4], MARGIN = 2, FUN = mean)## margin  1 表示行 2 表示列 
## 提取iris数据集中第1：4列 进行列平均

##遍历
lapply(X = c(1:3), FUN = log) #分别对1：3进行log    ##返回列表 



lm_1 <- lapply(iris[,1:3], 
               function(x)lm(x~iris$Petal.Width,
                             data = iris[,1:3]))## 取iris前3列数据 进行Petal.Width与前三列数据的线性回归


sapply(1:3,function(x)
  x+3
)##   计算1：3 的 x+3 结果    返回数据框或者向量 

tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = mean)## 适用于数据框 index是指指示变量 根据species 对数据进行切分 然后求Sepal.Length平均 

tapply(iris[,1:4], iris$Species, mean)

mapply##

myfun <- function(x,y){
  if(x>4) return(y)
  else return(x+y)
}
myfun(1:3,2:4)##if语句不能向量化操作 所以会报错 


mapply(myfun, 1:3, 2:4)  ##使用mapply函数不会报错  


#----------------------------------------------------------------------
#Lesson8 自带数据汇总函数

#一、数据汇总函数
#1、ave():以A中b为分组变量对A中a进行汇总FUN
ave(A$a, A$b,FUN = sd)
#2、by():以A中b为分组变量对A中a进行汇总FUN(可以是自定函数)
by(data = A$a, INDICES = A$b,FUN = mean)
by(A, A$b, function(x){
})

survival <- data.frame(id = 1:10, 
                       cancer = sample(c('lung','liver','colon'), 10, replace = TRUE), 
                       treatment = sample(c('Surg','Chemo'), 10, replace = TRUE), 
                       sur_days = sample(100:1000, 10))

by(data = survival$sur_days, INDICES = survival$cancer,FUN = mean)

data(mtcars)
aggregate(x = mtcars, by = list(VS = mtcars$vs == 1, high = mtcars$mpg>22), mean)
## 对原变量中 vs变量进行重新分类（分类变量），对连续性变量大于22的设置为high

#3、aggregate()  :对A，生成new1和new2去代替原来的old1和old2，执行FUN
aggregate(A, by = list(new1 = A$old1 == 1, new2 = A$old2>5), mean)
#对A进行选择防止字符串报错
aggregate(A[,2:4],by = list(new1 = A$old1 == 1, new2 = A$old2>5), mean)
#可以采用~的形式表示来简化代码
aggregate(.~b, data = , mean)
#4、sweep():行/列；统计量；采取+-*/，默认为-
sweep(x = , 1/2, STATS = 1, FUN = '+')

#----------------------------------------------------------------------
#Lesson9 plyr、dplyr、data.table数据汇总包

#一、plyr包
#1、ply函数族逻辑
#a(array)、d(data.frame)、l(list)
#函数格式:xyply(x代表输入的格式、y代表输出的格式)
library(plyr)
input/output   array   data.frame    list    discarded
array          aaply     adply       alply     a_ply
dataframe      daply      ddply       dlply    d_ply
list           laply      ldply        llply    l_ply 

#eg
my_matrix <- matrix(1:24, nrow = 3, ncol = 8)

aaply(my_matrix, .margins = 2, .fun = mean)  ##margins = 2 表示对操作   返回结果为数组
apply(my_matrix,MARGIN = 2,FUN = mean)    ##返回结果为向量
adply(my_matrix, .margins = 2, .fun = mean)  ## 返回结果为数据框

my_list <- list(1:10,2:8,rep(c(T,F),times = 5))
laply(my_list,.fun = mean)

my_df <- data.frame(name = c('Tony', 'Andy', 'Bob','Mary','Leo'),
                    height = c(172,176,173,167,190),
                    gender = c('M','F','F','M','M'),
                    age = c('old','young','young','young','old'))
ddply(my_df,.variables = .(gender) ,summarize ,mean_h = mean(height))
ddply(.data=my_df,.variables=.(gender), summarise, mean_a=mean(height),sd_a=sd(height))
ddply(.data=my_df, .variables=.(age, gender), .fun = summarize, mean_a=mean(height),sd_height = sd(height)) ## 根据两个分类变量对height 进行分类汇总
#install.packages('reshape2')
library(reshape2)
tips
ddply(tips, .(sex, smoker), function(x) sum(x$tip)/sum(x$total_bill))
ddply(tips, ~sex + smoker, function(x) sum(x$tip)/sum(x$total_bill))

#2、从数组汇总成数组/数据框
aaply(matrix, .margins = 2, .fun = mean)
adply(matrix, .margins = 2, .fun = mean)
#3、从列表汇总成数组
laply(list, .fun = mean)
#4、从数据框汇总成数据框:按b把A分类后汇总
ddply(.data = A, .variables = .(b), summarise, mean_a = mean(a))
#按b把A分类后汇总均数与标准差
ddply(.data=A,.variables=.(b), summarise, mean_a=mean(a),sd_a=sd(a))
#按b、c把A分类后汇总
ddply(.data=A, .variables=.(b, c), .fun = summarize, mean_a=mean(a))
#与tapply()比较
tapply(my_df$height, my_df$gender, mean)
#自定函数的运用(两种写法):按a、b把A分类后运行函数，注意函数中x代表A
ddply(A, .(a, b), function(x) sum(x$c)/sum(x$d))
ddply(A, ~a + b, function(x) sum(x$c)/sum(x$d))


#5、从数据框汇总成列表:适合表示回归这种列表形式
dlply(A, ~a, .fun = )
iris
my_mod1 <- function(x){
  lm(Sepal.Length ~ Sepal.Width,data = x)
}
dlply(iris, .variables = ~Species,my_mod1)
#6、plyr包中常用的其他函数及高级应用
#each()对数据进行多个指标分析；colwise()对指标的列进行分析；
#numcolwise()仅对属于数值变量的列进行分析
each(mean, sd, median)(A$a)
each(mean,sd,median)(iris$Sepal.Length)   ## 对一个变量进行多种统计指标分析
colwise(mean)(iris) ## 对列进行统计汇总
numcolwise(mean)(iris)  ## 仅对属于数值变量的列进行分析
#colwise()与ddply()的联用（三种写法）
ddply(A, ~a, colwise(mean, c('b','c')))
ddply(A, ~a, colwise(mean, .(b, c)))
ddply(A, ~a, colwise(mean, ~b + c))

ddply(iris , ~Species, colwise((mean),c('Sepal.Length','Sepal.Width')))## 根据种类 对长度和宽度求平均
ddply(iris, ~Species, colwise(mean, .(Sepal.Length, Sepal.Width)))
ddply(iris, ~Species, colwise(mean, ~Sepal.Length + Sepal.Width))##推荐这种

#二、dplyr包
library(dplyr)
#1、筛选
#filter()选出A中a为1、b为2的行  针对行 ！！！
filter(A, A$a=='1', A$b=='2')
sub1 <- filter(tips, tips$smoker == 'No',tips$day == 'Sun')
#与subset()对比:可提取满足条件的行并选择需要提取的列
subset(A,A$b=='abc' & A$c>n,select=1:2)
#slice()选择行数为1:3的行  选择A 数据集的1到3行 
slice(A, 1:3)
#select()选择A中的a、b、c列;a:h列;1:8列   针对列！！！
select(A,a,b,c)
select(A,a:h)
select(A,1:8)
sub3 <- select(tips ,sex:day)

#2、排序:把A先按a倒序、再按b正序排列
arrange(A,desc(a),b)
A[order(-A$a,A$b),]
new_tips <- arrange(tips, total_bill,tip) ## 以total——bill 升序
new_tips <- arrange(tips, desc(total_bill),tip) ## 降序

#3、变量重命名
rename(A, new_a = a, new_b = b) ## 新名字等于旧名字 
names(A)[2]<-'new_a'

#4、显示A中a的因子水平
distinct(A,a)
levels(A$a)
distinct(tips,sex)
levels(tips$sex)

#5、生成新变量:基于已有的a和b生成c，且能根据未进入数据框的c生成d
mutate(A, c = a/b, d = c * 100)
mutate(tips,rate = tip/total_bill, aa = rate*100) 

#6、抽样:从A中抽10个样;从A中抽10%的样
sample_n(A, size = 10)
sample_frac(A, 0.1)
sample_n(iris,10)
nrow(sample_n(iris,10))

#7、group_by():把A按a分组，利用分组进行汇总
group = group_by(A, a)
summarise(group, count = n(), mean_b = mean(b), sd_c =sd(c))

group = group_by(tips,smoker)
summarise(group, count = n(), mean_tips = mean(tip), sd_bill = sd(total_bill))

#管道符(%>%):把A放入group_by()，再把生成的group放入summarise()
A%>%group_by(a,b)%>%summarise(count=n(),mean_c=mean(c),sd_d=sd(d))  
result <- tips %>% group_by(smoker,sex) %>% summarise(count = n(), mean_tips = mean(tip), sd_bill = sd(total_bill))

#8、join家族:合并数据框
#a、b中共有的x进行合并
inner_join(a, b, by = 'x') ##以a为准
#以a为准，剔除b没有的x；以a为准，只保留b没有的x
semi_join(a, b , by = 'x') ##以b为准
anti_join(a,b, by = 'x')   ## a中除开b的部分

#以左为准合并，出现右没有的填充NA；以右为准合并，出现左没有的填充NA
left_join(a, b, by = 'x')
right_join(a, b,by = 'x') 

#三、data.table包
#1、data.table与数据框类似，但是可容纳观测数量不同的变量，会自动重复
#install.packages('data.table')
library(data.table)
data.table()
dt <- data.table(v1 = c(1,2), v2 = LETTERS[1:3], v3 = round(rnorm(12,2,2),digits = 2), v4 = sample(1:20, 12))
##自动重复行数不等的变量

#2、随机生成均数为b、标准差为c的a个正态数据
rnorm(a,b,c)
#3、选取子集
#选取行
dt[3:6,]
#筛选v2是'B'/'A'和'B'的所有行(%in%表示选择v2中的A与B)
dt[v2=='B']
dt[v2 %in% c('A','B')]
#选取列
#dt[,1:2]
dt[,list(v1,v2)]
dt[,v3]

#4、函数操作
dt[,sum(v4)]

dt[,list(sum_v4 = sum(v4),mean_v4 =  mean(v4))]

dt[,list(sum_v3 = sum(v3), mean_v4 = mean(v4))]



#生成一个新变量，变量名是sum_v3，计算v3总数
dt[,list(sum_v3 = sum(v3), mean_v4 = mean(v4))]

#生成一个新变量，变量名是v5，为v4+1
dt[,list(v5 = v4 + 1, v6 = v3 -1)]

#同时执行多个语句，用;进行分隔
dt[,{print(v2)
  plot(1:12, v3, col = 'red')}]

#以v2和v1分组，生成多个新变量，执行函数
dt[,list(sum_v3 = sum(v3), mean_v4 =  mean(v4)), by = list(v2,v1)]
#与ddply比较：
ddply(.data=A,.variables=.(b), summarise, mean_a=mean(a),sd_a=sd(a))
#在1至8行中以v2分组，生成多个新变量，执行函数
dt[1:8,list(sum_v3 = sum(v3), mean_v4 =  mean(v4)), by = v2]
#5、.N频数显示:显示v1、v2的频数
dt[,.N,by=list(v1,v2)]
#6、:=在原数据表的基础上增加新变量
dt[, v5 := v4+1]
dt[,c('V5','V6') := list(v3 + 1, v4-1)]
#7、attach()与detach():选择A为环境，取消A为环境
attach(A)
a
detach(A)
#8、选定为dt的v2进行操作
setkey(dt,v2)
#选择v2为A、C的行；选择v2为A、D的行，如果没有则不加入
dt[c('A','C')]
dt[c('A','D'),nomatch = 0]
#汇总v2中A、B的v4求和；分别计算A与B对应的v4求和
dt[c('A','B'),sum(v4)]
dt[c('A','B'),sum(v4),by = .EACHI]
#9、连续选子集
dt[,list(v4_sum = sum(v4)), by = v2][v4_sum > 35]

#----------------------------------------------------------------------
##lesson 10 条件与循环==================================================
##条件if-------------------------------------------------------------------

i <- 1
if (i > 10) print('i is positive') else
  print('i is negative')

i <- -1
if( i > 0 & i < 2) print('i = 1') else
  print('i don\'t know ')


x <- 1 
if(x > 2) 
{
  y = 2 * x 
  z = 3 * y 
} else ##else要跟在大括号之后
{
  y = 2 + x
  z = 2 * y
}
y;z


x <- 10
y <- c(2, 10, 12, 3, 17) 
if (y < x) x else y

##循环---------------------------------------------------------------------

i <- 3
repeat {if (i > 23) break else   ##repeat 不等于rep()函数
{print(i)
  i <- i + 3}
}
##如果i>23  即结束循环   打印所有i  

i <- 3
while (i <= 23) {
  print(i) 
  i <- i + 3
}
##满足i<=23  即进行后面的语句  不满足则结束 


for(i in 1:10){
  print(i)
}
#打印1到10 



set.seed(2017)
x <- sample(10:100, 10)##从10到100中随机选10个数
y <- sample(1:100, 10)## 从1到100中随机选10个数
for(i in 1:10){
  z[i] = x[i]>y[i]
}
z


set.seed(2017)
x <- sample(10:100, 10)
y <- sample(1:100, 10)
z <- NULL###在初试位置给z赋一个空值  即可在最后输出合格的z值 
for(i in 1:10){
  if(x[i] > y [i]){
    z = append(z, x[i])
  }
}
z

mat <- matrix(NA,nrow = 4,ncol = 3)

for(i in 1:4){
  for(j in 1:3){
    mat[i,j] <- 2
  }
}

##自定义函数 function ================================================================
#函数
mean()

##给function 赋予x y 两个参数 
my_fun1 <- function(x, y) {
  x + y
}
my_fun1(1,1)  ##将x y 均赋值为1 

##  在初始时即设定y的大小  
my_fun2 <- function(x, y = 2){
  x + y
}
my_fun2(1)


##缺省参数----------------------------

values <- c(sqrt(1:100))## 求1：100 的平方根 
my_fun3 <- function(x,...) {
  print(x) 
  summary(...)} 

my_fun3("Here is the summary for values:", values, digits=2)
##不知道参数怎么设置可以省略号 


addemup <- function(x,...) {
  args <- list(...)##list函数也可以替换为c函数 
  for (a in args) x <- x + a 
  x
}
addemup(1,2,3,4,3)


normalize <- function(x, m = mean(x, ...), s = sd(x, ...), ...) {
  (x - m) / s 
}

normalize(x==1:100)


install.packages("readxl")##直接读取EXCEL  
library(readxl)
A <- read_excel(path = "~/Desktop/1.xls")
install.packages("XLConnect")##数据的局部读取
library(XLConnect)

#LECTURE13: 数据的排序======================================================== 

x <- sample(1:100, 10)

sort(x, decreasing = TRUE)  ##sort函数默认升序  需要降序需要使用decreasing 

y <- c('python','ruby','java','r')

sort(y, decreasing = TRUE)



rank(x)  #秩次  ## 输出相对位置

z <- c(1,2,3,3,4,4,3,6,6,6,7,2,2)

rank(z)




order(x)  ## 返回元素的下标  最小的元素的下标 依次往后          

x[order(x)]

head(iris[order(iris$Sepal.Length, iris$Sepal.Width),])##根据Sepal.Length Sepal.Width 排序 


##长宽型数据的转换


freshmen <- c(172,120,122,120)
sophomores <- c(122,172,173,172)
juniors <- c(167,172,177,174)
height <- stack(list(fresh=freshmen,sopho=sophomores,jun=juniors))
height

tapply(height$values, height$ind, mean)  ## 对height数据集中根据ind  对values 求均值 

wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time", direction = "wide") ##转换长型数据为宽型数据   
## 操作对象  v.names选择操作的列名conc  idvar选择标识变量类似于id  timevar “time"  转换为宽型变量   

long <- reshape(wide, idvar = "Subject", varying = list(2:12),
                v.names = "concentrtion", direction = "long")
## 标识变量  varing 选择堆栈区域 

##reshape2---------------------------------------------

library(reshape2)
melt()
dcast()

new_iris <- melt(data = iris, id.vars = 'Species') # 不把species放进去融化堆栈


dcast(new_iris, formula = Species~variable, fun.aggregate = mean, value.var = 'value')

## 操作对象数据集 formula = 标识变量 ~ 操作变量  汇总函数   汇总的变量  (操作的是长型变量)

tips

dcast(data = tips, formula = sex~., fun.aggregate = mean, value.var = 'tip')##对性别进行分类汇总  注意~后的.

dcast(data = tips, formula = sex ~ smoker, fun.aggregate = mean, value.var = 'tip')## 根据性别和吸烟 进行分类汇总 形成列联表汇总

install.packages('Rcpp')
library(Rcpp)

rm(list = ls())

##变量的因子化  将连续变量转换为分类变量 -----------------------------------------------------------
age <- sample(20:80, 20)

#1, 公式法

age1 <- 1 + (age > 30) + (age>=40) + (age>=50)  ## 第2类是>30 小于 40  依此类推 
age1_fac <- factor(age1,labels = c('young','middle','m-old','old'))

age2 <- 1*(age<30) + 2*(age>=30 & age < 40) + 3*(age>=40 & age<30) + 4*(age>30) 


#2, cut()-------------------------------------------------------

age3 <- cut(age, breaks = 4, labels = c('young','middle','m-old','old'),
            include.lowest = TRUE,right = TRUE)## 是否包含左右区间

  
age4 <- cut(age, breaks = seq(20,80,length = 4), labels = c('young','middle','old'))
##每20分一段 

#3, ifelse()-------------------------------------------------------


ifelse(age > 30, 'old', 'young')  ## (条件   是的结果   否的结果)

ifelse(age>60, 'old', ifelse(age<30, 'young', 'middle'))

#4, car
library(car)
recode(var = age, recodes = '20:29 = 1; 30:39 = 2; 40:49 = 3; 30:hi = 4')
##分四个年龄段编码 



#Lesson10 缺失值、异常值、重复值============================================

#一、缺失值
NA
#1、忽略缺失值计算
mean(x,na.rm = T)
#2、判断有无缺失值及缺失值数量，去除缺失值
is.na(x)
sum(is.na(x))
x[!is.na(x)]
#3、找到缺失值的个数/位置
sapply(A, function(x)sum(is.na(x)))
sapply(A, function(x)sum(is.na(x)))
#通过describe函数的n发现有无缺失值
library(psych)
describe(A)
#4、有NA时进行回归计算
lm(a~b, data = A, na.action = na.omit)
#5、填补缺失值(以均数)
mean_value <- sapply(A$b,mean, na.rm = TRUE)
for(i in 1:4){A[is.na(A[,i]),i] = mean_value[i]}
#填补缺失值(以不同分类变量对应的均数)
mean_value <- tapply(A$a, list(A$b, A$c),mean,na.rm = TRUE)
for(i in 1:3){
  for(j in 1:2){
    A$a[is.na(A$a) & A$b==rownames(mean_value)[i] & A$c==colnames(mean_value)[j]] = mean_value[i,j]
  }
}
#6、赋缺失值
A[sample(1:nrow(A), 20), "rad"] <- NA
#7、Hmisc包把缺失值按均数/中位数/定值进行补全
library(Hmisc)
im_mean <- impute(BostonHousing$ptratio, mean/median/20)
BostonHousing$ptratio <- NULL
BostonHousing$im_mean <- im_mean
#8、mice包用随机森林方法补全缺失值
library(mice)
md.pattern(BostonHousing) #显示缺失值情况
mice_mod<-mice(BostonHousing[,!names(BostonHousing)%in%'medv'],method='rf')
mice_output <- complete(mice_mod) #将随机森林结果补全
actuals <- original_data$rad[is.na(BostonHousing$rad)]
predicts <- mice_output[is.na(BostonHousing$rad),'rad']
mean(actuals!=predicts) #比较两者差异大小
#9、VIM包对缺失值详细可视化并用高级方法填补
library(VIM)
#aggr函数可视化：非缺失值与缺失值的颜色、显示缺失值比例、根据缺失值多少排序
aggr(A, col = c('red', 'green'), numbers = TRUE, sortVars = TRUE, 
                  labels= names(airquality), cex.axis = 0.7, gap = 3)
#marginplot函数可视化：蓝色代表非缺失值、红色代表缺失值、紫色代表都缺失
marginplot(airquality[1:2])
#用多重回归填补缺失值：左边是含缺失值变量，右不含；自动选择/指定回归方法
regressionImp(Sleep+Gest+Span ~ BodyWgt+BrainWgt,data=sleep,family='auto')


#二、异常值
#outlier <- var_name[var_name > 230]设定异常值判断条件
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name)) 
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- var_name[var_name > 230]
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / tot*100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n")
  response <- readline(prompt="Do you want to remove outliers 
                       and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "\n")
    return(invisible(var_name))
  }
}
#数据框；变量
outlierKD(df, bp)

#三、重复值
unique(x)
duplicated(x)
x[!duplicated(x)]
anyDuplicated(x)
mydata[!(duplicated(mydata$name) & duplicated(mydata$birthday)),]
mydata$test <- paste(mydata$name, mydata$birthday)
newdata <- mydata[!duplicated(mydata$test),]

#----------------------------------------------------------------------
#Lesson11 字符串

#一、基础包功能
#1、显示字符长度
nchar(x)
length(x)
#2、转换大小写
toupper('yixuefang')
tolower('YIXUEFANG')
#3、paste()函数粘贴字符串；strsplit()切分字符串
#sep表示间隔符号，collapse表示组合成一个字符串，用''间隔
paste(stringa,STRINGB,sep = '-')
paste(stringa,STRINGB,collapse = '-')
#paste0实现无间隔粘贴
paste0(stringa, STRINGB,collapse = '-')
#切分数据，切分符号为''
strsplit(stringC, split = '/')
#4、substr()截取字符串，起始/终止位置；可通过赋值直接对原字符串进行改变
substr(stringd, start = 2, stop = 4)
substr(stringd,start = 2,stop = 4) <- 'aaa'
#5、grep()对字符串进行筛选
#查找的结构；查找的向量；是否显示值(为F则显示下标)
grep(pattern = 'FRA|fra', x = seq_names, value = TRUE)
#返回布尔值；忽略大小写
grepl(pattern = 'FRA', x = seq_names, ignore.case = T)
#s或S、0到9、2至4位、\\b(右边界)
grepl(pattern = '[s|S][0-9]{2,4}\\b', seq_names)
seq_names[!grepl(pattern = '[s|S][0-9]{2,4}\\b', seq_names)]
grep('ab\\b',my_string,value = T)
#6、变为数值型变量
as.numeric()
#去掉$，gsub可以去掉一个字符串所有的$，而sub仅能去掉第一个
gsub('\\$',replacement = '',money )
sub('\\$',replacement = '',money)
#检查有无'pp'；同类函数
regexpr('pp',test_string)>0
#可识别英式、美式英语
agrep('a',A)

#二、正则表达式
#1、原义表达式
'p'
#2、转译表达式
'.' #所有字符串
'[0-2]' #有0-2的数字
'^ap' #ap开头
'[^0-2]' #没有0-2的数字
'3{2,}' #3出现了2次及以上
'fo+' #='fo{1,}'
'(fo){1,}' #循环整个(fo)
'fo*' #='fo{0,}'
'^k|^m' #或
'ive\\b' #ive结尾
'\\^' #保义符号
'\\d' #= [0-9]
'\\D' #= [^0-9]
'\\s' #非空
'\\S' #有空
'\\w' #=[a-zA-Z0-9]
'\\W' #=[^a-zA-Z0-9]
'\\b' #匹配边界
'\\B' #匹配非边界
'\\<' #以空白字符开始
'\\>' #以空白字符结束

#三、stringr & stringi包
#1、stringr包
str_c('a','b',sep = '-') #粘贴
str_length() #字符串计数
nchar()
str_sub(yxf, c(1,4,2), c(2,6,11)) #提取内容、开始位置、终止位置
str_sub(yxf, 1,1) <-  'Y' #修改字符串的内容
substr()
str_dup(fruit, 2:4) #把fruit重复2:4次
str_trim(x, side = 'both'/'left'/'right') #去除空格（两边/左/右）
str_extract(phones, "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})")
# {2}表示重复2次
str_replace(fruits, "[aeiou]", "-") #替换第一个aeiou为-
str_replace_all(fruits, "[aeiou]", "-") #替换所有aeiou为-
#2、stringi包
stri_join(1:7, letters[1:7], sep='-')
stri_join(1:7, letters[1:7], collapse='-')
#sep是间隔，collapse是整合成一个字符串
stri_cmp_eq() #相同则TRUE
stri_cmp_neq() #不相同则TURE
stri_cmp_lt() #小于则TRUE
stri_cmp_gt() #大于则TRUE
stri_count(language, fixed = 'R') #出现次数，匹配固定表达式
stri_count(language, regex = '^J') #出现次数，匹配正则表达式
stri_count_boundaries(test, type="word"/"sentence"/"character")
#返回有多少个词、句子和字母。
stri_duplicated(c("a", "b", "a", NA, "a", NA), fromLast=TRUE)
stri_duplicated_any(c("a", "b", "a", NA, "a", NA))
#是否第一次出现，是否从后开始统计;返回0则无重复，否则返回第一个重复的位置
stri_dup(c("abc", "pqrst"), c(4, 2)) #重复abc4次、pqrst2次
stri_detect_fixed(c("sgi R", "REX"), c('i', 'R'),case_insensitive = TRUE)
stri_detect_regex(c("above", "abort", "about", "abnormal", 'abandon'), '^ab')
#查找a中是否有b，固定/正则表达式(忽略大小写)
stri_startswith_fixed(c("abaDc", "aabadc",'ababa'), "ba", from=2)
stri_endswith_fixed(c("abaDc", "aabadc",'ababa'),'ba', to = 3)
#判断是否以ba开始/结尾，从第2个开始；到倒数第3个结束
stri_extract_all(tEmp_text, regex = '[0-9]{2,4}\\b') #提取字符串
stri_extract_all_fixed("abaBAba", "Aba", case_insensitive=TRUE, overlap=TRUE)
#忽略大小写；是否重叠提取
stri_extract_all_boundaries("stringi: THE string processing package 123.42...")
#提取字符串带空格和符号
stri_extract_all_words("stringi: THE string processing package 123.42...")
#只提取单词
stri_isempty(c(',', '', 'abc', '123', '\u0103\u0104',' ')) #判断是否为空字符串
stri_locate_all('I want to learn R to promote my skills', fixed='to')
#找到匹配字符的位置

#----------------------------------------------------------------------
#Lesson12 日期时间

#一、时间与日期数据处理
as.Date('2017-02-16',format = )
#月份 %m 数字；%b 英文简称；%B 英文全称
#年份 %y 两位数年份；%Y 四位数年份
#天 %d
as.Date(100, origin = '2017-02-16') #从origin开始往后100天的日期
ISOdate(1993,3,10,16,14,20)
as.POSIXlt() #生成日期时间
#星期 %a 英文简称；%A 英文全称
strptime() #时间-字符
strftime() #字符-时间
julian(as.Date('2017-02-16'), origin = as.Date('2016-02-19')) #计算日期差值
difftime(as.Date('2017-02-16'),as.Date('2016-02-19'),units = 'weeks')
mean(c(as.Date('2017-02-16'),as.Date('2016-02-19'))) #计算日期均值
seq(as.Date('2016-02-10'), by = '2 weeks', length.out = 10) 
#生成时间序列：开始日期，步长，几次
library(stringi)
stri_datetime_add(as.Date('2017-02-16'), value = 10, units = 'days')
#增加时间：日期的10天后
stri_datetime_create(2014,4,20) #创建时间
stri_datetime_parse(c('2013-02-27','2013-02-29'), 'yyyy-MM-dd', lenient = TRUE)
#解析时间：lenient矫正日期

#二、lubridate包
ymd('020217',truncated = 1) #按照年月日转换为日期变量；补齐几个内容
ymd_hms('2017 02 19 14 23 23') #年月日时分秒
month(x_time, label = T, abbr = F) #提取月份，label代表用英文简写/数字，是否全称
day(x_time)
mday(x_time)
wday(x_time)
yday(x_time)
#返回日、月中日、周中日、年中日； <- 后课更改日期
now() #现在的日期时间
make_date(year = 2010:2016,month = 1:3,day = 1:3) #定义日期
make_datetime() #定义日期+时间
round_date(x_time, unit = 'halfyear') #近似日期
int <- interval(start = ymd('1900,01,01'),end = ymd('1999,12,31'))
time_length(int,unit = 'day')
#计算时间间隔
x + days(10) + hours(12) - minutes(30) #日期时间的加减法
    
#三、时间序列
ts(1:10, frequency = 12, start = c(1999, 1),end = c(2001,4))
#时间序列，产生10个，以月为单位，开始时间，终止时间
plot(ts(cumsum(1+round(rnorm(100),2)), start = c(1934,7), frequency = 12))
#生成时间序列曲线
library(xts)
xts(value, times)
window(myts, start = as.Date('2017-01-10'), end = as.Date('2017-01-13')) <- 1:6
#从时间序列中提取子集； <- 可以直接进行赋值
lag(myts,3) #滞后,几个
diff(myts) #离差值：这一个减上一个的差值，第一个为NA

#四、时间序列分析









#----------------------------------------------------------------------
#Lesson13 统计描述

#一、












#----------------------------------------------------------------------
#Lesson14 统计推断之单变量假设检验

#一、t检验









#二、数据变换








#三、方差分析








#四、卡方检验









#----------------------------------------------------------------------
#Lesson15 统计推断之多变量假设检

#一、回归分析与模型诊断







#二、Logistic回归







#三、生存分析与COX回归







