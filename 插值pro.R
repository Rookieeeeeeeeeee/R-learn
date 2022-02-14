rm(list = ls()); gc() # 清空内存
install.packages('sf')
install.packages('sp')
install.packages('rgdal')
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggfortify)
path1 <- "D:/R/IDW/hubei.shp"
hubeiboundary_sp <- rgdal::readOGR(dsn = path1, stringsAsFactors = FALSE)
x1 <- hubeiboundary_sp@data
xs1 <- data.frame(id = row.names(x1), x1)
hubeiboundary_df <- fortify(hubeiboundary_sp)

hubeiboundary_df <- full_join(hubeiboundary_df, xs1, by = "id")
hubeiboundary_df %<>% filter(is.na(long) == FALSE & is.na(lat) == FALSE)# 去除空行

# 统一坐标系
MyCRS <- CRS("+proj=aea +lat_1=25 +lat_2=50 +lon_0=105")
proj4string(Chinaboundary_sp) <- MyCRS
proj4string(Chinaprovinces_sp) <- MyCRS

# 将省会数据转化为sp对象
provinces_sp <- SpatialPointsDataFrame(coords = cbind(x = provinces$x, y = provinces$y),
                                                                         data = dplyr::select(provinces, name, shortname),
                                                                         proj4string = MyCRS)




##method 2 
install.packages('raster')
install.packages('gstat')
install.packages('maptools')
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggfortify)
library(raster)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(maptools)
setwd('D:/R/IDW')
##read site data-------------------------------------------
my_site_data <- na.omit(read.csv('20180101.csv', header = T))
plot(sort(my_site_data$temp),ylab=" 温度(度)",las=1,xlab='站点')

##read the map data-----------------------------------------
bound <- readOGR('hubei.shp')
plot(bound, col = 'grey')

##设定温度数据的投影为WGS84----------------------------------

#dsp <- SpatialPoints(my_site_data[,2:3], proj4string=CRS("+proj=longlat+datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
dsp <- SpatialPoints(my_site_data[,2:3], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
dsp <- SpatialPointsDataFrame(dsp,my_site_data)

cuts<-c(0,30,50,100,150,300)#设置间距

blues <-colorRampPalette(c("red","blue"))(5)#设置颜色梯度，即渐变色。c("red","blue")(5)代表颜色从黄色渐变到橘色，再渐变到蓝色，再到深蓝色,5则代表长度为5。例子：plot(20:1, bg =blues[rank(5:1)], cex = 2, pch = 22)

pols <-list("sp.polygons",bound, fill ="lightgray")#构建京津冀的SpatialPolygons对象

spplot(dsp,"temp", cuts=cuts, col.regions=blues, sp.layout=pols,pch=20,cex=2)

## 转换shp 文件经纬度也转换成WGS84-------------------------------------
WGS84<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")#设置参考系WGS84

dsp1<-spTransform(dsp,WGS84)#将经纬度转成平面坐标，使用WGS参考系

bound1<-spTransform(bound,WGS84)

#aa <- spplot(dsp1,"temp", cuts=cuts, col.regions=blues, sp.layout=pols,pch=20,cex=2)
#aa

##使用领域多边形进行插值-------------------------------------------------
#install.packages('dismo')
#install.packages('deldir')
library(dismo)
library(deldir)
v <- voronoi(dsp1)
plot(v)

## 将差值范围限定在武汉市湖北省内--------------------------------------------
#install.packages('rgeos')
library(rgeos)
bound2<-aggregate(bound1)#聚合，降低分辨率

v1<-intersect(v,bound2)#将两个图层相交

spplot(v1,"temp", col.regions=rev(get_col_regions()))#绘制多边形图


## shape 转栅格
blank_raster<-raster(nrow=100,ncol=100,extent(bound1))
values(blank_raster)<-1
plot(blank_raster)

## 寻找最佳分辨率
layout(matrix(1:4,ncol=2, byrow=TRUE))
res<-c(20,100,500,1000)
for(r in res){
  
  blank_raster<-raster(nrow=r,ncol=r,extent(bound1))
  
  values(blank_raster)<-1
  
  bound_raster<-rasterize(bound1,blank_raster)
  
  bound_raster[!(is.na(bound_raster))]<-1
  
  plot(bound_raster,main=paste("Res: ",r,"*",r))
  
  plot(bound1,add=T)
  
}
windows()##新开一个窗口显示画图 

## 将结果栅格化 
vr <- rasterize(v1,bound_raster,"temp")

plot(vr)


##使用最邻近点插值
gs<-gstat(formula=temp ~ 1, location=dsp1, nmax = 5, set = list(idp = 0))

nn<-interpolate(bound_raster,gs)

nnmask<-mask(nn,vr)##掩膜提取

plot(nnmask)

## IDW 插值 

gs <- gstat(formula=temp ~ 1, locations=dsp1)

idw <- interpolate(bound_raster, gs, idp = 2)

#aa.idw <- gstat::idw(temp ~ 1, dsp1, blank_raster,idp = 2)
idwmask<-mask(idw,vr)
windows()
plot(idwmask)

# 留一法验证

IDW.out <- vector(length = length(dsp1))

for (i in 1:length(dsp1)) {
  IDW.out[i] <- idw(temp ~ 1, dsp1[-i,], dsp1[i,], idp=2.0)$var1.pred
  
}

#绘制估计值和观测值的差异

OP <- par(pty="s", mar=c(4,3,0,0))

plot(IDW.out ~ dsp1$temp, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     
     col=rgb(0,0,0,0.5))

abline(lm(IDW.out ~ dsp1$temp), col="red", lw=2,lty=2)

abline(0,1)

par(OP)
a <- dsp1$temp
x <- mean(IDW.out)
j=1:563
R_sq <- 1-sum((a[j] - IDW.out[j])^2)/sum((a[j] - x)^2)

#计算均方根误差（RMSE）

sqrt( sum((IDW.out - dsp1$temp)^2) / length(dsp1))
class(dsp1)
## 数据提取
##读取人群数据
my_people <- read.csv('D:/R/IDW/people.csv',header = T,row.names = 1)
##转换经纬度
dsp_people <- SpatialPoints(my_people[,2:3], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
dsp_people <- SpatialPointsDataFrame(dsp_people,my_people)
dsp_people<-spTransform(dsp_people,WGS84)

aaa <- raster::extract(idwmask,my_people)
aaa <- as.data.frame(aaa,row.names = row.names(my_people))
