library(sqldf)
library(plotly)
library(radarchart)
library(tidyr)
library(reshape)
library(caret)
library(data.table)
library(plyr)
cwur <- read.csv('Enter the file path/cwurData.csv',encoding = 'UTF-8')
shanghai <- read.csv('Enter the file path/shanghaiData.csv',encoding = 'UTF-8')
times <- read.csv('Enter the file path/timesData.csv',encoding = 'UTF-8')


cw <- cwur[cwur$year==2015,]
sh <- shanghai[shanghai$year==2015,]
tm <- times[times$year==2015,]

## Not Sure
for(i in 5:12){
  cw[,i]<- 1/cw[,i]
  cw[,i]<- 100* cw[,i]
}


tm$international <- as.numeric(as.character(tm$international))
tm$income <- sub('-','0',tm$income)
tm$income <- as.numeric(as.character(tm$income))
tm$total_score <- as.numeric(as.character(tm$total_score))
tm$num_students <- gsub(',','',tm$num_students)
tm$num_students <- as.numeric(as.character(tm$num_students))
tm$international_students <- as.numeric(as.character(gsub('%','',tm$international_students)))/100
for(i in 1:length(tm$world_rank)){
  tm$female_male_ratio<-as.character(tm$female_male_ratio)
  if(tm$female_male_ratio[i]==''){
    tm$female_male_ratio[i]<-'0:1'
  }else{
    tm$female_male_ratio[i]<-tm$female_male_ratio[i]
  }
}
for(i in 1:length(tm$world_rank)){
  tm$female_male_ratio[i] <- 
    as.numeric(unlist(strsplit(as.character(tm$female_male_ratio[i]),':'))[1])/as.numeric(unlist(strsplit(as.character(tm$female_male_ratio[i]),':'))[2])
}


# Before Regression: EDA
reg_tm <- tm[1:100,4:13]
reg_sh <- sh[1:100,4:10]
reg_cw <- cw[1:100,5:13]
reg_tm$female_male_ratio <- as.numeric(reg_tm$female_male_ratio)

for(i in 1:9){
  reg_cw[,i]<- scale(reg_cw[i])
}

for(i in 1:7){
  reg_sh[,i]<- scale(reg_sh[i])
}

for(i in 1:10){
  reg_tm[,i]<- scale(reg_tm[i])
}


## Feature VS Feature{.tabset}

### Times
featurePlot(x = reg_tm[,c(1:5,7:10)], 
            y = reg_tm[,6], 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(colureg_tmmns = 3))

### ShangHai
featurePlot(x = reg_sh[,2:7], 
            y = reg_sh[,1], 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

### CWUR
featurePlot(x = reg_cw[,1:8], 
            y = reg_cw[,9], 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

## Feature vs Label (Total Score){.tabset}

### Times
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = as.matrix(reg_tm[,c(1:5,7:10)]), 
            y = reg_tm[,6], 
            plot = "scatter",
            ## Add a key at the top
            layout = c(3, 3),
            type = c("p", "smooth"),
            span = .5,
            auto.key = list(columns = 3))

### ShangHai
featurePlot(x = as.matrix(reg_sh[,2:7]), 
            y = reg_sh[,1], 
            plot = "scatter",
            ## Add a key at the top
            layout = c(3, 2),
            type = c("p", "smooth"),
            span = .5,
            auto.key = list(columns = 3))

### CWUR
featurePlot(x = as.matrix(reg_cw[,1:8]), 
            y = reg_cw[,9], 
            plot = "scatter",
            ## Add a key at the top
            layout = c(3, 3),
            type = c("p", "smooth"),
            span = .5,
            auto.key = list(columns = 3))

# Regression Models

## Times{.tabset}

### Regression Summary
model1 <- lm(total_score~.,reg_tm)
print(summary(model1))


### Residual Diagnosis
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
qqnorm(model1$residuals)
plot(model1$fitted.values,model1$residuals,main="Fitted Value vs Residuals",xlab="Fitted Values",ylab="Residuals",col="red")
hist(model1$residuals,col="orange",main = "Hisogram of Residuals")
plot(reg_tm$total_score[1:99],model1$residuals,main="Observations vs Residuals",xlab="Observations",ylab="Residuals",col="blue")
par(mfrow=c(1,1))


## ShangHai{.tabset}

### Regression Summary
model2 <- lm(total_score~.,reg_sh)
print(summary(model2))

### Residual Diagnosis
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
qqnorm(model2$residuals)
plot(model2$fitted.values,model2$residuals,main="Fitted Value vs Residuals",xlab="Fitted Values",ylab="Residuals",col="red")
hist(model2$residuals,col="orange",main = "Hisogram of Residuals")
plot(reg_sh$total_score,model2$residuals,main="Observations vs Residuals",xlab="Observations",ylab="Residuals",col="blue")
par(mfrow=c(1,1))


## CWUR{.tabset}

### Regression Summary
model3 <- lm(score~.,reg_cw)
print(summary(model3))

### Residual Diagnosis
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
qqnorm(model3$residuals)
plot(model3$fitted.values,model3$residuals,main="Fitted Value vs Residuals",xlab="Fitted Values",ylab="Residuals",col="red")
hist(model3$residuals,col="orange",main = "Hisogram of Residuals")
plot(reg_cw$score,model3$residuals,main="Observations vs Residuals",xlab="Observations",ylab="Residuals",col="blue")
par(mfrow=c(1,1))

## A: Comparison of Average Education Quality in Six Countries

### Times
tm_country <- sqldf('SELECT country, avg(teaching) as teaching, avg(international) as international, avg(research) as research, avg(citations) as citations, avg(income) as income from tm group by country')
tm_country
country <- c('Australia', 'China', 'Canada', 'Netherlands','United Kingdom','United States of America')
tm_radar <- tm_country[tm_country$country %in% country,]
tmp <- as.data.frame(t(tm_radar[,2:ncol(tm_radar)]))
colnames(tmp)<-tm_radar[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

### ShangHai
tm_c <- tm [,c('university_name','country')]
sh_new <- join(sh,tm_c)
sh_new<-sh_new[is.na(sh_new$country)!=TRUE,]
sh_new[is.na(sh_new)]<- 0
sh_country <- aggregate(sh_new[5:10], list(sh_new$country), mean)
colnames(sh_country)[1]<-'country'
sh_radar <- sh_country[sh_country$country %in% country,]
tmp_2 <- as.data.frame(t(sh_radar[,2:ncol(sh_radar)]))
colnames(tmp_2)<-sh_radar[,1]
tmp_2<- data.frame(item = colnames(sh_radar)[2:ncol(sh_radar)],tmp_2)
chartJSRadar(scores = tmp_2, maxScale = 60, showToolTipLabel = TRUE)

### CWUR
cw_country <- aggregate(cw[,5:12],list(cw$country), mean)
country2 <- c('Australia', 'China', 'Canada', 'Netherlands','United Kingdom','USA')
cw_country <- cw_country[cw_country$Group.1 %in% country2,]
colnames(cw_country)[1]<- 'country'
tmp_3 <- as.data.frame(t(cw_country[,2:ncol(cw_country)]))
colnames(tmp_3) <- cw_country$country
tmp_3 <- data.frame(item = rownames(tmp_3),tmp_3)
chartJSRadar(scores = tmp_3, maxScale = 3, showToolTipLabel = TRUE)

## B: Top 10 Universities in different countries on different ranks

### Times

#### US
tm_us <- tm[tm$country=='United States of America',]
tm_us<- tm_us[1:20,]
tm_us <- tm_us[,-c(1,3)]
tmp <- as.data.frame(t(tm_us[,2:6]))
colnames(tmp)<-tm_us[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)
#### UK
tm_uk <- tm[tm$country=='United Kingdom',]
tm_uk<- tm_uk[1:20,]
tm_uk <- tm_uk[,-c(1,3)]
tmp <- as.data.frame(t(tm_uk[,2:6]))
colnames(tmp)<-tm_uk[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)
#### CA
tm_ca <- tm[tm$country=='Canada',]
tm_ca<- tm_ca[1:15,]
tm_ca <- tm_ca[,-c(1,3)]
tmp <- as.data.frame(t(tm_ca[,2:6]))
colnames(tmp)<-tm_ca[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)
#### NL
tm_nl <- tm[tm$country=='Netherlands',]
tm_nl <- tm_nl[,-c(1,3)]
tmp <- as.data.frame(t(tm_nl[,2:6]))
colnames(tmp)<-tm_nl[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)
#### CN
tm_cn <- tm[tm$country=='China',]
tm_cn <- tm_cn[,-c(1,3)]
tmp <- as.data.frame(t(tm_cn[,2:6]))
colnames(tmp)<-tm_cn[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)
#### AU
tm_au <- tm[tm$country=='Australia',]
tm_au <- tm_au[,-c(1,3)]
tmp <- as.data.frame(t(tm_au[,2:6]))
colnames(tmp)<-tm_au[,1]
tmp<- data.frame(item = colnames(tm_radar)[2:ncol(tm_radar)],tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)


### Shanghai

#### US
sh_us <- sh_new[sh_new$country=='United States of America',][1:20,]
sh_us<- sh_us[,c(2,5:10)]
tmp <- as.data.frame(t(sh_us[,2:7]))
colnames(tmp)<-sh_us[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

#### UK
sh_uk <- sh_new[sh_new$country=='United Kingdom',][1:20,]
sh_uk<- sh_uk[,c(2,5:10)]
tmp <- as.data.frame(t(sh_uk[,2:7]))
colnames(tmp)<-sh_uk[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

#### CA
sh_ca <- sh_new[sh_new$country=='Canada',]
sh_ca<- sh_ca[,c(2,5:10)]
tmp <- as.data.frame(t(sh_ca[,2:7]))
colnames(tmp)<-sh_ca[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

#### NL
sh_nl <- sh_new[sh_new$country=='Netherlands',]
sh_nl<- sh_nl[,c(2,5:10)]
tmp <- as.data.frame(t(sh_nl[,2:7]))
colnames(tmp)<-sh_nl[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

#### CN
sh_cn <- sh_new[sh_new$country=='China',]
sh_cn<- sh_cn[,c(2,5:10)]
tmp <- as.data.frame(t(sh_cn[,2:7]))
colnames(tmp)<-sh_cn[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)


#### AU
sh_au <- sh_new[sh_new$country=='Australia',]
sh_au<- sh_au[,c(2,5:10)]
tmp <- as.data.frame(t(sh_au[,2:7]))
colnames(tmp)<-sh_au[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

### CWUR 

#### US
cw_us <- cw[cw$country=='USA',][1:20,]
cw_us<- cw_us[,c(2,5:12)]
tmp <- as.data.frame(t(cw_us[,2:ncol(cw_us)]))
colnames(tmp)<-cw_us[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 100, showToolTipLabel = TRUE)

#### UK
cw_uk <- cw[cw$country=='United Kingdom',][1:20,]
cw_uk<- cw_uk[,c(2,5:12)]
tmp <- as.data.frame(t(cw_uk[,2:ncol(cw_uk)]))
colnames(tmp)<-cw_uk[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 50, showToolTipLabel = TRUE)

#### CA
cw_ca <- cw[cw$country=='Canada',]
cw_ca<- cw_ca[,c(2,5:12)]
tmp <- as.data.frame(t(cw_ca[,2:ncol(cw_ca)]))
colnames(tmp)<-cw_ca[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 51, showToolTipLabel = TRUE)

#### NL
cw_nl <- cw[cw$country=='Netherlands',]
cw_nl<- cw_nl[,c(2,5:12)]
tmp <- as.data.frame(t(cw_nl[,2:ncol(cw_nl)]))
colnames(tmp)<-cw_nl[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 3, showToolTipLabel = TRUE)

#### CN
cw_cn <- cw[cw$country=='China',]
cw_cn<- cw_cn[,c(2,5:12)]
tmp <- as.data.frame(t(cw_cn[,2:ncol(cw_cn)]))
colnames(tmp)<-cw_cn[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 5, showToolTipLabel = TRUE)

#### AU
cw_au <- cw[cw$country=='Australia',]
cw_au<- cw_au[,c(2,5:12)]
tmp <- as.data.frame(t(cw_au[,2:ncol(cw_au)]))
colnames(tmp)<-cw_au[,1]
tmp<- data.frame(item = rownames(tmp),tmp)
chartJSRadar(scores = tmp, maxScale = 3, showToolTipLabel = TRUE)

