########################
### 자라섬 Main Code ###
### by Kyung-Hoon    ###
########################

#install.packages(c("party","randomForest","ggplot2","plyr","gcookbook"))
library(party)
library(randomForest)
library(ggplot2)
library(plyr)
library(gcookbook)

#setwd("~/Documents/R")
jarasum <- read.csv("jarasum_data_1.csv", sep=";", header=T)
jarasum[,17] <- as.factor(jarasum[,17])   # train, highway, Is_rainy는 factor형이므로 형변환
jarasum[,18] <- as.factor(jarasum[,18])
jarasum[,19] <- as.factor(jarasum[,19])
jarasum[,30] <- as.factor(jarasum[,30]) 
jarasum[,31] <- as.factor(jarasum[,31])
jarasum[,32] <- as.factor(jarasum[,32])
jarall <- jarasum[,-c(1,3,5)]   # Wave, Year, visit_per_day 제거

# party 나무 모형
jara <- jarall[c(1:11),]
jara_tree <- ctree(X.Visit. ~ ., controls = ctree_control( mincriterion = 0.1, minsplit = 2, minbucket = 3) ,  data=jara)
plot(jara_tree, main="jarasum tree model")

# randdomForest
jara_rf <- randomForest(X.Visit.~.,  data=jara , importance=TRUE, do.trace=5, ntree=100)
plot(jara_rf, log="y")
varImpPlot(jara_rf, main="jarasum randomForest", cex=0.7)


### predict ###
train <- jara[c(1:11),]   
test <- jara[11,]
jara_rf1 <- randomForest(X.Visit.~ ., data=train)  # train 데이터로 rf 생성
jara_pred <- predict(jara_rf1, test)            # test 데이터로 rf 예측
jara_pred                                       # 예측값
test$X.Visit.                                      # 실제값
abs(test$X.Visit.-jara_pred)/test$X.Visit.*100        # 실제값과 예측값의 차이
test1 <- jarall[c(12:33),]
jara_pred1 <- predict(jara_rf1, test1)          
jara_pred1

#####################
### 파생변수 생성 ###
#####################
# 최고 최저 기온 차
jarasum <- jarasum[c(1:11),]
jarasumder <- ddply(jarasum, .(X.Wave.), transform, max_min_temp_dif=X.max_temp.-X.min_temp.)
# 아티스트 유투브 조회수/경력
jarasumder <- ddply(jarasumder, .(X.Wave.), transform, art_view_exp=X.avg_atst_utube_view./X.avg_atst_experience.)
# 가평의 지명도 변화 추이 gt_gapyoung/gt_chunchun
jarasumder <- ddply(jarasumder, .(X.Wave.), transform, gt_gap_chu=X.gt_gapyoung./X.gt_chunchun.)

jarader <- jarasumder[,-c(1,3,5)]   # Wave, Year, visit_per_day 제거

# party 나무 모형
jara_tree_der <- ctree(X.Visit. ~ ., controls = ctree_control( mincriterion = 0.1, minsplit = 2, minbucket = 3) ,  data=jarader)
plot(jara_tree_der, main="jarasum tree model(derived variable)")

# randdomForest
jarader_rf <- randomForest(X.Visit.~.,  data=jarader , importance=TRUE, do.trace=5, ntree=100)

plot(jarader_rf, log="y")
varImpPlot(jarader_rf, main="jarasum randomForest(derived variable)", cex=0.7)

### predict ###
train <- jarader[c(1:10),]   
test <- jarader[11,]
jarader_rf <- randomForest(X.Visit.~ ., data=train)   # train 데이터로 rf 생성
jarader_pred <- predict(jarader_rf, test)          # test 데이터로 rf 예측
jarader_pred                                       # 예측값
test$Visit                                         # 실제값
abs(test$Visit-jarader_pred)/test$Visit*100        # 실제값과 예측값의 차이

### 전 박사님의 feedback

# 예측값이 너무 낮게 나오는 문제를 해결하기 위해서 전년대비 증가 관람객수를 타겟으로 변경해서 돌려본 결과 (이전 데이터 입니다)

# inc_visit :: 전년에서 증가한 고객수를 예측대상 변수 ( Y ) 로 잡고 ... 미리 CSV 파일에서 변환해둠
tail(jara,5)$inc_visit # 값 확인
jara_2015 <- tail(jara,1)

# 2014년을 기준으로 해서 일부 값을 증가시킨 상태로 돌려봄
jara_2015$Wave <- 12
jara_2015$Year <- 2015
jara_2015$GT_Jazz <- 60
jara_2015$CumGT_Jazz <- 216*1.15
jara_2015$GDP <- 1471144 *1.05
jara_2015$avg_atst_utube_view <- 2413752 * 1.2
jara_2015$min_temp <- 12
jara_2015$NTmb_Jazz <- 72 * 1.2
jara_2015$CumNTmb_Jazz <- 208 *1.04
jara_2015$gt_jarasum <- 25
jara_2015$gt_chunchun <- 60
jara_2015$gt_gapyoung <- 34
jara_pred_2015 <- predict(r01, jara_2015 )
jara_pred_2015

# 1437.333 :: 천오백명 정도 전년대비 늘어날 것이라는 예측 = 27만 + 1437.333 = 2015 관객수
# 비교를 위해 최근 3년간의 과거 값에 적용해 봄
jara_pred_p3y <- predict(r01, tail(jara,3) )
9 10 11 
31175.33 25285.33 78.00

# 2014년 경우에도 약간이라도 전년대비 늘어났어야 한다는 것이 모델이 주는 값
# 그러나 실제로는 행사일수가 다시 3일이 되면서 줄어들었음
# 일단, 음수가 나오지는 않아서 엉뚱한 값이 나오지는 않음을 확인



### 각 변수들의 그래프 ###
for(i in 1:35){
  th <- names(jarasumder)[i]
  plot(jarasumder$X.Wave., jarasumder[,i], type="o", cex=1.2, col ="chocolate", ylab=th)
}

# bubble chart 
plot(X.gt_gapyoung. ~ X.GT_Jazz., data=jarasum, pch=9, col="red", xlim=c(0,45), ylim=c(-3,23))
with(jarasum, symbols(X.gt_gapyoung. ~ X.GT_Jazz., circles=X.Visit., inches=0.5, add=T))
# 파생변수 추가한 bubble
plot(X.gt_jarasum. ~ gt_gap_chu, data=jarasumder, pch=9, col="blue", xlim=c(0,0.4), ylim=c(-0.3,2.3))
with(jarasumder, symbols(X.gt_jarasum. ~ gt_gap_chu, circles=X.Visit., inches=0.5, add=T))

### ggplot를 이용한 그래프 ####
ggplot(data=jarasum, aes(x=X.gt_gapyoung., y=X.GT_Jazz.))+geom_point(aes(colour=X.gt_chunchun., size=X.Visit.))
# 파생변수 추가한 
ggplot(data=jarasumder, aes(x=X.gt_jarasum., y=X.GT_Jazz.))+geom_point(aes(colour=gt_gap_chu, size=X.Visit.))

#####################
### 등고선 그래프 ###
### by Jeong-Ho   ###
#####################

## 상관관계 그래프
cor.prob <- function (X, dfr = nrow(X) - 2) {
    R <- cor(X, use="pairwise.complete.obs")
    above <- row(R) < col(R)
    r2 <- R[above]^2
    Fstat <- r2 * dfr/(1 - r2)
    R[above] <- 1 - pf(Fstat, 1, dfr)
    R[row(R) == col(R)] <- NA
    R
}

flattenSquareMatrix <- function(m) {
    if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
    ut <- upper.tri(m)
    data.frame(i = rownames(m)[row(m)[ut]],
               j = rownames(m)[col(m)[ut]],
               cor=t(m)[ut],
               p=m[ut])
}

jara <- read.csv("jarasum_data_1.csv", sep=";", header=T)
jara_1 <- jara[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
cor(jara)
is.numeric(jara)
cor(jara_1)
cor.prob(jara_1)
flattenSquareMatrix(cor.prob(jara_1))

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(jara_1)

##*#등고선,
install.packages("hdrcde")
require(hdrcde)
head(jara_1)
attach(jara_1)
hdr.boxplot.2d(GT_Jazz, Visit, xlab="관심도_Jazz", ylab="총 방문객", prob=c(0.20, 0.40, 0.60, 0.80))
hdr.boxplot.2d(avg_atst_utube_view, Visit, xlab="뮤지션 유명도", ylab="총 방문객", prob=c(0.20, 0.40, 0.60, 0.80))
hdr.boxplot.2d(min_temp, Visit, xlab="최저기온", ylab="총 방문객", prob=c(0.20, 0.40, 0.60, 0.80))

#####################
###   Clustering  ###
### by Chang-yeol ###
#####################

jara <- read.csv("jarasum_data_1.csv", header = T)
str(jara)
artist <- read.csv("artistinfo.csv", header = T)
str(artist)

## 데이터마이닝(Clustering)
#install.packages("cluster")

library(cluster)
jara$rainfall <- NULL
jara$train <- NULL
jara$highway <- NULL
jara$Is_rainy <- NULL
str(jara)

*#clustering,,, 3종류로 분류
## 자라섬 기본 데이터

(kc <- kmeans(jara, 3))
table(jara$visit_per_day, kc$cluster)
plot(jara[c("av_temp", "Visit")], col=kc$cluster) 
points(kc$centers[,c("av_temp", "Visit")], col=1:3, pch=8, cex=2)
## 아티스트 데이터
artist$artist_like_reg_2 <- NULL
artist$artist_like_reg_3 <- NULL
artist$artist_like_reg_4 <- NULL
artist$genre_2 <- NULL
artist$genre_3 <- NULL
artist$genre_4 <- NULL
artist$wave <- NULL
View(artist)
str(artist)
artist1 <- as.matrix(artist)
str(artist1)
(ks <- kmeans(artist1, 3)) # 오류로 결과 미도출
table(artist$artist, ks$cluster)



