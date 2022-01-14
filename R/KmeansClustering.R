### 자치구 k-means clustering

install.packages("readxl")
library(readxl)
#setwd("D://r1")
mydata=as.data.frame(read_excel("./data/clustering_dataset.xlsx"))
mydata
View(mydata)

# 인덱스 바꾸기
rownames(mydata) <- mydata$자치구
dim(mydata)
mydata


# 클러스터링 Package
install.packages("factoextra")
library(factoextra)


############## --------------------------
# 표준화 변환 (standardization)
mydata_s <- transform(mydata, 
                      V1_s = scale(장애인_거주현황),
                      V2_s = scale(콜택시_평균대기시간), 
                      V3_s = scale(출발지목적지))
mydata_s

# variable selection
mydata_v <- mydata_s[,c("자치구", "V1_s", "V2_s", "V3_s")]

# Correlation analysis 상관분석
cor(mydata_v[,-1])

round(cor(mydata_v[,-1]), digits=3) # 반올림

# Scatter plot matrix 변수 간의 산점도 행렬
plot(mydata_v[,-1])


# 주성분분석 PCA(Principal Component Analysis)
k_prcomp <- prcomp(mydata_v[,c(2:4)]) # 첫번째 변수 자치구는 빼고 분석
summary(k_prcomp)
### 제1요인이 표준편차가 가장 크고, 제2, 제3... 순서
### PC2까지 누적 80% 이상 -> 주성분 : PC1, PC2

# Rotation 후의 고유벡터(eigenvector)의 계수
print(k_prcomp)

# 주성분 분석 Scree Plot
plot(prcomp(mydata_v[,c(2:4)]), type="l", sub = "Scree Plot")
View(mydata_v)

# Biplot
biplot(prcomp(mydata_v[,c(2:4)]), cex = c(0.7, 0.8))

# 관측치별 주성분1, 주성분2 점수 계산(PC1 score, PC2 score)
k_pc1 <- predict(k_prcomp)[,1]
k_pc2 <- predict(k_prcomp)[,2]


# k_pc1, k_pc2을 열결합 
pc12 <- cbind(k_pc1, k_pc2)
View(pc12)

## 파이썬으로 elbow method해서 k=4로 결정하였습니다. (파이썬 k-means clustering 소스코드 참고) 

## K-means clustering
set.seed(1004)
kmeans_k4 <- kmeans(pc12, centers = 4)
names(kmeans_k4)

## center points per variables
kmeans_k4$centers

## add 'cluster' to the original dataset
mydata_v$cluster <- kmeans_k4$cluster
head(mydata_v)

fviz_cluster(kmeans_k4, mydata_v[,c(2:5)], ellipse.type = "norm")+theme_minimal()
View(mydata_v)

######## 클러스터링 결과 해석 ------------------------

##<고유벡터 해석>
#PC1 : V2 → 콜택시 평균대기시간
#PC2 : V1, V3 → 장애인거주수, 출발지목적지

##<클러스터링 해석>
#군집1 : PC1 높고, PC2 높다.  
#군집2 : PC1 낮고, PC2 낮다.
#군집3 : PC1 낮고, PC2 높다.
#군집4 : PC1 높고, PC2 낮다.

##<클러스터링 결론>
#군집1 ⇒ 콜택시 평균대기시간 많고, // 장애인거주수, 출발지목적지 많은 자치구
#군집2 ⇒ 콜택시 평균대기시간 적고, // 장애인거주수, 출발지목적지 적은 자치구
#군집3 ⇒ 콜택시 평균대기시간 적고, // 장애인거주수, 출발지목적지 많은 자치구
#군집4 ⇒ 콜택시 평균대기시간 많고, // 장애인거주수, 출발지목적지 적은 자치구
