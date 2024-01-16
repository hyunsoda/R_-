# R 데이터 분석
# Lecture note 8
# 2022_11_07
setwd("C:/temp100")
face<- read.csv(file="face.csv")

# Y : 민주당 후보의 Margin (민주당 후보가 공화당 후보보다 얼마나 표를 많이 얻었는가)
# Margin은 연속형 변수
# X(predictor) : 예측인자 d.comp (민주당 후보의 점수)
# d.vote=민주당 후보의 득표수 , r.vote= 공화당 후보의 득표수
face$d.share <-face$d.votes/(face$d.votes+face$r.votes) # 민주당 후보의 득표율
face$r.share <-face$r.votes/(face$d.votes+face$r.votes) # 공화당 후보의 득표율
# 두개의 차이가 Margin
face$diff.share<- face$d.share-face$r.share

# 산포도 그리기
plot(diff.share~ d.comp, data=face, pch=16, col=ifelse(face$w.party=="R","red", "blue")) # Y~X

# 상관계수 계산
cor(face$diff.share, face$d.comp) 
 # 0보다 큰 값이 나오며 꽤 큰 값이 나옴(상관계수가 크다)
 # -> 민주당 후보의 사진만 평가한 점수가 높을수록 당선확률(margin)이 커짐
 # 상관계수는 결측치가 있을 시 계산 못 함
# 결측치가 있는 경우 cor : use="pairwise.complete.obs" 명령어 사용

# 선형회귀모형
# Y : diff.share, X : d.comp
fit1 <-lm(diff.share ~ d.comp, data=face)
fit1
# Intercept = 상수항 0.6604가 기울기
summary(fit1)
coef(fit1) #coefficient

# yhat<- -0.312+0.66*face$d.comp
fitted(fit1)

plot(diff.share ~ d.comp, data=face)
abline(fit1)
abline(h=0, lty="dashed")

# 데이터셋 결합하기
pres08 <-read.csv(file="pres08.csv")
pres12 <- read.csv(file="pres12.csv")
names(pres08)
names(pres12)
# 데이터를 옆으로 가져다 붙이기 (merge) 
pres<-merge(pres08, pres12, by="state") # state변수에 맞춰서 붙인다.
# 데이터프레임에 state 변수 이름이 서로 다른 경우
pres<-merge(pres08,pres12, by.x="state", by.y="state.name")
# cbind (merge와 비슷하지만 순서 등을 고려하지 않는다)
pres1<-cbind(pres08, pres12)
pres1
# rbind : 밑으로 합치는 명령어 - 데이터 프레임의 개수가 동일할 때만 작동
pres2 <-rbind(pres08,pres12)

# 선형회귀분석 : Y= 2012년 Obama 득표율
# X= 2008년 Obama 득표율

# 표준화된 득표율을 사용한다. Standarization 어떤 값에서 평균을 빼주고 표준편차로 나눠줌
# Scale : 표준화시킴 (평균을 0으로 표준편차를 1로)
pres$Obama2012.z <-scale(pres$Obama.y)
pres$Obama2008.z <-scale(pres$Obama.x)
summary(pres)

fit1 <-lm(Obama2012.z ~ Obama2008.z, data=pres)
summary(fit1)
# 표준화된 값으로 regression할 때는 상수항을 빼도 값이 동일함
fit2 <-lm(Obama2012.z ~ -1+Obama2008.z, data=pres) # -1은 상수항을 빼라는 뜻
summary(fit2)

plot(Obama2012.z~Obama2008.z, data=pres, ylim=c(-4,4), xlim=c(-4,4))
abline(fit1)
# 추정한 직선 위에 거의 모든 데이터들이 분포하고 있다.
# 2008년 오바마의 선거 결과와 2012년 오바마의 선거결과가 강한 양의 상관관계를 가지고 있다.

# 평균으로의 회귀 현상
q25 <-quantile(pres$Obama2008.z, prob=0.25) # 25%에 위치한 값 찾기 (낮은 득표율)
q75 <-quantile(pres$Obama2008.z, prob=0.75) # 75% 에 위치한 값 찾기(높은 득표율)
q25 # 평균보다 낮은 값
q75

# 2012년 선거에서 2008년 보다 높은 득표한 곳 찾아내기
# 그 중에서 2008년 득표율이 상대적으로 낮았던 곳은 몇 곳인가
mean((pres$Obama2012.z>pres$Obama2008.z)[pres$Obama2008.z<=q25])
# 2012년 선거에서 2008년보다 높은 득표한 곳 중 2008년 득표율이 상대적으로 높았던 곳은 몇 곳인가
mean((pres$Obama2012.z>pres$Obama2008.z)[pres$Obama2008.z>=q75])
# 평균으로의 회귀가 성립하려면 위에 있는 값이 더 커야함

women <-read.csv(file="women.csv")
# reserved가 1인 곳이 여성의원들이 할당된 곳
# female 변수는 의장들이 여자인가
# 여성의원들이 배정된 곳에서는 여성 의장일 가능성이 크다
mean(women$female) # 전체 마을에서 여성의장의 비율
mean(women$female[women$reserved==1]) # reserved가 된 곳에서 여성의장의 비율
mean(women$female[women$reserved==0]) # 배정되지 않은 곳에서 여성의장의 비율
# 여성의원을 배정하지 않는다면 주로 남자가 의장을 한다.

# water의 차이
mean(women$water[women$reserved==1])
# [women$reserved==1]은 여성 정치인이 할당된 곳
mean(women$water[women$reserved==0])

mean(women$irrigation[women$reserved==1])
mean(women$irrigation[women$reserved==0])

# 선형회귀분석
fit3 <-lm(water~reserved, data=women)
summary(fit3)
# water의 차이에서 여자와 남자의 차이가 9정도 되었음
# reserved값이 9정도 됨

fit4 <-lm(irrigation~reserved, data=women)
summary(fit4)






