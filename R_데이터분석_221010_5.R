################
# R 데이터 분석
# Lecture Note 5 and 6
# 2022-10-10
setwd("C:/temp100")
afghan <- read.csv(file="afghan.csv")
summary(afghan)
summary(afghan$age)
summary(afghan$educ.years)

table(afghan$violent.exp.ISAF)
table(afghan$violent.exp.taliban)
# two-way table
tab1<-table(afghan$violent.exp.ISAF, afghan$violent.exp.taliban)
prop.table(tab1)
# 소수점 둘쨋자리까지 반올림
round(prop.table(tab1),digits=2)

head(afghan$income, n=10)
# is.na -> na가 결측치이냐 아니냐 
# 결측치이면 T, 아니면 F
is.na(afghan$income)
mean(is.na(afghan$income)) # T는 1 F는 0   5.6퍼센트의 결측치
sum(is.na(afghan$income)) # 결측치 수

x<- c(1,2,3,NA) #결측치때문에 계산할 수 없음
mean(x)
min(x)
max(x)
median(x)
# 결측치를 빼고 계산하라
mean(x, na.rm=T) # rm은 remove

# table함수는 결측치를 무시한다
table(afghan$violent.exp.ISAF)
table(afghan$violent.exp.taliban)
# 결측치가 몇 개 있는지 알고 싶다면
sum(is.na(afghan$violent.exp.ISAF))
# 결측치가 포함된 table 만들기
table(afghan$violent.exp.ISAF, exclude=NULL)
table(afghan$violent.exp.ISAF, afghan$violent.exp.taliban, exclude=NULL)
# 둘 다 응답하지 않은 비율
10/2754
# 한 번이라도 무응답이 있는 사람 아예 list에서 제외시키기
# listwise deletion, casewise deletion
afghan.sub<-na.omit(afghan) #afghan변수에서 한 번이라도 na가 있으면 제외하기
# 응답을 모두 한 사람들만 남겨라(row) comma뒤에 아무것도 안 씀=모든 변수
afghan.sub1 <- afghan[complete.cases(afghan),] # na.omit과 같음

# complete.cases가 장점이 더 많다.
# 변수를 안에서 설정할 수 있음
# EX. afghan.sub1 <- afghan[complete.cases(afghan$income),]
# -> income에 결측치가 있으면 삭제하고 이외의 결측치는 그대로 둬라 
nrow(afghan.sub) # 관측치가 몇 개인지 알려줌
length(afghan.sub) # Data frame에서는 변수(column)의 개수
income1 <- na.omit(afghan$income) # income이라는 변수에서 결측치가 있는 것을 제외하라
length(income1) # 2600개로 서로 다르다.

# 범주형 변수의 분포를 이해한다
# barplot 막대그리기
# table을 먼저 만들어야함
tab2 <- table(afghan$violent.exp.ISAF, exclude=NULL)
tab2
barplot(prop.table(tab2))# tab2의 비율을 barplot으로 그리겠다.
barplot(prop.table(tab2), names.arg = c("No vio", "vio", "NA"))

tab3 <- table(afghan$violent.exp.taliban, exclude=NULL)
tab3
barplot(prop.table(tab3), names.arg = c("No vio", "vio", "NA"))

# 연속형 변수의 분포를 파악하고자 한다.
# histogram 시각화를 사용한다.
# educ.years
summary(afghan$educ.years)
hist(afghan$educ.years)
# frequency F -> freq=F 는 density를 알려준다
# Density(확률밀도)
hist(afghan$age, freq=F)
# 다만 density는 정확히 비율과 같지는 않다. 그러나 비율과 유사하다

# educ.year 변수에 대한 히스토그램
# 5 : the number of bins (범주의 개수)
hist(afghan$educ.years, breaks=5) # breaks=5는 개수가 5가 되는 것 
# breaks 옵션에서는 자신이 직접 bin의 범위를 지정할 수 있다.
hist(afghan$educ.years, breaks=c(0,5,10,15,20)) #0~5,5~10,10~15 # 데이터 안에 모든 값이 들어가는 범위를 지정해야함 

# Breaks를 이용하지 않으면 Sturges' Rule을 이용해서 범주의 갯수를 정한다. log2(n)+1
log2(2754)+1

# abline 수직선, 수평선, 기울기가 있는 직선
hist(afghan$educ.years)
abline(v=5, col="red") # 5라는 점에서 vertical line(수직선)을 그려라 컬러는 빨강
abline(h=0.01, col="red") #0.01에 수평선을 그려라

# boxplot
boxplot(afghan$age)
# IQR Interquartile Range :75분위값 -25분위값

# province(시도)
table(afghan$province)
boxplot(afghan$age ~ afghan$province)

# educ.year 변수
boxplot(educ.years ~ province, data=afghan)

# Helmand, Uruzgan이 피해를 많이 받았는지?
# 각 시도별로 ISAF, taliban의 피해변수의 평균을 구하라
tapply(afghan$violent.exp.ISAF, afghan$province,FUN=mean, na.rm=T)
tapply(afghan$violent.exp.taliban, afghan$province,FUN=mean, na.rm=T)

# pdf로 그래프 인쇄, 저장하기
pdf(file="educ1.pdf", height=5, width=5) # pdf파일 열기 , 높이와 폭을 5inch로 맞추겠다.
boxplot(educ.years ~ province, data=afghan) #그래프 그리기
dev.off() # pdf 닫기

# 하나의 pdf 파일에 여러 개의 plot을 동시에 그리고자 하는 경우
pdf(file="educ4.pdf", height=5,width=5,)
par(mfrow=c(1,2))# 그래프를 옆으로 두 개 놓겠다.
boxplot(educ.years ~ province, data=afghan)
boxplot(educ.years ~ violent.exp.ISAF, data=afghan)
dev.off()


congress <-read.csv(file="congress.csv")
# dwnom1 : 경제적 보수/진보 +보수적 -진보적
# dwnom2 : 인종적 보수/진보 +보수적 -진보적
rep <-subset(congress, subset=(party=="Republican"))
dem <-subset(congress, subset=(party=="Democrat"))

rep80 <- subset(rep, subset=(congress==80)) # 80대 회기
rep112<-subset(rep,subset=(congress==112)) # 112대 회기
dem80 <- subset(dem, subset=(congress==80)) # 80대 회기
dem112<-subset(dem,subset=(congress==112)) # 112대 회기

#scatter plot: Spatial Vote
#80대 하원
plot(rep80$dwnom2~rep80$dwnom1)
plot(dem80$dwnom2~dem80$dwnom1)

plot(dwnom2 ~ dwnom1, data=rep80, col="blue", xlim=c(-1,1)) 
# plot을 먼저 그리고 points가 그려져서 xlimit을 넓히지 않으면 옆이 잘린다.
points(dwnom2~dwnom1, data=dem80, col="red") # 하나의 표 안에 넣으려면 두 번째는 plot이 아닌 points

# 112대 하원
# scatter point을 겹치게 할 때는 points를 쓴다.
plot(dwnom2 ~ dwnom1, data=rep112, col="blue", xlim=c(-1,1)) 
points(dwnom2~dwnom1, data=dem112, col="red") 

# 최근 시점으로 올수록 경제적 진보/보수 성향이 양극화되었다는 것을 확실히 알 수 있다.
# 그러나 인종적 진보/보수는 오히려 서로 유사해졌다는 것을 알 수 있다.

# 80~112대 회기까지 
# dwnom1(economic) : 시계열 라인그래프를 그린다.
# 각 정당에 대해서 라인그래프를 그린다
# dwnom2(race)에 대해서도 같은 그래프를 작성한다.
# 이 그래프를 통해서 연구질문에 답할 수 있다.

# 전반적 이념성향 : 평균 또는 중앙값
# median을 계산하다.
# 각 회기별 중앙값을 계산해야 한다.
dem.median_econ <-tapply(dem$dwnom1, dem$congress, FUN=median)
dem.median_econ
rep.median_econ <-tapply(rep$dwnom1, rep$congress, FUN=median)
rep.median_econ
# plot 그리기
plot(dem.median_econ ~ names(dem.median_econ), type="l", col="red")
plot(rep.median_econ ~ names(rep.median_econ), type="l", col="blue")

# line plot을 겹치게 할 때는 lines을 쓴다.
plot(dem.median_econ ~ names(dem.median_econ), type="l", col="red", ylim=c(-0.7,0.7))
lines(rep.median_econ ~ names(rep.median_econ), type="l", col="blue")

# race conservatism
dem.median_race <-tapply(dem$dwnom2, dem$congress, FUN=median)
dem.median_race
rep.median_race <-tapply(rep$dwnom2, rep$congress, FUN=median)
rep.median_race


plot(dem.median_race ~ names(dem.median_race), type="l", col="red", ylim=c(-0.7,0.7))
lines(rep.median_race ~ names(rep.median_race), type="l", col="blue")
# 정치적 양극화가 줄어들었다.