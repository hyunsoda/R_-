# R 데이터 분석
# Lecture Note 6
# 2022-10-17
setwd("C:/temp100")
gini <-read.csv(file="USgini.csv")
# 시계열 데이터 
# 시간의 흐름에 따른 데이터
# x 축: 시간의 흐름 / Y축 : 시간의 흐름 따른 변수의 변화

# plot : 가장 간단한 선그래프 그리는 명령어
plot(gini$gini ~ gini$year, type="l") #(y~x) l=line


congress <-read.csv(file="congress.csv")
# dwnom1 : 경제적 보수/진보 +보수적 -진보적
# dwnom2 : 인종적 보수/진보 +보수적 -진보적
rep <-subset(congress, subset=(party=="Republican"))
dem <-subset(congress, subset=(party=="Democrat"))
dem.median_econ <-tapply(dem$dwnom1, dem$congress, FUN=median)
dem.median_econ
rep.median_econ <-tapply(rep$dwnom1, rep$congress, FUN=median)
rep.median_econ
polar_econ <-rep.median_econ-dem.median_econ
polar_econ
# 값이 점점 커짐 ( 양극화가 되어간다)
plot(polar_econ ~ names(polar_econ), type="l") # y 변수~ x 변수
# 두 격차가 증가 (연도 대신 회기)

# 표준화 시키기
# (x-x평균)/sd
# 표준화된 값은 -4 ~ 4 사이에 나타난다.
# 표준화 함수 : scale
polar_econ_z<- scale(polar_econ) #scale : 어떤 변수를 표준화시킴

# cor : 상관계수 계산해준다 ( 공분산/루트xsd, 루트Ysd)
length(polar_econ)
length(gini$gini)
# cor 명령어를 쓰려면 두 변수의 길이가 같아야 한다.
gini1 <-gini$gini[seq(from=2, to=67, by=2)]
# gini라는 변수에서 2부터 67까지 2씩 건너뛰면서 가져와라 (짝수)
# seq : 위치
cor(gini1,polar_econ)

# 두 변수의 전체 분포 비교
rep112 <-subset(rep, subset=(congress==112))
dem112 <-subset(dem, subset=(congress==112))
hist(dem112$dwnom1, freq=F, xlim=c(-1.5,1.5), ylim=c(0,1.75))
hist(dem112$dwnom1, freq=F, xlim=c(-1.5,1.5), ylim=c(0,1.75)) # freq=F는 y의 값이 빈도가 아닌 density가 나온다
# xlim은 x값의 범위를 지정해준다. ylim은 y값의 범위를 지정해준다. y값의 density는 무조건 0보다 크다
# 두 histogram의 범위를 서로 맞춰주기 위해 xlim과 ylim사용
# 공화당에 극단적 값을가진 사람이 많음
# 민주당은 주로 -의 값 공화당은 주로 +의 값 민주당은 진보적 공화당은 주로 보수적 성향


# Q-Q plot 두 분포를 비교할 때 사용 
# 두 분포의 상대적 분산을 비교할 수 있다.
# 각 분위에 해당하는 두 변수의 값을 찾아서 점을 찍음 (x축의 10%에 있는 값, y축의 10% 분위에 있는 값)
# 두 분포가 동일하다= 두 분위수가 갖다 Q-Q plot은 45도 선이 된다
quantile(dem112$dwnom1, probs=C(0.1, 0.2, 0.3, 0.4, 0.5))
# Q-Q plot이 45도선 위에 그려지면 y축 변수가 x축 변수보다 더 큰 값을 가짐
# 먼저 온게 x축, 뒤 변수가 y축에 오게 된다.
qqplot(rep112$dwnom2,dem112$dwnom2, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5)) #y~x
abline(a=0, b=1) #abline은 이미 그려진 변수에 선을 그음 
#y=a+bx a에 상수 b는 기울기이기 때문에 a=0, b=1은 y=x선을 그림
# 높은 분위에서는 민주당이 크지만 중간에서는 공화당이 더 크다
# 인종적 문제에 관해서 보수적인 입장을 가진 민주당은 공화당보다 더 보수적이다.
# 민주당은 인종적 문제에 관해서는 보수적인 사람과 진보적인 사람의 분산이 더 크다.

# 행렬
x <- matrix(1:12, nrow=3, ncol=4, byrow=T) # 1부터 12까지 3x4 행렬을 만들어라 
# byrow=T는 row방향으로(옆으로) 먼저 숫자를 채워라
# 하나의 행렬 안에 문자와 숫자 섞기 x
x
rownames(x)<-c("a","b","c")
colnames(x)<-c("d","e","f","g")
x
x[1,1] # x의 1,1의 값
x[1,1:2] # 첫 번쨰 row의 1~2의 값

# as.matrix() -> dataframe을 강제로 행렬로 바꿈
y<-data.frame(y1=c(1,3,5), y2=c(2,4,8), y3=c(0.1,0.2,0.3))  # length같아야 함
z<-as.matrix(y)
z
class(z)

# coulmn방향으로 평균, 합 등을 계산하고 싶을 때
colSums(z)
colMeans(z)
rowSums(z)
rowMeans(z)

# apply함수를 이용하면 행렬에서 행 또는 열에 대해 연산을 할 수 있다.
apply(z, MARGIN=1, FUN=median) # 1: 행방향, 2 : 열방향
# z에서 행방향으로 중앙값을 계산하라
apply(z, MARGIN=2, FUN=median)

#list
x<-list(y1=1:10, y2=c("hi", "hello", "hey"))
x
x$y1
x$y2
x[1]
names(x)
length(x) # y1과 y2니까 2
# 직사각형에서 length는 colomn의 수, 하나의 백터에서는 하나의 element 개수
# list에서 length는 data.frame처럼 하나의 element개수