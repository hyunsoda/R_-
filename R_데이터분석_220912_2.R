# R 데이터 분석 
# Lecutre Note 2
# 2022-09-12

setwd("c:/temp100") #working directory 저장
world.pop<- c(2525799,3026033,3691173,4449049,5320817,6127700,6916183)
world.pop #numeric vector 숫자로 된 백터

y<- c("firm", "year","Income","sales")
y

#logical values : True of FAlse
z<- c(TRUE, FALSE, TRUE,TRUE)

# class : y라는 백터의 속성알기
class(y)

#백터의 길이
# element의 숫자가 몇 개인지
length(world.pop)
pop.first <- c(2525779, 3026003, 3691173)
# 두 번째 벡터를 밑에 갖다 붙이기
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second) #numeric vector
pop.all

#Indexing : 백터의 특정 요소를 가져오고자 하는 경우 [대괄호 사용]
# [ ]: square bracket

world.pop[2]
world.pop[1:2] # 1~2
world.pop[c(1,3)] #첫 번째와 세 번째 element가져오기 (element의 위치를 c라는 백터로 잡아두기)
world.pop[-3] #3번을 제외한 모든 값

# Q. 1번과 3번을 제외한 모든 값은?
world.pop[c(-1,-3)]

# 산술연산

pop.mil <- world.pop/1000
pop.mil

pop.rate <- world.pop/world.pop[1] #world pop 값을 world pop의 첫 번째 값으로 나눠라
pop.rate #Ex. 첫 번째 값이 1980년대 값일 때 pop.rate을 통해 1980년대에 비교해서 몇배가 되었는지 알 수 있음

# Q)인구증가율을 계산하라. 전 시점에 비해서 인구증가율(%)을 계산하라
growth <- 100*(world.pop[2:7]-world.pop[1:6])/world.pop[1:6]
growth

#list

x <-c(1,3,5,7,9) #numeric vector
y <-c("firm", "year") #char vector (문자백터)
analysis1 <-matrix(1:9, nrow=3) # 3 by 3 matrix #nrow=열
analysis1
# anylsis2는 이차원의 렬, data.frame의 형태이고, firm과 variable이라는 변수가 존재하고 각각 안에는 3개의 element가 존재
analysis2 <-data.frame(firm=c("R1","R2","R3"),variable=c("C1","C2","C3")) #data frame
analysis2 #엑셀 형태와 비슷

z <- list(x, y, analysis1, analysis2) #다양한 형태를 한 번에 묶음
z 
class(z)
z[1] #indexing가능

# vector, list, data.frame 
WK <-c(4,7,16,12) #length=4
AE <-c(3,5,11,11) #length=4
FL <-c(2,5,12,17)
SP<-c(0,0,4,0) #length=4
# 백터를 엮어 data.frame으로 만들때 각각의 백터의 길이는 같아야 한다.

excersice1<-data.frame(WK,AE,FL,SP)
excersice1
View(excersice1)

length(world.pop) #numeric vector
mean(world.pop)
median(world.pop)
min(world.pop)
max(world.pop)
sum(world.pop)/length(world.pop) #평균을 계산하는 것과 같다
range(world.pop)
year<-seq(from=1950, to=2010, by=10) #from, to, by가 있음 1950년부터 2010년까지 10년씩 건너 뛰면서
year
# seq()함수는 증가 혹은 감소 순서로 구성된 백터를 생성한다.
seq(from=1, to=10, by=2)
seq(from=2010, to=1950, by=-10) # 감소

#백터에 named하기
world.pop
names(world.pop)<- year #world.pop에 year를 이름처럼 배정
world.pop
names(world.pop)

#나만의 function
myf1<-function(x) {
  out1<-mean(x)
  return(out1)
}
#x라는 input이 들어가서 x라는 input의 평균을 계산해서 out1로 저장하고 out1을 myf1의 결과로 개시하라
#return은 out1번의 결과를 myf1의 결과로 개시하라는 뜻
x<-1:10
myf1(x)
x<-1:100
myf1(x)

myf2<-function(x){
  s.out<-sum(x)
  l.out<-length(x)
  m.out<-mean(x)
  out<-c(s.out, l.out, m.out)
  names(out)<-c("sum","length","mean")
  return(out)
}
#x라는 input이 들어가는데 x를 더한 값, x의 길이, x의 평균을 모은 것을 out으로 저장하고 
#각각 sum, length, mean으로 이름붙여라
x<-1:10
myf2(x)

# Q)world.pop 백터에 myf2 함수를 적용하라.
x<-world.pop # world.pop는 숫자 백터
myf2(x)

myf2(world.pop) # x의 역할을 world.pop가 한다.

setwd("C:/temp100")
#UNpop.csv 파일을 저장했다.
Unpop <- read.csv(file="UNpop.csv")
View(Unpop)

names(Unpop)
ncol(Unpop) #the number ofcolumns (variables)
nrow(Unpop) #the number of rows(obs)
dim(Unpop) #dimensions
summary(Unpop) #summary statistics (mean, sd,min, max)

#1st Quartile (1사분위수): 25% 위치에 있는 값
#3rd Quartile (3사분위수): 75% 위치에 있는 값

#R에서만든 데이터(객체)를 저장하고자 하는 경우 : save,image 함수를 사용한다.
# 저장하면 RData 확장자명을 갖게 된다.

# UNpop.RData
# R 데이터를 불러오는 법
rm(z) #z를 삭제
rm(list=ls()) # 모든 객체를 삭제한다.

load(file="UNpop.RData")

str(UNpop)
# $표시를 통해 각각의 변수에 접근 
UNpop$year #Unpop의 year 변수가져오기
UNpop$world.pop
 
UNpop[,1] #indexing 2차원이기 때문에 앞에 row을 비워두면 모든 row을 가져온다/앞은 row 뒤는 column
UNpop[,2]
UNpop[1,]
UNpop[1:3,1] #1부터 3까지 첫 번째 column

UNpop[,"year"]
UNpop[1,"world.pop"] #첫 번째 row에서 world.pop의 값

# R에서 결측치는 NA로 표시
world.pop1<-c(UNpop$world.pop, NA)
world.pop1 # 결측치를 가진 백터이다.

mean(world.pop1) #평균을 계산하라.
mean(world.pop1, na.rm=T) #결측치 제거 na=NA rm=remove T=true(적용해라)  mean은 결측치를 제거해줘야 한다
summary(world.pop1) #summary는 자동으로 결측치를 제외하고 계산


# 객체 저장: RData 파일로 저장
save.image(file="CH1_1.RData") # save.image는 만들어 놓은 모든 객체를 저장
load(file="CH1_1.RData")

save(world.pop1, file="CH1_2.RData") #save는 원하는 객체만 ,로 나열한 후 저장
rm(list=ls())
load(file="CH1_2.RData")

load(file="CH1_1.RData")

# data frame -> csv 파일로 내보내고자 하는 경우

write.csv(UNpop, file="pop1.csv", row.names=F)
#row의 name을 주지 말아라 F=주지 말아라 번호가 있는 column은 지우고 나머지만 내보내기

# Linting
# R 코드의 잠재적 오류, 잘못된 명령이 있는지 확인해볼 수 있다.
install.packages("lintr")
library(lintr)
lint("UNpop_error.R") 
#snake case: 변수 이름 연결할 때 언더바 쓰는 것