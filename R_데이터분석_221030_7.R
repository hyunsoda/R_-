#R을 활용한 데이터 분석
#221031
#Lecture note 7 and 8
########
setwd("c:/temp100")
# loop 사용하기
for (i in 1:100) {
  print(i*2)
}
# 1번부터 100번 까지 loop를 실행하는데 print(i)를 실행하라
x <- 1:100
for (i in x){
  print(i*2)
}
# 비어있는 백터 만들어서 loop실행하기
# 비어있는 백터 만들기
# rep(): reapeat , NA : missing
x1<- rep(NA,10) #x1이라는 백터는 비어있는 10개의 값
x1

values<-c(2,4,6)
n <- length(values) #3이 된다.
results <-rep(NA,n)
results #results는 na가 세 개 나오는 백터가 된다.

for (i in 1:n){
  results[i] <- values[i]*2
  cat(values[i], "times is equal to", results[i], "\n")
  cat("current i =", i, "\n")
}
# loop가 어디쯤인지 알려주기 위함 current i
# "\n" 한 줄 띄어라
# cat : 설명하고 싶을 때 print 대신 cat사용
# values 값에 *2한 값을 오게 함
results

results<-rep(NA,3)
data<- data.frame ("a"=1:2, "b"=c("hi", "hey"), "c"=3:4)
data
for (i in 1:3){
  results[i] <- median(data[,i])
}
results

# loop를 사용하지 않은면 반복해서 코드를 작성해야 한다.
results[1] <- median(data[,1])
results[2] <- median(data[,2])
results[3] <- median(data[,3])

x2 <-c(1,2,3,4)
x3 <- ifelse(x2>3, "Yes", "No")
# x2가 3보다 크면  "Yes" 실행, 작으면 "No"실행
x3

operations <- "add"
if (operations=="add"){
  4+4
}
if (operations=="multiply"){
  4*4
}
# operation의 값은 add로 주어졌기 때문에 "add"일 때는 4+4가 나오지만 "multiply"값은 false이므로 4*4의 값은 안 나옴

operations <- "multiply"
if(operations=="add") {
  4+4
} else{
  4*4
}
# if else는 하나의 구문이기 때문에 else를 밑으로 내리지 말고 붙여서 사용해야 함
# operations가 "add"이면 4+4를 실행하지만 그 외의 경우에는 4*4를 실행하라

operations <- "subtract"
if (operations=="add") {
  4+4
} else if (operations=="multiply"){
  4*4
} else{
  cat("Invalid operations")
}

values <-1:5
results<- rep(NA,5)
for(i in 1:5){
  x<-values[i]
  r<- x%%2 # a%%b는 a를 b로 나누었을 때 나머지
  if (r==0) {
    results[i]<-2*values[i]
  } else {
    results[i]<-1*values[i]
  }
}
# results 는 1부터 5까지인데 r이 0이면 2를 곱한 값 r이 0이 아니면 1을 곱한 값
results
# 몫 quotient
10%/%2 # 몫
10%%2 # 나머지

setwd("C:/temp100")
pres08<-read.csv(file="pres08.csv")
polls08<-read.csv(file="polls08.csv")
# margin 승자와 패자의 득표율 차이
# margin이 작을 수록 비등한 것
# margin이 크면 한 쪽이 압도적으로 이긴 것
polls08$margin <- polls08$Obama-polls08$McCain
pres08$margin<-pres08$Obama-pres08$McCain

# 비어있는 백터 만들기
poll.pred<- rep(NA,51)
poll.pred1<- rep(NA,51)
# 가장 최근 여론조사 결과 가져와서 루프로 채워넣기
x<-as.Date("2008-10-27")
x
y<-as.Date("2008/10/27")
y
# as.Date : 문자를 날짜 class로 바꿔준다.
polls08$Electionday<- as.Date("2008-11-04")
polls08
polls08$ElectionToDay<-polls08$Electionday-as.Date(polls08$middate)
  # 선거 날짜에서 여론 조사날짜 뺀 것
str(polls08)
st.name<-unique(polls08$state)
# unique : name 중복되지 않고 하나씩 가져오기
st.name
names(poll.pred)<-st.name
names(poll.pred1)<-st.name
poll.pred 

for (i in 1:51){
  state.date<-subset(polls08, subset=(state==st.name[i]))
  latest <- subset(state.date,subset=(ElectionToDay==min(ElectionToDay)) )
  latest1<-subset(state.date,subset=(ElectionToDay==max(ElectionToDay)) )
  poll.pred[i]<- mean(latest$margin) # margin이라는 변수를 가져와서 빈 칸에 집어넣어라
  poll.pred1[i]<- mean(latest1$margin)
  # mean은 선거와 가까운여론조사가 한 개 이상이면 그의 평균을 가져와라
} # 자기 선거까지 남은 날짜가 가장 작은 것을 가져옴
# state이라는 변수가 st.name의 첫 번째 값과 같으면 남겨라 (AL주)
# min은 가장 가까이 있는 여론조사
# max는 가장 멀리 있는 여론조사

# poll.pred가 가장 가까운 선거날짜와 가장 가까운 여론조사의 날짜가 있는 선거 득표율 차이
poll.pred

# 실제 margin과 poll.pred(여론조사 margin)의 차이를 계산한다.
# 차이를 계산한다.

errors<- pres08$margin-poll.pred ## 주의할 점 : pres08과 poll.pred의 state 순서가 같아야 한다.
errors # 예측오차
# 예측의 성과는 어떻게 평가할 것인가?
mean(errors)

# 예측 성과를 평가할 때 평균을 보는 것은 문제가 있다.
# 큰 값이라도 +와 -가 trade off되면 0으로 나타난다.
# RMSE : Root Mean Square Error 평균오차제곱합
# RMSE : 가장 최근 여론조사 예측성과는 RMSE=5.90
sqrt(mean(errors^2))
# RMSE의 값이 상대적으로 적을수록 우리는 예측성과가 더 우수하다고 해석한다.

errors1<- pres08$margin-poll.pred1 
sqrt(mean(errors1^2))
hist(errors) # y축을 freq
hist(errors, freq=F, ylim = c(0,0.08)) # y 축이 density
abline(V=mean(errors),col="red")
# abline(v: vertical, h: horizontal, a, b(직선의 기울기))

# Obama 와 Mccain의 득표율의 차이 : margin
# margin에 대해서 예측하였다.
# margin 변수는 연속형 변수

# 미국 선거는 득표율 margin이 중요하지 않다
# 각주의 승자를 예측하는 것이 중요하다.
# 특정 주에서 Obama가 승리하면 1, 패배하면 -1
# 선거인단 수를 예측한다.

# 오바마 승리하면 1, 패배하면 -1의 값을 준다.
# pres08,poll.pred(여론조사 결과)
# 실제 선거결과(winner)와 예측결과가 서로 다른 state는 어디인가
pres08$state[sign(pres08$margin)!=sign(poll.pred)] # poll.pred자체가 margin
# pres08$margin과 poll.pred$margin을 비교해서 같지 않은 것을 sign하라
# 51개 주 중에 단 3개 주를 제외하고는 정확히 선거결과를 제대로 예측했다.
# 정확하게 맞춘 비율은 
48/51

# 혼동 행렬 만들기
y.pred<- sign(poll.pred) # 예측결과 +1,-1
x.pred<-sign(pres08$margin) # 실제 선거결과 +1, -1
table(y.pred, x.pred)
# CCR (Correctly Classification Rate) 대각선 값들
# 94.1%
# 범주를 예측하고 그 예측성과를 평가한다.
# confusion matrix에서 CCR을 계산한다.

# 가장 최근 여론조사를 이용해서 CCR=94.1%가 되었다
# 가장 멀리 있는 여론조사를 이용하면 CCR=?
y.pred<- sign(poll.pred1)  # 여론조사 결과
x.pred<-sign(pres08$margin) # 실제 선거 결과
table(y.pred, x.pred) # confusion martrix
# CCR= 42/51
42.51
# -1 : Negative, 1 : Positive
# CCR
42/51
# FP : False positive Pr (Yhat=1|Y=0)
0/22
# FN : False negative Pr(Yhat=0|Y=1)
9/29

# 미국의 270 선거인단을 확보하면 이김 (전체 선거인단 수가 538명)

# 364명 (2008년 오바마가 획득한 선거인단 수)
sum(pres08$EV[pres08$margin>0]) # EV에서 margin이 0보다 큰 경우만 더해라

poll.pred
sum(pres08$EV[poll.pred>0]) # poll.pred는 여론조사에서 나온 margin