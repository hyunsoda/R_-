# R 데이터 분석
# Lecture note 9
# 2022-11-14
setwd("C:/temp100")
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

# social
social<-read.csv(file="social.csv")
table(social$messages)
# neighbors가 social press에 해당하는 부분

# generate three dummy variables
social$Control <-ifelse(social$messages=="Control",1,0)
social$Hawthorne <-ifelse(social$messages=="Hawthorne",1,0)
social$Neighbors <-ifelse(social$messages=="Neighbors",1,0)
social$CD <-ifelse(social$messages=="Civic Duty",1,0)
# 범주가 4개이면 dummy 변수 3개만 만든다
fit5<-lm(primary2006 ~ Control+Hawthorne+Neighbors, data=social)
summary(fit5)
names(social)
# coefficient는 빠진 변수와의 차이를 알려줌
# Neighbors 값이 0.063 -> 내가 neighbors그룹에 속하면 civic duty그룹에 비해 0.063만큼 투표를 더 많이 한다.
# Hawthorne effect : 내가 실험의 대상이 되고 있는 것을 알게 되면 행동이 달라지는 것
# control이 -값 -> civic duty에 비해 control그룹이 투표를 더 적게한다
# 가장 효과가 좋은 방법은 neighbors

# messages안에 이미 변수들이 들어있음
# 자동으로 알파벳 순에서 1번을 기준변수로 잡음
# 결과는 같음
fit6<-lm(primary2006~messages, data=social)
summary(fit6)
0.314538+-0.017899 # control그룹의 평균 투표율
mean(social$primary2006[social$messages=="control"]) #control 그룹의 평균 투표율

# neighbors 그룹은 control 그룹 보다 얼마나 투표율이 높은가?
# beta3hat-beta1hat = 0.-63411-(-0.17899)

# predict : y hat 값을 계산해줌
# x에 값을 부여해야함
# unique함수는 데이터 프레임 변주에서 변수가 무엇이 있는 지 뽑아내줌
x1<- unique(social$messages)
unique.messages<-data.frame(messages=x1)
unique.messages
predict(fit6, newdata=unique.messages)
# newdata는 어떤 값을 x값에 집어넣을지 설정
# 각 그룹의 투표값(예측값)을 보여준다
# civic duty값 0.314는 상수항임을 알 수 있다.
# EX. 2번은 hawthorne (yhat=alpha hat + beta2hat) (hawthorne의 coefficient와 intercept값 더해주기)

# Prediction from the regression
# 각 그룹의 평균 투표율과 정확히 일치한다.
# data frame에서 특정 범주의 범주별로 또 다른 평균을 계산할 때
# tapply
tapply(social$primary2006, social$messages, FUN=mean)

# 왜 regression 예측값이 정확히 각 그룹의 평균과 일치하는가?

# dummy 변수를 모두 사용하고 싶으면 상수항을 제외하면 된다.
# 각 그룹의 coefficient는 그 그룹의 평균값이 된다.
fit7 <- lm(primary2006~-1+CD+Control+Hawthorne+Neighbors, data=social)
# -1를 더해주면 상수항을 제외하고 추정하라는 뜻
summary(fit7)

# 값의 이름을 다 써주는 대신 messages 사용
fit8<-lm(primary2006~ -1+messages, data=social)
fit8

# 2004년 투표를 했던 사람들
voter<-subset(social, subset=primary2004==1)
nonvoter<-subset(social, subset=primary2004==0)
# 투표했던 그룹 내에서 Neighbors와 Control의 차이를 본다
tapply(voter$primary2006,voter$messages,FUN=mean)
# tapply와 밑에 있는 voter_N,C와 같은 값 나옴
voter_N<-mean(voter$primary2006[voter$messages=="Neighbors"])
voter_C<-mean(voter$primary2006[voter$messages=="Control"])
voter_N
voter_C
# 투표를 과거에 했던 그룹에서 평균 차이 (처리효과)

# 투표를 하지 않았던 그룹
tapply(nonvoter$primary2006,nonvoter$messages,FUN=mean)
# tapply와 밑에 있는 voter_N,C와 같은 값 나옴
nonvoter_N<-mean(nonvoter$primary2006[nonvoter$messages=="Neighbors"])
nonvoter_C<-mean(nonvoter$primary2006[nonvoter$messages=="Control"])
nonvoter_N
nonvoter_C
# 과거에 투표를 했던 그룹에서 캠페인의 캠페인의 효과가 더 크다

# 이질적 처리효과를 평균 차이로 계산했는데, 
# regression을 통해서도 같은 결과를 얻을 수 있다.

# social 전체 데이터에서 neighbors와 control만 남긴다.
social_NC<-subset(social, subset=(messages=="Control"|messages=="Neighbors"))
# social 그룹에서 messages가 Control이거나 Neighbors 둘 중 하나에 속하면 데이터로 남겨라 
# | = or

fit9 <-lm(primary2006 ~ primary2004+messages+primary2004:messages, data=social_NC)
# primary2004:messages는 두 변수를 곱한다는 뜻
summary(fit9)
# primary2004:messagesNeighbors의 coefficient는 
#투표를 한 사람에서의 neighbor control처리효과와 하지 않은 사람내에서 neighbor control처리 효과의 차이
 fit10<-lm(primary2006~primary2004*messages, data=social_NC)
# primary2004*messages 곱하면 각각의 상호작용과 곱한 값까지 나옴
summary(fit10)
#fit9와 같은 결과





