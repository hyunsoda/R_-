# R을 활용한 데이터 분석
# Lecture note 4
# 2022.09.26
setwd("C:/temp100")
minwage <- read.csv(file="minwage.csv")
#wagebefore= 뉴저지에서 최저임금 올리기 전/ fullbefore= 풀타임 일하는 사람들 최저임금 올리기 전 
dim(minwage) # 358개 가게들, 8개 column
nrow(minwage)
summary(minwage)

#subset 함수
minwageNJ <- subset(minwage, subset=(location!="PA")) #!는 ~이 아니다라는 뜻
minwagePA <- subset(minwage, subset=(location=="PA")) # ~와 같다는 == 두 개

# 최저임금 인상 후에 잘 지켜지고 있는지 보기 
# NJ $5.05
# 가장 낮게 받는 사람이 $5.05이상 받는지 확인
mean(minwageNJ$wageAfter<5.05) #NJ주의 최저가 5.05보다 크면 0 작으면 1의 값을 주고 평균을 구함.
# 다 잘 지키고 있다면 0이 나와야 함
# 0.3%가 최저임금 준수를 하지 않고 있음
mean(minwageNJ$wageBefore<5.05)

# PA state의 after, before 임금비율을 계산해라
mean(minwagePA$wageAfter<5.05)
mean(minwagePA$wageBefore<5.05)
# Y 변수 : 정규직 직원의 비율
# X 변수 : 최저임금 인상

# NJ주의 최저임금 인상 후 정규직 비율
minwageNJ$fullPropAfter <- (minwageNJ$fullAfter)/(minwageNJ$fullAfter+minwageNJ$partAfter)
minwagePA$fullPropAfter <- (minwagePA$fullAfter)/(minwagePA$fullAfter+minwagePA$partAfter)

# after 시점에서 두 states의 정규직 비율의 차이를 계산한다.
mean(minwageNJ$fullPropAfter)-mean(minwagePA$fullPropAfter) # 정규직 비율 뉴저지가 더 높음, 부정적인 영향이 없다

tableNJ <- table(minwageNJ$chain)
prop.table(tableNJ)
tablePA <-table(minwagePA$chain)
prop.table(tablePA)

#뉴저지주의 버거킹 데이터
minwageNJ.bk <-subset(minwageNJ, subset=(chain=="burgerking"))
#펜실베니아 버거킹 데이터
minwagePA.bk <-subset(minwagePA, subset=(chain=="burgerking"))
# after시점에서 두 states 의 정규직 비율의 차이
mean(minwageNJ.bk$fullPropAfter)-mean(minwagePA.bk$fullPropAfter)
# +가 나옴 여전히 정규직 비율이 줄어든 것이 아님을 확인할 수 있음

table(minwageNJ.bk$location)

minwageNJ.bk.NS <- subset(minwageNJ.bk, subset=(location=="northNJ")|(location=="southNJ"))
# | (shift+won)는 a or b 라는 뜻
# NJ.bk.NS와 PA.bk를 비교한다. 정규직 비율 비교
# 하위분류화는 교란요인을 줄일 수 있는 장점을 갖고 있지만 해나갈수록 표본의 숫자가 줄어듬 -> 신뢰성이 줄어든다. 
# 빅데이터에서 적합하다

# 중앙값 비교
median(minwageNJ$fullPropAfter)-median(minwagePA$fullPropAfter)
summary(minwageNJ$fullPropAfter)
summary(minwagePA$fullPropAfter)

# InterQuartile Range: IQR 데이터의 분포와 관한 것
# 3rd Quartile에서 1st Quartile 뺀 값
# IQR이 작다는 것은 75%위치와 25% 위치의 차이가 많이 나지 않는다는 뜻 = 작은 범위안에 전체의 50%가 몰려있음
# IQR이 크다는 것은 75%와 25% 범위가 넓다는 것, 50%에 크게 분포, 나머지 50%에 더 크게 분포 
# IQR이 작을수록 데이터들이 모여있고 클수록 데이터들이 퍼져있다.
IQR(minwageNJ$fullPropAfter)
IQR(minwagePA$fullPropAfter)
# PA에서 레스토랑 정규직 비율이 어느 가게에 몰려있음을 알 수 있음

boxplot(minwageNJ$fullPropAfter) # IQR확인 가능

summary(minwageNJ$wageBefore)
summary(minwageNJ$wageAfter)
# 기존에 최저임금 이상을 받던 사람들에게는 영향이 가지 않았음

# quantile은 백분위수 
quantile(minwageNJ$wageAfter) # 사분위수 계산
quantile(minwageNJ$wageAfter, probs=c(0.1,0.9)) # 10분위 수와 90분위 수 계산/ R에서 연속하는 숫자,문자는 백터로 잡아줌
quantile(minwageNJ$wageAfter, probs=seq(from=0, to=1, by=0.1))
# sequence함수는 연속된 숫자를 자동으로 만들어낼 때 사용 0부터 1까지 0.1간격으로라는 뜻
# 최저임금 인상 후 노동자들의 80%가 최저임금만 받고 있는 상황임, 나머지 20%만이 더 받고 있음, 최저임금이 최고임금이 되어버림
