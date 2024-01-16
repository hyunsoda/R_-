# R_데이터분석
# Lecture note 12
# 2022-12-05
setwd("C:/temp100")
k<-10 # 10명이 있다. 10명 중에 최소 2명이 생일이 같을 확률
# pr0: 10명이 모두 생일이 서로 다를 확률
# log(pr0)=lfactorial
pr0<-exp(lfactorial(365)-k*log(365)-lfactorial(365-k))
pr2<-1-pr0
pr2
k<-23
pr0<-exp(lfactorial(365)-k*log(365)-lfactorial(365-k))
pr2<-1-pr0
pr2
# 23명이 있으면 50%를 넘는다

# 함수로 만들기
birthday<-function(k){
  pr0<-exp(lfactorial(365)-k*log(365)-lfactorial(365-k))
  pr2<-1-pr0
  return(pr2)
}
birthday(10)
birthday(50)

k<-2:50
bday<-birthday(k)
plot(bday~k)
abline(h=0.5, col="red")

# Monte carlo simulation
set.seed(1234)
k<-23 #pr2 is close to 0.507
sims<-1000 # the number of simulations

event<-0
for (i in 1:sims){
  days<-sample(1:365, size=k, replace=T) #replace=T : 복원추출
  days.unique<-unique(days) # unique는 중복된 것을 한 번으로 취급
  if(length(days.unique)<k){ #unique로 중복된 것을 취급하면 23보다 값이 작아지니까 그럴경우 event +1
    event<- event+1 # 중복이 있을 때마다 event에 1을 더함 , 총 시행에서 event의 값이 중복의 값
  }
}
event # event 500정도 나와야 함
# days<-sample(1:365, size=23, replace=T)
# days
# 23개의 생일날짜가 나옴
event/sims # 사건발생횟수/ the number of simulations

###########################
FLVoters<-read.csv(file="FLVoters.csv")
# VTD는 구역
# 결측치가 1번이라도 나오면 삭제한다
FLVoters<-na.omit(FLVoters) #887명이 삭제되었다.
dim(FLVoters)

# 한계확률 : Pr(race)
# Pr(race==white) : 인종에 대한 한계확률
tab1<- table(FLVoters$race)
6213/9113
prop.table(tab1)

# A : race, B : 성별(gender)
# Pr(race=white)=Pr(white, female)+ Pr(white, male)
# 교집합의 확률 : joint probability
tab2<- table(FLVoters$race, FLVoters$gender)
prop.table(tab2)
0.360035115+0.321738176

# Pr(흑인|여성)
tab3<- table(FLVoters$gender)
prop.table(tab3)
0.360035115/0.5358279
# 한 번에 계산
tab4<-table(FLVoters$race[FLVoters$gender=="f"])
prop.table(tab4)

# race에 대한  marginal probability
# Pr(race=Asian)
joint.p<-prop.table(tab2)
joint.p
rowSums(joint.p) #옆으로 합침

colSums(joint.p) # 여성, 남성 marginal probability


# three-way table
# age group만들기
FLVoters$age.group<-NA
FLVoters$age.group[FLVoters$age<=20]<-1
FLVoters$age.group[FLVoters$age>20 &FLVoters$age<=40]<-2
FLVoters$age.group[FLVoters$age>40 &FLVoters$age<=60]<-3
FLVoters$age.group[FLVoters$age>60]<-4
table(FLVoters$age.group)
 
# race, gender, age,group
joint3<-table(FLVoters$race, FLVoters$gender, FLVoters$age.group)
prop.table(joint3)
# 마지막 변수를 기준으로 결합확률이 만들어진다
1/9113
# 결합확률 : Pr(race, gender, age.group)

# Pr(A교집합B교집합|C)구하기 (A : white, B : female C:age=1)
# = Pr(A교B교C)/Pr(c)
prop.table(table(FLVoters$age.group))
0.0059256008/Pr(age.group=1)
0.0059256008/0.01766707
0.3354037

# Pr(A|B교C) (A : white, B : female, C : age.group=1)
# Pr(A교B교C)/pr(B교C)
prop.table(table(FLVoters$gender, FLVoters$age.group))
0.0059256008/0.009656535
0.6136363

# 0.613
# Pr(B|C)=Pr(female|age.group=1)
tab6<-table(FLVoters$gender[FLVoters$age.groupr==1])
prop.table(tab6)
0.3354037/0.5465839
# = 0.613636

















