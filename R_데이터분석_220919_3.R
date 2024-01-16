# R 데이터 분석 3강 
#2022.09.19
setwd("C:/temp100")
resume <- read.csv(file="resume.csv")

dim(resume)[1] #관측치 개수, 변수의 개수 [1]은 첫 번째 값만 가져옴
head(resume, n=10) #앞 6개 값만 나옴 10명 나오게 하고 싶으면 괄호 안에 n=10
tail(resume)
resume[2,4] #indexing 2번쨰 row의 4번째 column

#summary statistics 계산하기
summary(resume) #4개 변수의 기초통계량 계산
#mean 0.08? -> 8%만 전화회신을 받았다는 뜻 1값의 비율이 8%

#two-way frequency table : race and call
#one-way freq table
table(resume$race)
table1<-table(resume$race,resume$call)
addmargins(table1)
prop.table(table1, margin=1) #margin= row(옆으로)더하면 1 , margin=2로 쓰면 아래로 더해야 1이 나옴

table(resume$firstname) #Latoya의 이름 개수 확인
nrow(resume) #전체 관측치가 몇 개인지 알려줌
table1[1,2]
sum(table1[,2])

mean(resume$call)
resumeB <-resume[resume$race=="black",] #black만 가져와서 새로운 데이터프레임을 만듬/row indexing하려면 앞쪽에서 comma,
#coulumn을 비웠다는 것은 다 가져오라는 뜻
# ==: 조건에서 같다 표시할 때는 ==로 사용
resumeW <-resume[resume$race=="white",]
#백인의 회신율
mean(resumeW$call)
mean(resume$call)

# Logical value (True.False)
class(TRUE) #True=1
class(FALSE) #FALSE=0

as.integer(TRUE) #TRUE를 정수로 바꿔라
as.integer(FALSE)

x<-c(TRUE, FALSE, TRUE)
mean(x)
sum(x)

resume[1,4] # 첫 번째 row의 4번째 값
resume$race[4] # race의 4번째 값, 일차원의 변수가 되기 때문에 ,필요없음
(resume$race=="black")[1:5] #resume의 처음 다섯개의 값이 black과 같은지 표시해라 (TRUE or FALSE)

# 데이터의 일부분 가져오기
# subset 명령어를 활용하자.

# resumeB1이라는 데이터를 새로 만드는데(subset) 원데이터의 일부분을 가져옴 -> race가 black과 같은 데이터를 가져옴
resumeB1 <- subset(resume, subset=(race=="black")) 
resumeW1 <- subset(resume, subset=(race=="white")) 

resumeBf <- subset(resume, select = c("call","firstname"), 
                   subset=(race=="black" & sex=="female")) # subset 두 개 설정
resumeBm <- subset(resume, select = c("call","firstname"), 
                   subset=(race=="black" & sex=="male"))
mean(resumeBf$call)
mean(resumeBm$call)
# select는 column을 지정할 때 사용

resumeF1 <- subset(resume, subset=(sex=="female"))
resumeM1 <- subset(resume, subset=(sex=="male"))
mean(resumeF1$call)
mean(resumeM1$call)

# ifelse함수
# ifelse(X,Y,Z)
# 조건을 쓰고 만족하면 첫번째 숫자(Y), 만족하지 않으면 두 번째 숫자(Z)
resume$BF <- ifelse(resume$race=="black" & resume$sex=="female", 1, 0)
# resume$BF라는 함수를 만드는데 흑인&여성이면 1, 아니면 0
mean(resume$BF)
table(resume$BF)

# Three-way table
# 앞의 two-way table을 세 번째 변수가 1일때 한 번 만들고, 0일 때 한 번 만들어라
table(resume$race, resume$sex, resume$BF)

# Factor Variable
# 문자를 숫자로 바꿀 때 주로 사용
resume$race1 <- as.factor(resume$race)
# race변수를 숫자로 저장 (생긴 모양은 같은데 글자 밑에 숫자가 있다고 생각)
str(resume) # structure 어떤 변수들이 어떻게 저장되어있는지 보는 것 / race1이 factor라고 저장되어 있는 것 확인가능
levels(resume$race1) #levels:범주형 변수가 몇 개
table(resume$race1)

# tapply 함수
# 범주형 변수일때만 작동
# 연속형 변수의 통계를 계산하는데 범주별로 계산 EX. call이라는 변수의 평균을 계산하고 싶은데 인종별로 계산하고 싶음
tapply(resume$call, INDEX=resume$race, FUN=mean) #FUN은function
tapply(resume$call, resume$race, mean) #call 변수를 race에 따라 계산

# firstname을 factor variable로 변환한다.
resume$first1 <- as.factor(resume$firstname)
# call 변수의 평균을 firstname 별로 계산한다.
tapply(resume$call, resume$first1, mean)
# 회신율이 가장 높은 이름은?
name1 <- tapply(resume$call, resume$first1, mean)
sort(name1, decreasing=T) #decreasing=T : 내림차순으로 정렬
sort(name1, decreasing=F) #decreasing=F : 오름차순으로 정렬
# Brad
# 회신율이 가장 낮은 이름은?
# Aisha