# R_데이터분석
# Lecture note 10
# 2022-11-21
setwd("C:/temp100")
install.packages("SnowballC")
install.packages("tm")
library(tm) # text mining 주어진 텍스트에서 단어를 뽑아낸다
library(SnowballC) # 어간처리 ex. opened의 어간은 open

# Corpus ->말뭉치(언어학)
corpus.raw<-Corpus(DirSource(directory="federalist", pattern="fp"))
# directory는 폴더이름을 적는다 폴더 불러오기
# pattern="fp" -> federalist 폴더 안에 fp로 시작하는 파일 모든 걸 불러와라
# 각각의 파일을 불러와서 corpus라는 함수를 적용해라
corpus.raw

# 모든 글자를 소문자로 바꾼다.
corpus.prep<-tm_map(corpus.raw, content_transformer(tolower))

# 불필요한 공백을 제거한다. (white space)
corpus.prep<-tm_map(corpus.prep, stripWhitespace)

# 구두점을 제거한다. (마침표를 제거한다)
corpus.prep<-tm_map(corpus.prep, removePunctuation)

# 숫자 제거
corpus.prep<-tm_map(corpus.prep, removeNumbers)

# 불용어 제거 (stopwords) 단어로 보기 어려운 것 I, You 등
stopwords("english") # 영어에서 사용되는 불용어들을 보여준다
corpus.prep<-tm_map(corpus.prep, removeWords, stopwords("english")) # removewords 다음에 어떤 단어를 삭제할 것인지

# 말뭉치를 전처리한 과정이 된다.
inspect(corpus.prep[1]) # 첫 번째 논문의 전처리 결과
content(corpus.prep[2]) # 2번 컨텐츠를 보자

# 어간처리
corpus<-tm_map(corpus.prep,stemDocument)

# corpus를 가지고 dtm만든다 (document term matrix 문서-용어 행렬) 
#다른 말로 sparse matrix라고 함 희소성 매트릭스 -> element에 있는 값이 거의 다 0
dtm<-DocumentTermMatrix(corpus)
dtm
# terms 4849 -> 4849개의 단어를 찾아냄
# 이 matrix의 크기는 85*4849개의 행렬
# non-/sparse entries 더한 숫자가 전체개수 (85*4849)
# 0이 적혀있는 element를 sparse라고 부름, 0이 아닌 element는 nonsparse
# sparsity : 367248/412165 sparse/전체
# 전체 중 89%가 0이다
# maximal term length : 가장 긴 단어 18글자
# term frequency : 첫 번째 문장에서 he라는 단어가 1번 쓰였으면 1 

dtm[1:5, 1:8] # 첫 번째 5줄에서 첫 번째 8단어 보기
# 5*8행렬의 document term matrix의 결과
# 앞은 row 뒤는 coulmn
# 5*8개의 elements
# sparsity는 40개 중에 22개 -> 55%

# document에서 만든 것을 행렬로 만들어야지만 실제 값(숫자)을 볼 수 있다
dtm1<-as.matrix(dtm)
dtm1[1:5,1:8]

# word cloud
install.packages("wordcloud")
library(wordcloud)
# matrix 구조에서 만들어야 한다
wordcloud(colnames(dtm1),dtm1[12,],max.words=20)
wordcloud(colnames(dtm1),dtm1[24,],max.words=20)
# 첫 번째 arguement는 colnames는 사용된 단어들 
# 두 번째 arguement는 어떤 row사용할 것인지 (어떤 논문에서 가져올 것인가)
# max.words=2 # 최대 20개까지만 만들기
nrow(dtm1) #85편의 논문

# tf-idf : 특정 문서에서 단어의 상대적 중요성
dtm.tfidf<- weightTfIdf(dtm) # 행렬이 아닌 그냥 document사용
dtm.tfidf
dtm.tfidf1<-as.matrix(dtm.tfidf) # 결과를 행렬상태로 변환
dtm.tfidf1[1:5,1:5]
# 값이 높을수록 중요하다

# 가장 중요한 단어 정렬
head(sort(dtm.tfidf1[12,], decreasing=T))
# 12번쩨 논문의 가장 중요한 값 6개만 정렬
head(sort(dtm.tfidf1[24,], decreasing=T))

# 저자예측 
# 어간화되지 않은 corpus.prep 사용
# 1000단어 당 몇 번 사용됐는 지
dtm1<-as.matrix(DocumentTermMatrix(corpus.prep))
tfm<-dtm1/rowSums(dtm1)*1000
# rowSums는 옆으로 단어를 다 더함 -> 전체 단어 수
tfm[1:5,1:5]
ncol(tfm) # column의 개수
# 첫 문서에서 able이란 단어는 1000단어 당 1.3번 사용됐다

# 관심 있는 단어 찾아내기
# 우선 관심있는 단어를 백터로 만든다.
words<-c("although","always", "commonly","consequently", "considerable", "enough", "upon", "whilst")
# there, while : 불용어로서 이미 삭제가 되어 있다.
tfm1<-tfm[,words]
tfm1[1:5,1:5]

# hamilton이 썼다고 알려진 문서들
hamilton<-c(1,6:9,11:13,15:17,21:36,59:61,65:68)
hamilton1<-colSums(tfm1[hamilton,])
# hamilton이 쓴 전체 문서에서 8개 단어의 term frequency의 1000개 단어 당 비율
# colSums는 밑으로 더함
hamilton1
hamilton1<-hamilton1/length(hamilton)
hamilton1
# 하나의 문서에서의 term frequency

# madison이 썼다고 알려진 문서들
madison<-c(10,14,37:48,58)
madison1<-colSums(tfm1[madison,])
# hamilton이 쓴 전체 문서에서 8개 단어의 term frequency의 1000개 단어 당 비율
# colSums는 밑으로 더함
madison1
madison1<-madison1/length(madison) # 문서 한 개당 단어 frequency
madison1

# rbind는 rowbind
tfm.ave<-rbind(hamilton1,madison1)
tfm.ave

# 저자를 예측하기 위해서는 tfm.ave 결과를 활용한다
# y변수를 생성한다.
author<-rep(NA,nrow(dtm1))
# nrow(dtm1)은 전체 문서의 개수
# 85개의 결측치 값이 생성된다
author
author[hamilton]<-1
author[madison]<- -1
author # y 변수가 된다

# x변수 생성하기
# x변수는 용어빈도 자체를 사용할 수도 있다.
# 특정 단어의 1000단어당 빈도 수를 사용할 수도 있다.
# y와 x가 같이 있는 data frame을 만든다
author.data<- data.frame(y=author[c(hamilton,madison)],tfm[c(hamilton,madison),])
# tfm= 1000단어당 빈도수

# linear regression : y,x변수가 필요하다
hm.fit<- lm(y~ upon+consequently+whilst, data=author.data)
summary(hm.fit)
# + 값은 upon이란 단어가 많을 수록 hamilton일 가능성이 크다
# - 값은 consequently란 단어가 많을수록 hamilton일 가능성이 낮아진다.
hm.fitted<-fitted(hm.fit)
hm.fitted
# 값이 +면 hamilton이 저자일 가능성이 크다
# 값이 -면 madison이 저자일 가능성이 크다
sd(hm.fitted) # 예측값의 표준편차 = 0.765
summary(hm.fit)
# upon이란 단어를 1000단어 당 1번 사용하면 hamilton일 가능성이 0.12만큼 증가한다
# 표준편차 이내로 증가한다
# 값이 표준편차 이내로 움직여야 안정적이다고 할 수 있다.

# In-sample prediction
# hamilton과 madison 논문을 이용하여 회귀분석을 하고
# fitted값을 얻었다
# fitted>0 -> 해밀턴, fitted<0 매디슨 논문
mean(hm.fitted[author.data$y==1]>0)
# y==1은 해밀턴 논문 
mean(hm.fitted[author.data$y==1]<0)

# 남은 일 : disputed papers에 대해선 fitted값을 계산하고 
# 그 값이 0보다 큰지 0보다 작은지 판단한다.
# fitted값이 0보다 크면 hamilton, 0보다 작으면 madison

disputed<-c(49,50,57,62,63)
# as.data.frame() 행렬에서 data frame으로 변환하기
tf.disputed<-as.data.frame(tfm[disputed,])
pred<-predict(hm.fit,newdata=tf.disputed)
pred
# 5편 논문에 대한 frequency를 hm.fit regression에 넣어서 예측값을 계산한다.

# 50번 논문만 hamilton논문이고 나머지는 madison논문이다
# 5편의 disputed 논문 중 4편은 매디슨 논문이고 한 편만 hamilton논문이다.