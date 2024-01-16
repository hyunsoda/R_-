# R_데이터분석
# Lecture Note 11
# 2022-11-28
setwd("C:/temp100")
florence <-read.csv(file="florentine.csv",row.names="FAMILY")
# FAMILY라는 변수는 데이터에 포함시키지 말고 숫자들만 행렬에 포함이 되게 만듬
# 0은 결혼 네트워크가 없다
# 1은 결혼 네트워크가 있다.
# 결혼은 방향이 없는 undirected 네트워크

# matrix
florence <-as.matrix(florence)
florence
florence[1:5,1:5]

# 타 가문과 가장 인척관계가 많은 가문은?
# 옆으로 합치기
# rowSums, colSums
rowSums(florence)
# Medici가문이 인척 네트워크가 가장 많은 가문이다.

install.packages("igraph")
library(igraph)
# 인접행렬 데이터를 이용해서 작성한다.
florence1 <-graph.adjacency(florence, mode="undirected",diag=F)
# mode는 무방향성 네트워크
# diag=F : 자신의 가문과는 결혼할 수 없기 때문에 대각선은 0이다/ 자기자신과는 연결x
plot(florence1,vertex.label.cex=0.7)
# 개별 unit을 mode라고 부름 (동그란 부분)
# edge는 node를 선으로 연결한 것
# 무방향성이기 때문에 화살표가 아닌 선으로 표시
# vertex=node, label=글자, cex=글자크기 -> 글자크기를 70프로로 줄여라
 
# degree는 edge의 수를 나타낸다. 중심성 측정
degree(florence1)
# rowSums값과 같음
# 값이 높을수록 다른 가문과 인척성이 높다
# 단점 : 인접한 가문을 넘어서는 가문은 볼 수 없다. 관련이 있어도 바로 옆에 인접하지 않으면 볼 수 없음

# 근접성 계산
closeness(florence1) # 값은 0과 1사이
# 근접성은 총 엣지의 수(farness)의 역수
# 근접성의 역수는 farness (이심성) 총 엣지의 수
1/(closeness(florence1)*15)
# 15 : 자기 자신을 제외한 가문의 수
# Ex. Medici 가문은 평균 1.6개의 edge로 다른 가문을 연결할 수 있디 -> 중심성이 높음

# 매개성
betweenness(florence1)
# Medici가문은 다른 가문을 연결할 때 중요한 역할을 함
# 가문 총 수는 16개, 본인가문을 제외하면 15개 가문이 된다.
# Medici가문의 47.5는 총 105개의 연결고리 중 47.5%
# 105개의 연결고리는 만약 1번 가문을 본다면 1번이 들어간 것 제외, 자기 자신과의 연결 제외한 모든 경우의 수

# 중심성을 동그라미 크기로 나타내기
florence1 <-graph.adjacency(florence, mode="undirected",diag=F)
plot(florence1, vertex.size=betweenness(florence1), vertex.label.cex=0.7)

a1<-closeness(florence1)*1000
a1<-a1[-12] # 12번 째 값은 연결된 것이 없어서 결측치가 나옴 빼줘야함
plot(florence1, vertex.size=a1, vertex.label.cex=0.7)
# 근접성의 값이 너무 작아서 1000을 곱해줌 큰 의미 x

#####################################################3

# 공간 데이터(Spatial Data)
install.packages("maps")
library(maps)
data(us.cities)
View(us.cities)
table(us.cities$capital)
map(database = "usa") # 괄호 안에 아무것도 안 치면 world
map(database = "world", regions= "South Korea")

map(database = "usa")
# 50개의 주도만 표시
capitals<-subset(us.cities, subset=(capital==2))
points(capitals$lat ~ capitals$long, cex=capitals$pop/50000)
# x 좌표는 longitude y 좌표는 latitude
# cex를 통해 원의 크기 조절
# 인구에 따른 원크기 (50000은 값이 너무 크게 나와서 줄여준 것, 큰 의미x)
# 다시 그리려면 75번째 줄부터 다시 실행 (덧그리는 것이기 때문에)

# 특정 지역만 불러오기
map(database = "state", regions="California")

# 공간 폴리곤 데이터
map(database="usa")
# 좌표들을 보자
usa1<-map(database="usa",plot=F)
# 7252개의 데이터 개수
length(usa1$x) #longitude
usa2<-map(database="state",plot=F)
length(usa2$x)

#########################################

map(database="state", col="blue", fill=T) 
# fill= 채우다 파란색으로 채움

colors()
rgb(red=1, green=0, blue=0)
# 색을 섞을 떄 rgb()사용

# 파란색과 빨간 색만 표현한다
pres08<-read.csv(file="pres08.csv")

pres08$Dem<-pres08$Obama/(pres08$Obama+pres08$McCain) # 민주당 득표율
pres08$Rep<-pres08$McCain/(pres08$Obama+pres08$McCain) # 공화당 득표율

# Obama가 이긴 주는 파란색, 
# McCain이 이긴 주는 빨간색으로 표시하자
map(database="state", col=ifelse(pres08$Dem>pres08$Rep,"blue","red"), fill=T) 

# 보라색 채색을 하는 것을 목표로 한다. 
map(database="state", region="California", col=rgb(red=0.4, blue=0.6, green=0), fill=T)
# red 0.4 득표 blue 0.6득표
# 득표별 색 짙기로 나타내고 싶으면 득표율에 따라 rgb에서 값을 주면 된다
map(database="state", region="Nevada", col=rgb(red=0.6, blue=0.4, green=0), fill=T, add=T)
# add=T가 없으면 각각의 주만 나옴, 
# add=T값을 주면 50개의 주가 하나의 그림으로 합쳐져서 나옴


