# LN1 
# 2022-09-05
setwd("C:/temp100") # working directory 를 지정
getwd()

#working directory 내에 있는 데이터 불러오기
auto <-read.csv(file="AUto.csv")
view(auto)

install.packages("haven")
library(haven)

# Unpop.dta
data1<- read_dta(file="Unpop.dta")

install.packages("rgl")
library(rgl)
example(plot3d)

install.packages("readxl")
library(readxl)
auto1<-read_excel("AUto1.xlsx")
# read_excel(Auto1.xlsx", Sheet="")

#패키지가 제공하는 R 데이터
install.packages("lmtest")
library(lmtest)
data(package="lmtest")
data("bondyield")

#ISLR 패키지를 설치한 후 Caravan data를 가져와라
install.packages("ISLR")
library(ISLR)
data(package="ISLR")
data("Caravan")

# help() 또는 ? 를 사용하여 해당 도움말을 얻는다
help(read.csv)
?read.csv
?haven::read_dta # haven 안에 있는 read_dta 도움말 알려줘

# 외부 패키지인 경우 반드시 library()로 불러들인 후 도움말 기능 사용 가능

# header=T
# 첫 번째 행에 있는 값을 열 이름으로 받아들인다.

# stringAsFactors=F
# 데이터 프레임에서 문자형이 자동으로 factor로 인식되는 것을 막는다
# 문자형을 factor가 아닌 character로 인식되도록 한다.


