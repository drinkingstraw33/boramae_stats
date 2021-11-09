#데이터의 입력
dkk <- read.csv("동기부여.csv")

#데이터프레임의 확인
head(dkk)

#모형의 기술
dkk.model <- '
 F1=~I1+I2+I3+I4
 F2=~E1+E2+E3+E4
'

#모수의 추정
library(lavaan) # lavaan 패키지의 입력
dkk.fit <- cfa(dkk.model, data=dkk, std.lv=TRUE) #모수의 추정

#결과의 츨력
summary(dkk.fit, fit.measures=TRUE, standardized=TRUE)


#데이터의 입력
math <- read.csv("수학시험.csv")

#데이터프레임의 확인
head(math)

#모형의 기술
math.model <- '
 F1=~math1+math2+math3+math4+math5+math6+math7+math8+math9+math10
'

#모수의 추정
math.fit <- cfa(math.model, data=math, ordered=c("math1", "math2", "math3", "math4", "math5", "math6", "math7", "math8", "math9", "math10"), std.lv=TRUE)

#결과의 출력
summary(math.fit, fit.measures=TRUE, standardized=TRUE)

#인자가 하나이고 관측 변수가 두개인 확인적 인자분석
math.model2 <- ' #모형의 기술
 F1=~math1+math2
'
math.fit2 <- cfa(math.model2, data=math, ordered=c("math1", "math2"), std.lv=TRUE) #모수의 추정

#등가제약
math.model3 <- ' #모형의 기술
 F1=~b*math1+b*math2
'  
math.fit3 <- cfa(math.model3, data=math, ordered=c("math1", "math2"), std.lv=TRUE) #모수의 추정
summary(math.fit3, fit.measures=TRUE, standardized=TRUE) #결과의 출력

#인자부하량을 1로 고정한 확인적 인자분석
dkk.fit2 <- cfa(dkk.model, data=dkk) #모수의 추정
summary(dkk.fit2, fit.measures=TRUE, standardized=TRUE) #결과의 출력

# 부적해
dkk.model3 <- ' #모형의 기술
 F1=~I1+I2
 F2=~I3+I4
'
dkk.fit3 <- cfa(dkk.model3, data=dkk, std.lv=TRUE) #모수의 추정
summary(dkk.fit3, fit.measures=TRUE, standardized=TRUE) #결과의 출력



#데이터의 입력
knj <- read.csv("감정.csv")

#데이터프레임의 확인
head(knj)

#모형의 기술（4인자 모형）
knj.model1 <- '
 F1=~A1+A2+A3
 F2=~C1+C2+C3
 F3=~M1+M2+M3
 F4=~P1+P2+P3
'
knj.fit1 <- cfa(knj.model1, data=knj, std.lv=TRUE) #모수의 추정
summary(knj.fit1, fit.measures=TRUE, standardized=TRUE) #결과의 출력

#모형의 기술（2차 인자모형）
knj.model2 <- '
 F1=~A1+A2+A3
 F2=~C1+C2+C3
 F3=~M1+M2+M3
 F4=~P1+P2+P3
 H=~F1+F2+F3+F4
'
knj.fit2 <- cfa(knj.model2, data=knj, std.lv=TRUE) #모수의 추정
summary(knj.fit2, fit.measures=TRUE, standardized=TRUE) #결과의 출력



#연습과 해답
#문1 해답
skk <- read.csv("성격.csv")
skk.model1 <- '
 F1=~온화+쾌활+외향적+친절+사교적+협조적+적극적+솔직
'
skk.fit1 <- cfa(skk.model1, data=skk, std.lv=TRUE)
summary(skk.fit1, fit.measures=TRUE, standardized=TRUE)

#문2 해답
skk.model2 <- '
 F1=~쾌활+외향적+사교적+적극적
 F2=~온화+친절+협조적+솔직
'
skk.fit2 <- cfa(skk.model2, data=skk, std.lv=TRUE)
summary(skk.fit2, fit.measures=TRUE, standardized=TRUE)

