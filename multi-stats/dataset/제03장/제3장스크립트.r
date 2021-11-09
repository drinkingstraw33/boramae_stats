#데이터 입력
csdat <- read.csv("고객만족도데이터.csv")
head(csdat,3)


#함수 lm을 이용한 중회귀분석의 실행
res1<-lm(고객수~입지만족도+설비만족도+면적만족도+트레이너만족도,data=csdat)
summary(res1)　

#설명변수간의 상관계수
cor(csdat$트레이너만족도,csdat$트레이너수)

#설명변수간의 상관이 높은 경우의 중회귀분석 출력
resm1 <- lm(고객수~트레이너만족도,data=csdat)
summary(resm1)

resm2 <- lm(고객수~트레이너만족도+트레이너수,data=csdat)
summary(resm2)

#VIF의 산출예
library(car) #패키지 car의 입력
vif(resm2) #함수 vif를 이용한 VIF의 산출


#고객만족도의 4변수에 관한 VIF의 산출
vif(res1)

#절편과 편회귀계수의 신뢰구간 산출
confint(res1,level=0.95)

#표준편회귀계수의 산출
scsdat <- as.data.frame(scale(csdat))#데이터의 표준화와 데이터프레임화
res2 <- lm(고객수~입지만족도+트레이너수,data=scsdat) #표준편회귀계수의 추정

#함수 lm 출력의 일부
summary(res2)

#질적변수도 포함한 중회귀분석
res3 <- lm(고객수~입지만족도+설비만족도+면적만족도+트레이너만족도+접객연수+입회특전,data=csdat)
summary(res3)　

#AIC의 산출
extractAIC(res1) #설명변수가 양적변수만인 회귀모형
extractAIC(res3) #설명변수에 질적변수도 포함한 회귀모형

#BIC의 산출
extractAIC(res1,k=log(30)) #설명변수가 양적변수만인 회귀모형
extractAIC(res3,k=log(30)) #설명변수에 질적변수도 포함한 회귀모형

#t분포
pt(-0.6949209,25)*2  #절편의 p값
(1- pt(3.075611,25))*2  #설비만족도 편회귀계수의 p값


#qt의 산출
qt(0.025,25) #하측확률이 0.025로 주어진 qt의 산출
qt(0.975,25) #상측확률이 0.025로 주어진 qt의 산출


#신뢰구간의 산출
21.640-2.059539*7.036 #신뢰구간의 하한
21.640+2.059539*7.036 #신뢰구간의 상한

#연습과 해답
#문1 해답
kwamokdat <- read.csv("과목시험결과.csv")
dim(kwamokdat)
colnames(kwamokdat)

#문2 해답
restest <- lm(final~t1+t2+t3+t4+t5,data=kwamokdat)

#문3 해답
library(car)
vif(restest)

#문4 해답

#t4를 삭제하는 경우
restest2 <- lm(final~t1+t2+t3+t5,data=kwamokdat)
#t2를 삭제하는 경우
restest2 <- lm(final~t1+t3+t4+t5,data=kwamokdat)
summary(restest2)

#문5 해답
confint(restest2)


#문6 해답
restest3 <- lm(final~t1+t2,data=kwamokdat)
extractAIC(restest2)
extractAIC(restest3)

#문7 해답
round(cor(kwamokdat),2)
