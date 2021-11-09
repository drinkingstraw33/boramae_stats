#데이터의 입력
kwz <- read.csv("우물안개구리.csv")

#데이터프레임의 확인
head(kwz)

#급내상관계수의 산출
library(ICC) #패키지 ICC 입력
ICCest(as.factor(학급), 학업적자기개념, data=kwz, alpha=0.05, CI.type="Smith")

#중심화
kwz$시험점수.m <- ave(kwz$시험점수, kwz$학급) #시험점수의 집단평균（학급평균）값의 산출
kwz$시험점수.cwc <- kwz$시험점수-kwz$시험점수.m #집단평균중심화
kwz$시험점수.cgm <- kwz$시험점수-mean(kwz$시험점수) #전체평균중심화

#상관계수의 산출
round(cor(kwz[,c("시험점수.m", "시험점수.cwc", "시험점수.cgm")]), 3)

#랜덤절편모형
library(lmerTest) #패키지 lmerTest 입력
model1.cwc <- lmer(학업적자기개념~시험점수.cwc+(1|학급), data=kwz, REML=FALSE)
summary(model1.cwc)

#랜덤경사모형
model2.cwc <- lmer(학업적자기개념~시험점수.cwc+(1+시험점수.cwc|학급), data=kwz, REML=FALSE)
summary(model2.cwc)

#시험점수의 학급평균값 중심화
kwz$시험점수.cm <- kwz$시험점수.m-mean(kwz$시험점수)

#집단수준의 변수를 포함하는 다수준 모형
model3.cgm <- lmer(학업적자기개념~시험점수.cgm+시험점수.cm+(1+시험점수.cgm|학급), data=kwz, REML=FALSE)
summary(model3.cgm)

#교차 수준의 상호 작용항을 포함하는 다수준 모형
model4.cgm <- lmer(학업적자기개념~시험점수.cgm+시험점수.cm+시험점수.cgm*시험점수.cm+(1+시험점수.cgm|학급), data=kwz, REML=FALSE)
summary(model4.cgm)

#모형 비교
anova(model3.cgm, model4.cgm)

#신뢰구간의 산출
confint(model4.cgm, method="Wald")


#연습과 해답
kch <- read.csv("가치.csv")

#문1 해답
library(ICC)
ICCest(as.factor(학생), 흥미, data=kch, alpha=0.05, CI.type="Smith")

#문2 해답
kch$가치.cwc <- kch$가치-ave(kch$가치, kch$학생)
kch$기대.c <- kch$기대-mean(kch$기대)

#문3 해답
library(lmerTest)
model1 <- lmer(흥미~가치.cwc+(1|학생), data=kch, REML=FALSE)
summary(model1)

#문4 해답
model2 <- lmer(흥미~가치.cwc+(1+가치.cwc|학생), data=kch, REML=FALSE)
summary(model2)

#문5 해답
model3 <- lmer(흥미~가치.cwc+기대.c+가치.cwc*기대.c+(1+가치.cwc|학생), data=kch, REML=FALSE)
summary(model3)

#문6 해답
anova(model1, model2, model3)

