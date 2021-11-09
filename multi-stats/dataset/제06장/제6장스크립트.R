#데이터의 입력
shp <- read.csv("실패.csv")

#데이터프레임의 확인
head(shp)

#모형의 기술
shp.model <- '
 실패불안~질책
 학습의욕~격려
 학업성적~실패불안+학습의욕
'

#모수의 추정
library(lavaan) # 패키지 lavaan의 입력
shp.fit <- sem(shp.model, data=shp) #모수의 추정

#결과의출력（표준화추정값，결정계수）
summary(shp.fit, standardized=TRUE, rsquare=TRUE)

#결과의출력（표준화추정값，결정계수，신뢰구간）
summary(shp.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE)

#결과의출력（표준화추정값，결정계수，신뢰구간，적합도지표）
summary(shp.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE)

#모든 적합도지표의 출력
fitmeasures(shp.fit)

# 수정지표의 출력
modindices(shp.fit)

#오차변수 사이에 상관을 가정한 모형으로 분석
shp.model2 <- ' #모형의 기술
 실패불안~질책
 학습의욕~격려
 학업성적~실패불안+학습의욕
 실패불안~~학습의욕
'
shp.fit2 <- sem(shp.model2, data=shp) #모수의 추정
summary(shp.fit2, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE) #결과의 출력

# 표본공분산행렬을 기초로 한 분석
shp.cov <- cov(shp) #표본공분산행렬의 산출
shp.cov.fit <- sem(shp.model2, sample.cov=shp.cov, sample.nobs=500) #모수의 추정
summary(shp.cov.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE) #결과의 출력


#연습과 해답
#문1 해답
hyk <- read.csv("수업평가.csv")
hyk.model <- '
 흥미~난이도+유용성
 학습행동~흥미
 성적~학습행동
'

#문2 해답
hyk.fit <- sem(hyk.model, data=hyk)

#문3 해답
summary(hyk.fit, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE)

#문4 해답
modindices(hyk.fit)

#문5 해답
hyk.model2 <- '
 흥미~난이도+유용성
 학습행동~흥미+난이도
 성적~학습행동
'
hyk.fit2 <- sem(hyk.model2, data=hyk)
summary(hyk.fit2, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE)

#문6 해답
summary(hyk.fit2, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE, ci=TRUE)



