#데이터의 입력，데이터프레임의 확인
sws <- read.csv("행복조사.csv") #데이터의 입력
head(sws)

#모형의 기술（계수를 고정）
sws.model1 <- "
f1 =~ 1 * E1 + E2 + E3
f2 =~ 1 * R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + f2
f4 ~ f1 + f2 +f3
f1 ~~ f2
"
#모형의 기술（분산을 고정）
sws.model2 <- "
f1 =~ E1 + E2 + E3
f2 =~ R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + f2
f4 ~ f1 + f2 + f3
f1 ~~ 1 * f1 + f2
f2 ~~ 1 * f2
"

#모형의 추정
library(lavaan)
sws.fit <- lavaan(model = sws.model1, data = sws, auto.var = TRUE) #auto.var-외생변수의 분산을 추정대상으로 한다（TRUE）,하지 않는다（FALSE）
# sws.fit<-lavaan(model = sws.model1, sample.cov = cov(sws), sample.nobs = nrow(sws), auto.var = TRUE) #표본공분산행렬과 표본크기를 지정하여 추정

#결과의 출력
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE) #fit.measures-적합도, standardizes-표준화 추정값, ci-95%신뢰구간을 출력한다（TRUE）

#모든 적합도지표를 출력
fitmeasures(sws.fit)

#모형 적합도에 관한 부분적 평가의 지표（잔차행렬）
residuals(sws.fit, type = "cor")

#모형 적합도에 관한 부분적 평가의 지표（수정지표）
modindices(sws.fit)

#분산설명률의 출력
lavInspect(sws.fit, "rsquare")

#추정값 경로도 그리기
library(semPlot)
semPaths(sws.fit, whatLabels = "std", layout = "tree2", curve = 1.2, #whatLabels-경로에 부여된 수치의 종류, layout-경로도의 형식, curve-양방향 화살표의 굴곡 정도
	optimizeLatRes = TRUE, edge.color = "black", nCharNodes = 0, #optimizeLatRes-잠재 변수에 새겨진 오차의 각도를 조정하다, edge.color-화살표의 샛, nCharNodes-도형내에 표시하는 문자수
	edge.label.position = c(rep(0.4, 17), rep(0.5, 18)), edge.label.cex = 0.8)#edge.label.position-화살표 위의 수치표시 위치（모수의 수만큼만 지정한다. 공분산은 두 개만큼 세어진다）

#모형의 기술（모수 함수의 정의를 포함）
sws.model3<-"
f1 =~ 1 * E1 + E2 + E3
f2 =~ 1 * R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + a * f2
f4 ~ f1 + b * f2 +c * f3
f1 ~~ f2
DRE  := b
IDRE := a * c
TTE  := b + a * c
"
sws.fit <- lavaan(model = sws.model3, data = sws, auto.var = TRUE)
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)


#연습과 해답

#문3 해답（예）
sws <- read.csv("행복조사.csv")
sws.model<-"
f1 =~ E1 + E2 + 1 * E3
f2 =~ R1 + R2 + 1 * R3
f3 =~ M1 + M2 + 1 * M3
f4 =~ H1 + H2 + 1 * H3
f1 ~ f3
f4 ~ f2 + f3
"
sws.fit <- lavaan(model = sws.model, data = sws, auto.var = TRUE)

#문4，문5
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

