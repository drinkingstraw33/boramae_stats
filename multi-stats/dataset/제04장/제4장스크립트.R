#데이터의 입력
sts <- read.csv("스트레스.csv")

#데이터프레임의 확인
head(sts)

#함수 lm을 이용한 다중회귀분석의 실행（단계1）
res1 <- lm(번아웃2~번아웃1, data=sts)
summary(res1)

#함수  lm을 이용한 다중회귀분석의 실행（단계2）
res2 <- lm(번아웃2~번아웃1+스트레스+지원, data=sts)
summary(res2)

#F값의 산출
((0.4168-0.4039)/(3-1))/((1-0.4168)/(300-3-1))

#p값의 산출
1-pf(3.273663, 2, 296)

#결정계수 증분의 검정
anova(res1, res2)

#AIC의 산출
extractAIC(res1) #단계1 회귀모형의 AIC
extractAIC(res2) #단계2 회귀모형의 AIC

#BIC의 산출
extractAIC(res1, k=log(300)) #단계1 회귀모형의 BIC
extractAIC(res2, k=log(300)) #단계2 회귀모형의 BIC

#중심화
sts$스트레스.c <- sts$스트레스-mean(sts$스트레스) #'스트레스'의 중심화
sts$지원.c <- sts$지원-mean(sts$지원) #'지원'의 중심화

#상호작용항의 작성
sts$상호작용 <- sts$스트레스*sts$지원 #중심화를 하기 전의 상호작용항
sts$상호작용.c <- sts$스트레스.c*sts$지원.c #중심화를 한 후의 상호작용항

#상관계수의 확인
cor(sts[,c("스트레스", "지원", "상호작용")]) #중심화 전의 상관행렬
cor(sts[,c("스트레스.c", "지원.c", "상호작용.c")]) #중심화 후의 상관행렬

#'번아웃1'의 중심화
sts$번아웃1.c <- sts$번아웃1-mean(sts$번아웃1)

#함수 lm을 이용한 다중회귀분석의 실행（상호작용효과의 검토）
res3 <- lm(번아웃2~번아웃1.c+스트레스.c+지원.c+스트레스.c*지원.c, data=sts)
summary(res3)

#결정계수 증분의 검정
anova(res2, res3)

#표준편회귀계수의 산출
z.sts <- as.data.frame(scale(sts)) #데이터의 표준화와 데이터프레임화
res3.z <- lm(번아웃2~번아웃1+스트레스+지원+스트레스*지원, data=z.sts) #표준편회귀계수의 추정
summary(res3.z) #결과의 출력

#단순경사분석（사회적 지원이 많은 경우의 번아웃에 대한 스트레스 경험의 효과）
sts$지원.h <- sts$지원.c-sd(sts$지원.c)
res3.h <- lm(번아웃2~번아웃1.c+스트레스.c+지원.h+스트레스.c*지원.h, data=sts)
summary(res3.h)

#단순경사분석（사회적 지원이 적은 경우의 번아웃에 대한 스트레스 경험의 효과）
sts$지원.l <- sts$지원.c+sd(sts$지원.c)
res3.l <- lm(번아웃2~번아웃1.c+스트레스.c+지원.l+스트레스.c*지원.l, data=sts)
summary(res3.l)

#신뢰구간의 산출
confint(res3)

#변수선택
bsb <- read.csv("야구.csv") #데이터의 입력
head(bsb) #데이터프레임의 확인
library(MASS) #패키지 MASS의 입력
base <- lm(연봉~1, data=bsb) #절편만의 모형
step.res <- stepAIC(base, direction="both", scope=list(upper=~타수+안타+타점+홈런+볼넷+사구+삼진+타율)) #스텝 와이즈법에 따른 변수 선택
summary(step.res) #최종적인 변수선택의 결과


#연습과 해답
#문1해답
ssk <- read.csv("성적.csv")
res1 <- lm(시험성적~성별+학원유무+자신감, data=ssk)
summary(res1)
res2 <- lm(시험성적~성별+학원유무+자신감+열정, data=ssk)
summary(res2)

#문2해답
anova(res1, res2)

#문3해답
extractAIC(res1)
extractAIC(res2)

#문4해답
ssk$열정.c <- ssk$열정-mean(ssk$열정)
res3 <- lm(시험성적~성별+학원유무+자신감+열정.c+학원유무*열정.c, data=ssk)
summary(res3)

#문5해답
ssk$열정.h <- ssk$열정.c-sd(ssk$열정.c)
res3.h <- lm(시험성적~성별+학원유무+자신감+열정.h+학원유무*열정.h, data=ssk)
summary(res3.h)

ssk$열정.l <- ssk$열정.c+sd(ssk$열정.c)
res3.l <- lm(시험성적~성별+학원유무+자신감+열정.l+학원유무*열정.l, data=ssk)
summary(res3.l)


#문6해답
library(MASS)
base <- lm(시험성적~1, data=ssk)
step.res <- stepAIC(base, direction="both", scope=list(upper=~성별+학원유무+자신감+열정+평일공부시간+휴일공부시간+뉴스시청+독서+실외놀이))
summary(step.res)

