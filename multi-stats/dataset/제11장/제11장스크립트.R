#데이터의 입력、범주형 변수의 변환、데이터프레임의 확인
aks <- read.csv("빈집털이조사.csv") #데이터의 입력
aks$빈집털이01 <- ifelse(aks$빈집털이 == "있음",1,0)
aks$보안01 <- ifelse(aks$보안 == "가입",1,0)
aks$경비견01 <- ifelse(aks$경비견 == "있음",1,0)
head(aks)

#로지스틱 회귀분석의 실행
aks.out <- glm(빈집털이01 ~ 부재시간 + 대화 + 건축년수 + 보안01 + 경비견01, family = "binomial", data = aks)

#로지스틱 회귀분석의 출력 표시
summary(aks.out)

#계수・절편의 지수변환값 산출과 해석
exp(aks.out$coefficients)

#계수・절편에 관한 신뢰구간의 산출
confint(aks.out, level = 0.95)
exp(confint(aks.out, level = 0.95))

#표준화계수의 산출
LRAstdcoef <- function(glm.out, vname){ #glm.out-함수glm을 이용한 출력 객체, vname-표준화하는 양적인 설명변수명
	#vname에 지정된 변수로만 구성된 데이터프레임
	subdat <- (glm.out[["data"]])[ , vname]
	#subdat 각 변수의 표준편차
	SDs <- apply(subdat, 2, sd)
	#표준화 전의 추정값
	rawcoef <- (glm.out[["coefficients"]])[vname]
	#표준화 후의 추정값
	stdcoef <- rawcoef * SDs
	return(stdcoef)
}
LRAstdcoef(aks.out,c("부재시간", "대화", "건축년수"))

#Hosmer-Lemeshow의 적합도 검정
library(ResourceSelection) 
hoslem.test(x = aks.out$y, y = fitted(aks.out)) #x-목적변수의 관측값, y-목적변수의 예측값（확률）

#AIC와 BIC의 산출
extractAIC(aks.out) #AIC
extractAIC(aks.out, k = log(nrow(aks.out$data))) #BIC

#독립변수군의 유효성 확인
aks.out_null <- glm(빈집털이01~1, family = "binomial", data = aks)
anova(aks.out_null, aks.out, test="Chisq") 

#변수선택의 실행
step(aks.out_null, direction = "both", scope = ( ~ 부재시간 + 대화 + 건축년수 + 보안01 + 경비견01))

#다중공선성의 확인
library(car)
vif(aks.out)


#연습과 해답
#문1 해답
sks <- read.csv("자격시험.csv")
sks$시험결과01 <- ifelse(sks$시험결과 == "합격", 1, 0)
sks$기원01 <- ifelse(sks$기원 == "있음", 1, 0)

#문2 해답
sks.out <- glm(시험결과01 ~ 공부시간 + 기원01 + 연령, family = "binomial", data = sks)
summary(sks.out)

#문3 해답
exp(sks.out$coefficients)

#문4 해답
LRAstdcoef(sks.out, c("공부시간", "연령")) #앞에서 기술한 LRAstdcoef의 입력이 필요
exp(LRAstdcoef(sks.out, c("공부시간", "연령")))

#문5 해답
library(ResourceSelection) 
hoslem.test(x = sks.out$y, y = fitted(sks.out)) 

#문6 해답
library(car)
vif(sks.out)
