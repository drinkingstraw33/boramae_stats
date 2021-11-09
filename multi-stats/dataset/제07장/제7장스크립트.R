#데이터의 입력
dkk <- read.csv("동기부여.csv")

#데이터프레임의 확인
head(dkk)

#고윳값의 산출
cor.dkk <- cor(dkk) #상관행렬의 산출
eigen(cor.dkk) #고윳값의 산출

#스크리 플롯의 출력
library(psych) #패키지 psych의 입력
VSS.scree(dkk) #스크리 플롯의 출력

#평행분석
fa.parallel(dkk, fm="ml", fa="pc", n.iter=100)

#탐색적 인자분석（초기해）
fa.dkk1 <- fa(dkk, nfactors=2, fm="ml", rotate="none") #모수의 추정
print(fa.dkk1, sort=TRUE, digits=3) #결과의 출력

#탐색적 인자분석（초기해）：상관행렬을 지정한 분석
fa.dkk.cor <- fa(cor.dkk, nfactors=2, fm="ml", rotate="none", n.obs=500) #모수의 추정
print(fa.dkk.cor, sort=TRUE, digits=3) #결과의 출력

#탐색적 인자분석（프로맥스 회전）
library(GPArotation) #패키지 GPArotation의 입력
fa.dkk2 <- fa(dkk, nfactors=2, fm="ml", rotate="promax") #모수의 추정
print(fa.dkk2, sort=TRUE, digits=3) #결과의 출력

#α계수의 산출
dkk.nht <- dkk[, c("I1","I2","I3","I4")]
alpha(dkk.nht) #내발적 동기부여척도의 α계수

dkk.ght <- dkk[, c("E1","E2","E3","E4")]
alpha(dkk.ght) #외발적 동기부여척도의 α계수

#ω계수의 산출
omega(dkk.nht, nfactors=1) #내발적 동기부여척도의 ω계수

omega(dkk.ght, nfactors=1) #외발적 동기부여척도의 ω계수


#데이터의 입력
math <- read.csv("수학시험.csv")

#데이터프레임의 확인
head(math)

#인자수의 결정
cor.math <- polychoric(math)$rho #Polychoric 상관행렬의 산출
eigen(cor.math) #고윳값의 산출
VSS.scree(cor.math) #스크리 플롯의 출력
fa.parallel(cor.math, fm="ml", fa="pc", n.iter=100, n.obs=300) #평행분석

#탐색적 인자분석（1인자해）
fa.math <- fa.poly(math, nfactors=1, fm="ml") #모수의 추정
print(fa.math, sort=TRUE, digits=3) #결과의 출력

#신뢰성계수의 산출
alpha(cor.math, n.obs=300) #알파계수의 산출
omega(cor.math, nfactors=1, n.obs=300) #오메가계수의 산출



#연습과 해답
#문1 해답
skk <- read.csv("성격.csv")
cor.skk <- cor(skk)
eigen(cor.skk)

#문2 해답
library(psych)
VSS.scree(skk)

#문3 해답
fa.parallel(skk, fm="ml", fa="pc", n.iter=100)

#문4 해답
library(GPArotation)
fa.skk <- fa(skk, nfactors=2, fm="ml", rotate="promax")
print(fa.skk, sort=TRUE, digits=3)

#문5 해답
skk2 <- skk[,c("쾌활", "적극적", "외향적", "사교적")]
alpha(skk2)

skk3 <- skk[,c("협조적", "온화", "솔직", "친절")]
alpha(skk3)

#문6 해답
omega(skk2, nfactors=1)
omega(skk3, nfactors=1)


