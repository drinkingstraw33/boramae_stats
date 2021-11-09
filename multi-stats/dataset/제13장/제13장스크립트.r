#'제조사' '브랜드파워' '기술력'의 3중 교차집계표
b3dat <- read.csv("자전거데이터3.csv",row.names=1)#데이터의 입력
head(b3dat)

xtabs(~제조사+브랜드파워+기술력,data=b3dat)

#'자전거데이터2.csv'의 입력
b2dat  <- read.csv("자전거데이터2.csv",row.names=1)#데이터의 입력
b2dat

#대응분석의 실행
library(FactoMineR) #패키지의 입력
resb2dat <- CA(b2dat) #대응분석의 실행

#고윳값의 출력
resb2dat$eig

#행 점수와 열 점수의 출력
resb2dat$row$coord #행 점수의 표시
resb2dat$col$coord #열 점수의 표시
              
#함수 summary의 출력(일부 발췌)
summary(resb2dat)

#'제조사'의 군집분석
z <- scale(b2dat) # 열방향으로 z점수화

#제곱 유클리드 거리
D0 <- dist(z, method = "euclidean")
D <- (1/2)*D0^2

#계층적 군집분석
resclust <- hclust(D,method="ward.D")
plot(resclust) #덴드로그램

#군집의 해석
clus <- factor(cutree(resclust,k=2))　#군집번호 취득
clus
b2dat$cluster <- clus
by(b2dat[,-6],b2dat$cluster,apply,2,mean) #군집별 평균값의 산출


#함수 dummie.data.frame을 이용한 데이터 행렬의 변환
library(dummies) #패키지 입력
db3dat <- dummy.data.frame(b3dat,sep=":")　#함수 dummie.data.frame의 실행
head(db3dat) #처음 6행의 일부를 표시

#함수 CA를 이용한 (다중)대응분석의 실행
resdb3dat <- CA(db3dat)
resdb3dat$eig #고윳값 출력의 일부

#열 범주만의 그래프
plot(resdb3dat, invisible="row")
#plot(resdb3dat, invisible="col") #행 범주(평가자)만의 그래프

#함수 MCA를 이용한 다중 대응분석의 실행1
resb3dat <- MCA(b3dat)

#다중 교차집계표의 변환
crosb3dat <- xtabs(~제조사+브랜드파워+가성비+기술력+경기실적+디자인,data=b3dat)#다중 교차집계표의 작성
crosdf<- as.data.frame(crosb3dat)#데이터프레임의 변환
head(crosdf) 
nrow(crosdf) #행수의 확인

#도수가 0인 셀을 제외
crosdf2 <- crosdf[(which(crosdf[,7]>=1)),]　#도수가 1 이상인 행만 선택
nrow(crosdf2)

head(crosdf2)

#함수 MCA를 이용한 다중 대응분석의 실행2
rescrosdf2<-MCA(crosdf2,quanti.sup=7,row.w=crosdf2$Freq)

#표 13.1에 대한 대응분석의 출력
b2dat2 <- b2dat[,1:3]
resb2dat2 <- CA(b2dat2) #대응분석의 실행
summary(resb2dat2) 
round(dist(rbind(resb2dat2$row$coord,중심=c(0,0))),3) #제조사간의 거리

#연습과 해답
#문1 해답
exdat <- read.csv("자전거데이터연습2.csv") 

#문2 해답
library(dummies)
dexdat <- dummy.data.frame(exdat,sep=":")

#문3 해답
library(FactoMineR)
rdexdat <- CA(dexdat)
summary(rdexdat)

#문4 해답
rexdat <- MCA(exdat)
summary(rexdat)

#문5 해답
dfexdat <- as.data.frame(xtabs(~.,data=exdat))
dfexdat2 <- dfexdat[which(dfexdat$Freq>=1),]

#문6 해답
rdfexdat2 <- MCA(dfexdat2,quanti.sup=7,row.w=dfexdat2$Freq)
summary(rdfexdat2)
