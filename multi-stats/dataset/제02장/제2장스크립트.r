#데이터프레임 구조의 표시
jhk <- read.csv("인사평가결과.csv")#데이터 입력
str(jhk)#함수str을 이용한 데이터 구조의 출력

#데이터프레임에 변수를 추가한다
jhk$총합평균 <- apply(jhk[,10:11],1,mean)#'총합평균'을 데이터 프레임에 추가
str(jhk)

#도수분포표
score <-c(1,5,2,10,8,2,1,4,3,3)
table(score)


#factor형으로 변환한 도수분포표
fscore <- factor(score,levels=seq(0,10,1))#함수factor에 의한 변환
str(fscore)#구조의 확인
table(fscore)#도수분포표


#=, ≠에 의한 조건추출
mdat <- subset(jhk,성별=="M")#남성 데이터만 추출한다
head(mdat)
mdat2 <- subset(jhk,성별!="F")#이쪽도 남성의 데이터만 추출한다
head(mdat2)


#<, ≦, >,≧에 의한 조건추출
cope1 <- subset(jhk, 협동성<50)#협동성이 50점 미만(<)인 행을 추출한다
head(cope1)
cope2 <- subset(jhk, 협동성<=50)#50점 이하(≦)인 행을 추출한다
head(cope2)
cope3 <- subset(jhk, 협동성>50)#50점보다 큰(>) 행을 추출한다
head(cope3)
cope4 <- subset(jhk, 협동성>=50)#50점 이상(≧)인 행을 추출한다
head(cope4)

#논리곱과 논리합에 의한 조건추출
m1 <- subset(jhk,(성별=="M")|(연령=="숙련"))#남성 또는 숙련(논리합)
head(m1)
m2 <- subset(jhk,(성별=="M")&(연령=="숙련")&(기능>=50))#남성 및 숙련 및 기능이 50점 이상(논리곱)
head(m2)
m3 <- subset(jhk,(성별=="M")&((연령=="중견")|(연령=="숙련")))#남성 및 중견 또는 숙련(논리합과 논리곱)
head(m3)

#subset을 이용하지 않은 행의 조건추출
cond <- (jhk$성별=="M")&((jhk$연령=="중견")|(jhk$연령=="숙련"))#조건에 부합하는 행에 관해서는 TRUE를，부합하지 않는 행에는 FALSE를 부여한다
head(cond)#위에 정의한 벡터 cond의 처음 6요소를 표시
m4 <- jhk[cond,]
head(m4)

#결측 데이터의 입력
kesson <- read.csv("결측데이터.csv")#결측 부분이 공백인 경우
kesson

#결측 데이터의 입력2
kesson2 <- read.csv("결측데이터2.csv")#결측 부분이 수치인 경우
kesson2

#인수 na.strings에 의해 결측값을 지정
kesson3 <- read.csv("결측데이터2.csv",na.strings=c(999,9999))#na.strings으로 결측값을 지정
kesson3

#na.omit에 의해 완전한 데이터 행렬을 생성
kanzen <- na.omit(kesson3)
kanzen

#complete.cases에 의해 행을 추출
cind <- complete.cases(kesson3)#NA가 아닌 행번호를 취득한다
cind

#complete.cases의 결과를 이용하여 완전한 데이터를 생성한다
kanzen2 <- kesson3[cind,]#위에 기술한 kanzen과 내용이 일치한다
kanzen2

#함수 sort에 의해 1변수 정렬
score <-c(1,5,2,10,8,2,1,4,3,3)
sort(score,decreasing=FALSE)#decreasing은 생략 가능．TRUE로 내림차순 정렬


#'정렬데이터.csv'의 내용
sdat <- read.csv("정렬데이터.csv")#데이터 입력
sdat

#함수 order에 의해 정렬된 행번호를 취득
posi <- order(sdat$협동성)#함수 order로 협동성을 오름차순으로 정렬할 때의 위치번호를 취득한다
posi
posi2 <- order(sdat$협동성,decreasing=TRUE)#decreasing=TRUE로 내림차순의 행번호를 취득

#1변수에 의한 데이터 행렬의 정렬
sdat[posi,]#오름차순의 경우
sdat[posi2,]#내림차순의 경우

#복수변수에 의한 데이터프레임의 정렬
posi3 <- order(sdat$협동성,sdat$총합)
sdat[posi3,]


#합병하는 데이터
datA <-read.csv("합병데이터A.csv")#행수 8
datB <-read.csv("합병데이터B.csv")#행수 4
datA

#합병
merge(datA,datB,by="ID")#by에는 양 데이터에 공통으로 포함되어 있는 변수명을 지정

#결측값이 있는 행도 남기는 합병
merge(datA, datB, by="ID", all=TRUE)

#치환을 하는 데이터
vec <- c(2,3,4,5,1,2,3,1,2)#테스트 데이터의 작성
tmat <- matrix(vec,ncol=3)
tmat


#함수 which에 의해 치환대상요소의 위치를 취득
loc2 <- which(tmat==2,arr.ind=TRUE)
loc4 <- which(tmat==4,arr.ind=TRUE)
loc2


#수치의 치환
tmatc <- tmat #변환전 데이터 행렬의 사본을 작성한다
tmatc[loc4] <- 2 #tmat에서 4가 있는 위치를 선택하고, 2를 대입
tmatc[loc2] <- 4 #tmat에서 2가 있는 위치를 선택하고, 4를 대입
tmatc

#고정 길이 데이터 입력
itemresp <- readLines("항목반응고정길이.txt")
itemresp

#수치의 위치정보를 작성
spoint <- c(1,seq(7,11,1)) #시작점의 생성
epoint <- c(6,seq(7,11,1)) #끝점의 생성
spoint
epoint

#변수의 작성
raw0 <- sapply(itemresp,substring,spoint,epoint)
raw0

#행렬의 형성
dimnames(raw0) [[2]]  <- 1:5 #뒤에 행이름이 복잡해지므로 정숫값을 부여
raw1 <- t(raw0) #행과 열을 교환
colnames(raw1) <- c("ID",paste("문",1:5,sep="")) #변수명을 부여
raw1

#정오 데이터로 변환
key <- read.csv("key.txt") #정답 키의 입력
key[,1]
binmat <- sweep(raw1[,-1],2,key[,1],FUN="==")*1 #정답을 1, 오답을 0으로 변환
binmat

#작업 디렉토리를 'POS폴더'로 변경
#폴더 안의 파일 표시
fname <- dir()
fname


#ID-POS 데이터의 예
pos0 <- read.csv("201301.csv") #데이터 입력
head(pos0,3)
str(pos0) #변수의 형

#복수의 데이터를 한번에 읽어들임
tmp <- lapply(fname,read.csv,stringsAsFactors=FALSE)

#복수의 데이터프레임을 세로로 연결한다
posall <- do.call(rbind,tmp)#함수 do.call과 rbind를 병용하여 복수의 데이터프레임을 세로로 연결한다

#Factor 형으로 변환
locv <- c("고객ID","점포","상품카테고리")
posall[,locv] <- lapply(posall[,locv],as.factor)

#'고객ID'에 의한 정렬
tmploc <- order(posall$고객ID,posall$구매일,posall$구매시간)
pos <- posall[tmploc,]
head(pos)

#RFM 분석
R <- tapply(posall$구매일,posall$고객ID,max) #고객ID 별로 최신 구매일을 구한다
F <- tapply(posall$고객ID,posall$고객ID,length) #고객ID 별로 총구매횟수를 구한다
M <- tapply(posall$구매금액,posall$고객ID,sum) #고객ID 별로 총구매금액을 구한다
rfm <- data.frame(R=R,F=F,M=M) #R,F,M을 데이터프레임으로 모두 더한다
tmploc2 <- order(rfm$M,rfm$F,rfm$R,decreasing=TRUE) #우선순위를 M > F > R으로 정렬
rfm2 <- rfm[tmploc2,] #정렬한 데이터를 저장
rfm2[1:7,] #상위 20 퍼센트 이내(36명*0.2=7.2명)의 고객을 표시


#다양한 교차 집계표의 산출
t1 <- table(posall$고객ID,posall$상품카테고리) #전체
t2 <- xtabs(~고객ID+상품카테고리+점포,data=posall) #점포별(3점포)
t3 <- xtabs(~고객ID+상품카테고리+구매일,data=posall) #구매일별(336일)

#Factor 형 변환에 의한 교차 집계표
dim(table(posall[,c("고객ID","상품카테고리")])) #데이터 전체의 교차 집계표의 행수와 열수를 구한다
storeA <- subset(posall,점포=="A") #점포A에서의 교차 집계표의 행수와 열수를 구한다
dim(table(storeA[,c("고객ID","상품카테고리")])) #고객ID 별로 월별 총구매금액을 구한다
cid <- posall$고객ID #고객ID의 벡터
buym <- substr(posall$구매일,1,6) #구매월의 벡터
resmat <- tapply(posall$구매금액,list(cid,buym),sum) #함수 tapply의 집단 인수를 리스트 형식으로 제공한다. 여기에서는 고객ID와 구매월을 지정하고 있다
resmat[is.na(resmat)] <- 0 #해당 데이터가 존재하지 않으며 NA 값이 할당되어 있는 부분에 0을 대입한다.
head(resmat,3) 

#함수 getitemname 입력
#벡터 x에 대하여 그 요소가 1 이상인 경우 그 요소명을 반환하는 직접 작성한 자작함수 getitemname을 정의
getitemname <- function(x)
{
	return(names(which(x>=1)))	
}

res2 <- apply(t1,1,getitemname )#자작함수를 고객ID와 상품 카테고리의 교차 집계표에 적용한다
head(res2,2)#처음 2요소를 추출


#내점일을 date형식으로 변환
tmpdate <- paste(substr(posall$구매일,1,4),"-",substr(posall$구매일,5,6),"-",
substr(posall$구매일,7,8),sep="") #함수 as.Date의 인수로 사용하도록 내점일을 문자열로 변환
tmpdate[1:5]
ndate <- as.Date(tmpdate) #문자열을 date 형식으로 변환한다
restime <- tapply(ndate,posall$고객ID,diff) #고객별로 내점간격을 구한다
head(restime,2) #2명분의 내점간격을 표시

#고객ID별로 내점간격의 분포를 그린다
restime2 <- lapply(restime,as.numeric) #리스트의 요소를 수치화해둔다
par(mfrow=c(2,3)) 
lapply(restime2[1:6],hist,breaks=10,xlab="diff",main="",col="grey") #처음 1～6 고객의 히스토그램을 그린다

#고객ID별로 내점간격의 분포를 요약한다
library(psych) #패키지 psych 입력
resd <- lapply(restime2,describe) #패키지 psych의 describe 함수로 요약
resd[1:2]


#연습과 해답
#문1 해답(작업 디렉토리를 'POS폴더2'로 변환)
fname <- dir()
fname2 <- paste(fname,"/2013",sprintf("%02d",1:12),".csv",sep="")
tmpall <- lapply(fname2,read.csv,stringsAsFactors=FALSE)
posall2 <- do.call(rbind,tmpall)
locv <- c("고객ID","점포","상품카테고리")
posall2[,locv] <- lapply(posall2[,locv],as.factor)

#문2 해답
loc2 <- (substr(posall2$구매일,1,6)=="201302")
loc5 <- (substr(posall2$구매일,1,6)=="201305")
pos02 <- posall2[loc2,]
pos05 <- posall2[loc5,]

#문3 해답
store02 <- tapply(pos02$구매금액,list(pos02$고객ID,pos02$점포),sum)
store05 <- tapply(pos05$구매금액,list(pos05$고객ID,pos05$점포),sum)
store02[is.na(store02)] <-0
store05[is.na(store05)] <-0

#문4 해답
dat02 <- data.frame(rownames(store02),store02) #고객ID를 변수로서 포함하는 데이터 프레임을 작성
dat05 <- data.frame(rownames(store05),store05)
colnames(dat02) <- c("고객ID", "2월점포A", "2월점포B","2월점포C") #변수명을 변경
colnames(dat05) <- c("고객ID", "5월점포A", "5월점포B","5월점포C")
mdat <- merge(dat02,dat05,by="고객ID") #고객ID에 의해 두 데이터 셋을 합병

#문5 해답
get200 <- function(x) #200원 이상이었던 월과 점포의 정보를 추출하는 함수의 정의
{		
	loc <- which(x>=200)
	return(names(x[loc]))
}

apply(mdat[,2:7],1,get200) #정의한 자작함수를 이용하여, 고객ID별로 정보를 추출


#문6 해답
ptime <- factor(round(posall2$구매시간),level=seq(9,21,1))
table(posall2$상품카테고리,ptime)

#문7 해답
tmpdate2 <- paste(substr(posall2$구매일,1,4),"-",substr(posall2$구매일,5,6),"-",
substr(posall2$구매일,7,8),sep="") #년월일의 정보를 yyyy-mm-dd 형식으로 변환
ndate2 <- as.Date(tmpdate2) #문자열을 date 형식으로 합병한다
restime3 <- tapply(ndate2,posall2$고객ID,diff) #고객별로 내점간격의 차이를 구하다
restime4 <- lapply(restime3,as.numeric) #리스트의 요소를 수치화해둔다
x50 <- function(x) #벡터의 요소가 50이상인 경우 TRUE를, 아니면 FALSE를 반환하는 함수 x50를 정의
{
	res <- sum(x>=50)
	return(ifelse(res>=1,TRUE,FALSE))
}
sid <- sapply(restime4,x50)
names(sid[sid==TRUE])

#문8 해답(작업 디렉토리를 '제02장'으로 변경)
fmat <- readLines("항목반응고정길이2.txt") #데이터 입력
sp <- c(1,7:106) #시점의 자리수
ep <- c(6,7:106) #종점의 자리수
fmat2 <- sapply(fmat,substring,sp,ep)
dimnames(fmat2)[[2]] <- paste("ID",sprintf("%04d",1:1000),sep="") #행 이름을 변경
fmat3 <- t(fmat2) #행렬의 전치
key2 <- read.csv("key2.txt") #정답 키를 입력
sweep(fmat3[,-1],2,key2[,1],FUN="==")*1 #정오 반응 데이터의 생성
