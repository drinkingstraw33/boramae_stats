#데이터 입력
jhk <- read.csv("인사평가결과.csv") #데이터 입력

#데이터 확인
dim(jhk) #순서1. dim을 이용한 다변량 데이터의 행수 열수 확인
colnames(jhk) #순서2. colnames을 이용한 변수명 확인
head(jhk,3) #순서3. head을 이용한 첫 세 줄 표시

#히스토그램 그리기
library(lattice)#패키지 lattice 입력
histogram(~스트레스,data=jhk,breaks=20,type="count")

#대푯값의 산출
mean(jhk$스트레스) #스트레스의 평균값
median(jhk$스트레스) #스트레스의 중앙값　
sort(table(jhk$연령)) #연령의 최빈값

#산포도의 산출
sd(jhk$스트레스) #스트레스의 SD
var(jhk$스트레스) #스트레스의 분산

#중앙값으로부터 평균편차의 산출
mean(abs(jhk$스트레스-median(jhk$스트레스)))

#집단별 히스토그램
histogram(~협동성|연령+성별,data=jhk,breaks=15)

#집단 사이의 분포 비교
tapply(jhk$협동성,jhk$성별,mean) #성별 '협동성'의 평균값을 구한다
tapply(jhk$협동성,jhk$성별,sd) #성별 '협동성'의 SD를 구한다


#상자수염도 
boxplot(jhk$기능,horizontal=TRUE) #'기능'의 상자수염도를 가로 방향으로 작성한다(horizontal=FALSE는 세로 방향)
boxplot(협동성~성별,data=jhk,horizontal=TRUE) #성별로 '협동성'의 상자수염도를 그린다


#사분위수를 포함한 요약통계량의 산출
summary(jhk$기능)


#등분산 F검정
var.test(협동성~성별,data=jhk)

#독립 표본 t검정（등분산 가정）
t.test(협동성~성별,data=jhk,var.equal=TRUE)

#Welch법에 의한 t검정
t.test(협동성~성별,data=jhk)

#대응 표본 t검정
score <- c(jhk$총합,jhk$작년총합)
year <- c(rep("금년",800),rep("작년",800))
t.test(score~year,paired=TRUE)

#집단별로 신뢰구간 그리기
library(gplots)#패키지 gplots 입력
plotmeans(협동성~성별,data=jhk,p=0.95,ylim=c(49,54))


#신뢰구간 산출
t.test(jhk$협동성[jhk$성별=="F"])
t.test(jhk$협동성[jhk$성별=="M"])


#열(변수에 대한 기초집계)
varname <- c("협동성","자기주장","기능","지식")
jhk2 <- jhk[,varname] #데이터프레임으로부터 4변수를 추출한다
apply(jhk2,2,mean) #변수별 평균값을 구한다
apply(jhk2,2,sd) #변수별 SD를 구한다

#행(관측대상에 대한 기초집계)
apply(jhk2,1,sum) #사월별로 4변수의 점수합을 구한다
apply(jhk2,1,sd) #사원별로 4변수의 SD를 구한다

#다변수 분포를 집단간 비교
by(jhk2,jhk$성별,apply,2,mean) #4변수의 평균 산출
by(jhk2,jhk$성별,apply,2,sd) #4변수의 SD 산출

#표준화 절차
zscore <- scale(jhk2) #z점수 산출
head(zscore,2)
tscore <- zscore*10 + 50 #편차값 산출
head(tscore,2)

#산점도 그리기
gino <- jhk$기능
chisiki <- jhk$지식
plot(gino,chisiki,xlab="기능",ylab="지식")

#산점도행렬 그리기
kjs <- c("협동성","자기주장","스트레스")
plot(jhk[,kjs])

#층별 산점도 그리기
library(lattice)　#패키지lattice 입력
xyplot(지식~기능|연령+부서,data=jhk)


#상관계수의 산출
cor(jhk$협동성,jhk$스트레스) #2변수간 상관계수를 구한다

#상관행렬의 산출
cor(jhk[,kjs])　#cor에 다변수를 입력하여 상관행렬을 구한다

#공분산행렬의 산출
cov(jhk[,kjs])

#상관계수의 검정
library(psych)　#패키지 psych 설치
corkekka <- corr.test(jhk[,kjs])　
corkekka$t #t값의 산출
corkekka$p #p값의 산출


#교차 집계표 작성
(cross <- table(jhk$부서,jhk$연령))


#교차 집계표를 비율 표기
prop.table(cross)　#전체 도수를 기준으로 한 비율 표기
prop.table(cross,1)　#행 방향의 비율 표기
prop.table(cross,2)　#열 방향의 비율 표기


#층별 교차 집계표의 작성
xtabs(~부서+연령+성별,data=jhk)


#연관계수의 산출
library(vcd)　#패키지 vcd 입력
assocstats(cross)　#부서와 연령의 연관계수 산출

#교차 집계표와 연관계수의 관계
(m1 <- matrix(c(50,0,0,50),ncol=2))　#완전연관인 경우
assocstats(m1)　#완전연관 교차 집계표에서의 연관계수
(m2 <- matrix(c(10,20,100,200),ncol=2))　#독립인 경우
assocstats(m2)　#독립된 교차 집계표에서의 연관계수


#교차 집계표에 대한 카이제곱 검정
(reschisq <- chisq.test(cross))

#잔차분석
reschisq$stdres


#편상관계수의 산출
sixname <- c("협동성","자기주장","기능","지식","총합","작년총합")
jhk3 <- jhk[,sixname]
cor(jhk3[,5],jhk3[,6]) #총합，작년총합의 상관계수를 구한다
partial.r(jhk3,c(5,6),c(1,2,3,4)) #협동성，자기주장，기능，지식을 통제한 편상관계수를 구한다


#순서 범주형 변수를 포함한 데이터프레임 작성
(sogoc <- c(-Inf,mean(jhk$총합),Inf))　#2값 범주형 변수화를 위한 계급폭 작성
(scat <- cut(jhk$총합,breaks=sogoc,right=FALSE,labels=c(0,1))) #계급폭을 이용하여 데이터를 0과 1로 변환한다
(ginoc <- c(-Inf,summary(jhk$기능)[c(2,5)],Inf))　#다중값 범주형 변수화를 위한 계급폭 작성
(gcat <- cut(jhk$기능,breaks=ginoc,right=FALSE,labels=c(0,1,2)))　#계급폭을 이용하여 데이터를 1과 2로 변환한다

#순서 범주형 변수를 포함한 상관행렬의 산출
library(polycor)　#패키지 polycor 입력
jhk4 <- data.frame(총합범주=scat,기능범주=gcat,지식=jhk$지식)　#양적변수 '작년총합'도 포함하여 데이터프레임을 작성한다
hetcor(jhk4,ML=TRUE) #최우법으로 상관행렬 산출


#함수 effectd1 입력
effectd1 <- function(x1,x2,clevel=0.95)
{
    library(MBESS)
    #각 집단의 표본 크기 산출
    n1 <- length(x1);n2 <- length(x2)
    #각 집단의 평균 산출
    m1 <- mean(x1);m2 <- mean(x2)
    #각 집단의 표본표준편차 산출
    s1 <- sqrt(mean((x1-m1)^2))
    s2 <- sqrt(mean((x2-m2)^2))
    #모표준편차의 추정값 산출
    sast <- sqrt(((n1*s1^2)+(n2*s2^2))/(n1+n2-2))
    #효과크기의 산출
    d <- (m1-m2)/sast
    #독립표본 ｔ검정 실행(등분산 가정)과 자유도 산출
    rest <- t.test(x1,x2,paired=FALSE,var.equal=TRUE)
    #효과크기의 신뢰구간 산출
    resconf <- conf.limits.nct(t.value=rest$statistic,
    df=rest$parameter,conf.level=clevel)
    ll <- resconf$Lower.Limit*sqrt((n1+n2)/(n1*n2))
    ul <- resconf$Upper.Limit*sqrt((n1+n2)/(n1*n2))
    u3 <- pnorm(d,0,1)
    return(list=c(효과크기=d,신뢰수준=clevel,구간하한=ll,
    구간상한=ul,U3=u3))
}

#독립표본 t검정에 대응하는 효과크기 산출
#사전에 함수 effectd1을 R로 읽어들인다
fdat <- jhk$협동성[jhk$성별=="F"]
mdat <- jhk$협동성[jhk$성별=="M"]
effectd1(fdat, mdat, clevel=0.95)

#함수 effectd2 입력
#이 함수는 결측값에 대응하지 않는다.
effectd2 <- function(x1,x2,clevel=0.95)
{
    library(MBESS)
    #표본 크기의 산출
    n <- length(x1-x2)
    #차이의 평균 v.bar 산출
    v.bar <- mean(x1-x2)
    #차이의 불편분산의 제곱근 sv 산출
    sv.p <- sd(x1-x2)
    #효과크기의 산출
    d.p <- v.bar/sv.p
    #대응비교 ｔ검정의 실행과 자유도 산출
    rest <- t.test(x1,x2,paired=TRUE)
    #효과크기의 신뢰구간 산출
    resconf <- conf.limits.nct(t.value=rest$statistic,
    df=rest$parameter,conf.level=clevel)
    ll <- resconf$Lower.Limit/sqrt(n)
    ul <- resconf$Upper.Limit/sqrt(n)
    u3 <- pnorm(d.p,0,1)
    return(list=c(효과크기=d.p,신뢰수준=clevel,구간하한=ll,
    구간상한=ul,U3=u3))
}

#대응비교 t검정에서의 효과크기 산출
effectd2(jhk$총합,jhk$작년총합,clevel=0.95)


#상관계수의 신뢰구간 산출
corkekka2 <- corr.test(jhk[,kjs],alpha=0.05) 
print(corkekka2,short=FALSE)


effectv <- function(x,y,clevel=0.95)
{
    library(vcd)
    library(MBESS)
    #교차 집계표의 산출
    tmpcross <- table(x,y)
    #표본 크기의 산출
    n <- sum(tmpcross)
    #집계표의 행수와 열수를 산출
    size <- dim(tmpcross)
    #자유도를 산출
    dof <- prod(size-1)	
    #카이제곱값과 크래머 계수 V의 산출
    resas <- assocstats(tmpcross)
    chi <- resas$chisq_tests["Pearson","X^2"]	
    v <- resas$cramer
    #카이제곱값이 주어졌을 때의 비중심도 상한값，하한값을 산출
    resconf <- conf.limits.nc.chisq(Chi.Square=chi,
    df=dof,conf.level=clevel)
    
    if(resconf$Lower.Limit>0)#하한값이 0을 넘어서는 영역에 들어갔을 경우
    {
        #신뢰구간의 하한・상한 산출  
        ll <- sqrt((dof+resconf$Lower.Limit)/((min(size)-1)*n))
        ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
        return(list=c(효과크기V=v,카이제곱값=chi,신뢰수준=clevel,
        구간하한=ll,구간상한=ul))
    }else if(resconf$Lower.Limit==0) #하한값이 음수가 되었을 경우
    {
        #신뢰구간의 하한을 0으로 제약한 후 상한을 산출       
        resconf <- conf.limits.nc.chisq(Chi.Square=chi,
        df=dof,conf.level=NULL,alpha.lower=0,alpha.upper=(1-clevel)/2)
        ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
        return(list=list(
        "하한값이 음수가 되었으므로 신뢰구간의 하한값을 0으로 했습니다.",
        c(효과크기V=v,카이제곱값=chi,신뢰수준=clevel,구간하한=0,
        구간상한=ul)))
    }
}

#크래머 계수 V에 대한 신뢰구간 산출
effectv(jhk$연령,jhk$부서,clevel=.95)


#연습과 해답
#문1 해답
mat <- read.csv("학력조사결과.csv")
library(lattice)
histogram(~사전점수|동아리,data=mat)
boxplot(사전점수~동아리,data=mat,horizontal=TRUE)

#문2 해답
tapply(mat$사전점수,mat$동아리,mean)
tapply(mat$사전점수,mat$동아리,median)
tapply(mat$사전점수,mat$동아리,sd)

#문3 해답
m <- mat$수학[mat$성별=="M"]
f <- mat$수학[mat$성별=="F"]
t.test(m,f,var.equal=TRUE)
effectd1(m,f,clevel=0.95)

#문4 해답
goukei <- apply(mat[,c("사전점수","사후점수")],1,sum)

#문5 해답
spre1 <- scale(mat$사전점수)
plot(mat$사전점수,spre1)
cor(mat$사전점수,spre1)

#문6 해답
library(psych)
matc <- mat[,c("사전점수","사후점수","국어","사회","영어")]
partial.r(matc,c(3:5),c(1:2))

#문7 해답
library(polycor)
kcat <- cut(mat$국어,breaks=c(-Inf,mean(mat$국어),Inf),right=FALSE,labels=c(0,1))
scat <- cut(mat$사회,breaks=c(-Inf,mean(mat$사회),Inf),right=FALSE,labels=c(0,1))
ecat <- cut(mat$영어,breaks=c(-Inf,mean(mat$영어),Inf),right=FALSE,labels=c(0,1))
mat2 <- data.frame(kcat,scat,ecat)
hetcor(mat2,ML=TRUE)

#문8 해답
effectv(mat$성별,mat$동아리,clevel=0.95)
