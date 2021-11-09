#그룹화・그룹의 영향을 검토
#데이터의 입력，데이터프레임의 확인
tmp <- read.csv("점포조사.csv", row.names = 1) #데이터의 입력
head(tmp)

#계층적 군집분석의 실행
tmp_clt <- tmp[, c("밝기", "넓이", "정돈", "청결")]
D0 <- dist(tmp_clt, method = "euclidean")
D<-(1 / 2) * D0 ^ 2
tmp.out <- hclust(d = D, method = "ward.D")
plot(as.dendrogram(tmp.out), ylab = "비유사도", nodePar = list(lab.cex = 0.5, pch = NA), ylim = c(0, 500))

#군집수의 차이를 그리기
cluster3 <- as.factor(cutree(tmp.out, k = 3))
cluster4 <- as.factor(cutree(tmp.out, k = 4))
tmp_clt_resW <- data.frame(tmp_clt, "군집수3" = cluster3, "군집수4" = cluster4, "점포" = row.names(tmp_clt))
library(tidyr)
tmp_clt_resL1 <- gather(tmp_clt_resW, key = 관점, value = 평갓값, -군집수3, -군집수4, -점포)
tmp_clt_resL2 <- gather(tmp_clt_resL1, key = 군집수, value = 군집, -관점, -평갓값, -점포)
library(ggplot2)
P0 <- ggplot(data = tmp_clt_resL2, aes(x = 관점))
P1 <- P0 + geom_line(aes(y = 평갓값, color = 군집, linetype = 군집, group = 점포), stat = "identity")
(P2 <- P1 + facet_wrap( ~ 군집수))

#군집수의 타당성을 확인
CNvalidity <- function(dat, clusters){
	CNvalidity0 <- function(dat, clusters, index = NULL){ #CH, H, KL의 값을 개별적으로 기각하는 함수CNvalidity0
	# index의 지정 영역 처리
	if(!is.element(index, c("CH", "H", "KL"))){
		stop("index의 지정이 올바르지 않습니다")
	}
	N <- ncol(clusters) - 2 #수치를 내는 군집의 수（최초와 최후의 열은 대상 외）
	d <- ncol(dat) #변수의 수
	value <- data.frame(cluster.n = names(clusters)[2:(ncol(clusters)-1)], numeric(N)) #지표 값을 담기 위한 객체
	colnames(value)[2] <- index #지표 이름(CH, H, KL 중 어느 것인가)을 벡터의 열 이름에 부여한다.
	W <- function(X){ #제곱합 곱합 행렬을 반환하는 함수W
		X <- as.matrix(X)
		V <- X-(rep(1, nrow(X)) %*% t(colMeans(X))) #평균편차화 데이터
		W <- t(V) %*% V
		return(W)
	}
	from <- as.numeric(names(clusters)[2]) #지표의 값을 산출하는 최초의 군집수
	to <- as.numeric(names(clusters)[(ncol(clusters) - 1)]) #지표의 값을 산출하는 최후의 군집수
	for(i in from:to){　#i는 지표를 산출하는 군집수
		# dat0-dat에 군집수 i일 때의 군집, 군집수 i+1일 때의 군집, 군집수 i-1일 때의 군집을 열로 추가한 데이터프레임
		dat0 <- data.frame(dat, cluster1 = factor(clusters[, names(clusters) == as.character(i)]), 
			cluster2 = factor(clusters[, names(clusters) == as.character(i + 1)]), 
			cluster3 = factor(clusters[, names(clusters) == as.character(i - 1)]))
		Ws1 <- by(data = dat0[, 1:d], INDICES = dat0$cluster1, FUN = W) #군집수 i일 때의 각 군집의 제곱합 곱합 행렬의 리스트
		Ww1 <- Reduce(f = "+", Ws1) #군집수 i일 때의 군내 제곱합 곱합 행렬(각 군집의 제곱합 곱합 행렬의 합)
		if(index == "CH"){
			indexname <- "Calinski & Harabasz index"
			Z  <- Reduce(f = rbind, by(data = dat0[, 1:d],INDICES = dat0$cluster1, FUN = colMeans)) #군집수 i일 때의 각 군집 평균을 각 행에 가진 행렬(i×d)
			ZZ <- Z - rep(1, i) %*% t(colMeans(dat0[, 1:d])) #Z로부터 전체평균을 뺀 행렬(i×d)
			Nc <- diag(table(dat0$cluster1))#군집수 i일 때의 각 군집의 대상수를 대각요소로 하는 대각행렬
			Wb <- t(ZZ) %*% Nc %*% ZZ #군집수 i일 때의 군간 제곱합 곱합 행렬
			indexvalue <- (sum(diag((Wb))) / (i - 1))/(sum(diag((Ww1))) / (nrow(dat0) - i)) #CH의 값
		}else if(index == "H"){
			indexname <- "Hartigan index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W) #군집수 i+1일 때의 각 군집의 제곱합 곱합 행렬의 리스트
			Ww2 <- Reduce(f = "+", Ws2) #군집수 i+1일 때의 군내 제곱합 곱합 행렬(각 군집의 제곱합 곱합 행렬의 합)
			indexvalue <- (sum(diag((Ww1))) / sum(diag((Ww2))) - 1) * (nrow(dat0) - i - 1) #H의 값
			if(i == from){ #diffH를 산출하기 위한 처리（최초 군집수-1일 때 H의 값）
				Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)#군집수 i-1일 때의 각 군집의 제곱합 곱합 행렬의 리스트
				Ww3 <- Reduce(f = "+", Ws3) #군집수 i-1일 때의 군내 제곱합 곱합 행렬(각 군집의 제곱합 곱합 행렬의 합)		
				indexvalue_sub <- (sum(diag((Ww3))) / sum(diag((Ww1))) - 1 ) * (nrow(dat0) - (i - 1) - 1) #diffH의 값
			}
		}else if(index == "KL"){
			indexname <- "Krzanowski & Lai index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W)　#군집수 i+1일 때 각 군집의 제곱합 곱합 행렬의 리스트
			Ww2 <- Reduce(f = "+", Ws2) #군집수 i+1일 때의 군내 제곱합 곱합 행렬(각 군집의 제곱합 곱합 행렬의 합)
			Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)　#군집수 i-1일 때 각 군의 제곱합 곱합 행렬의 리스트
			Ww3 <- Reduce(f = "+", Ws3) #군집수 i-1일 때의 군내 제곱합 곱합 행렬(각 군집의 제곱합 곱합 행렬의 합)
			DIFF1 <- sum(diag((Ww3))) * (i - 1) ^ (2 / d) - sum(diag((Ww1))) * (i) ^ (2 / d) #KL의 분자
			DIFF2 <- sum(diag((Ww1))) * (i) ^ (2 / d) - sum(diag((Ww2))) * (i + 1) ^ (2 / d) #KL의 분모
			indexvalue <- abs(DIFF1 / DIFF2) #KL의 값
		}		
		value[value[, "cluster.n"] == as.character(i), 2] <- indexvalue #지표 값의 대입
	}

	if(index == "H"){
		#value-H의 값과 diffH의 값을 각 열로 하는 객체
		value <- data.frame(value, diffH = c(-1 * diff(c(indexvalue_sub, value[, index]))))
	}
	return(value)
	}
	#함수 CNvalidity0을 이용한 각 지표 값의 산출과 총합
	CHindex <- CNvalidity0(dat = dat, clusters = clusters, index = "CH") 
	 Hindex <- CNvalidity0(dat = dat, clusters = clusters, index = "H") 
	KLindex <- CNvalidity0(dat = dat, clusters = clusters, index = "KL")
	indices <- merge(merge(CHindex, Hindex, by = "cluster.n", sort = FALSE), KLindex, by = "cluster.n", sort = FALSE)
	return(indices)
}
from <- 1; to <- 11
clabel <- function(x){factor(cutree(tmp.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = tmp_clt, clusters = clusters)

#1요인의 분산분석（전체）
tmp_aov <- data.frame(tmp, "군집" = cluster4)
tapply(tmp_aov[, "체류시간"], INDEX = tmp_aov[, "군집"], FUN = mean)
tmp_aov.out <- aov(formula = 체류시간 ~ 군집, data = tmp_aov)
summary(tmp_aov.out)

#1요인의 분산분석（다중비교）
TukeyHSD(tmp_aov.out)

#척도점수화・척도점수를 이용한 설명
#데이터의 입력，데이터프레임의 확인
kbs  <-  read.csv("경마조사.csv") #데이터의 입력
head(kbs)

#데이터의 수정과 인자수의 검토
kbs_fa  <-  kbs[, 1:20]
library(psych)
VSS.scree(kbs_fa)
eigen(cor(kbs_fa))$values
fa.parallel(kbs_fa, fm = "ml", fa = "pc", n.iter = 100)

#탐색적 인자분석의 결과
library(GPArotation)
kbs_fa.out  <-  fa(kbs_fa, nfactors = 2, fm = "ml", rotate = "promax")
print(kbs_fa.out, sort = TRUE, digits = 3)

#α계수의 산출
kbs_S1 <- kbs_fa[, 1:10]
alpha(kbs_S1)
kbs_S2 <- kbs_fa[, -1 * c(1:10, 18, 19)] #1부터 10, 18, 19열 이하를 추출
alpha(kbs_S2)

#척도점수의 산출
S1 <- rowSums(kbs_S1)
S2 <- rowSums(kbs_S2)

#계층적 회귀분석용의 데이터프레임 작성
kbs_hmr  <-  data.frame(kbs[, c("월요병", "수지", "성별", "연령")], S1, S2)
head(kbs_hmr)

#계층적 회귀분석의 실행（분산설명률의 증분을 검정）
M1 <- lm(월요병 ~ 성별 + 연령 + 수지, data = kbs_hmr)
(M1_R2 <- summary(M1)$r.squared)
M2 <- lm(월요병 ~ 성별 + 연령 + 수지 + S1 + S2, data = kbs_hmr)
(M2_R2 <- summary(M2)$r.squared)
M2_R2 - M1_R2 #분산설명률의 증분
anova(M1, M2)

#계층적 다중회귀분석의 실행（AIC의 산출）
extractAIC(M1)
extractAIC(M2)

#투입 후의 다중회귀분석 결과
summary(M2)
confint(M2,level = 0.95)



#측정상황의 확인・다변수간의 관계를 검토
#데이터의 입력，데이터프레임의 확인
dsk <- read.csv("남학교조사.csv") #데이터의 입력
head(dsk)

#확인적 인자분석 모형의 추정
dsk_model_cfa <- "
정서 =~ 정서1 + 정서2 + 정서3 + 정서4 + 정서5
폭력 =~ 폭력1 + 폭력2 + 폭력3 + 폭력4 + 폭력5
"
library(lavaan)
dsk.out_cfa <- cfa(model = dsk_model_cfa, data = dsk)
summary(dsk.out_cfa, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

#검토모형의 모형 기술
dsk_model_path <- "
학업 =~ 1 * 학업1 + 학업2 + 학업3 + 학업4 + 학업5
친구 =~ 1 * 친구1 + 친구2 + 친구3 + 친구4 + 친구5
정서 =~ 1 * 정서1 + 정서2 + 정서3 + 정서4 + 정서5
폭력 =~ 1 * 폭력1 + 폭력2 + 폭력3 + 폭력4 + 폭력5
정서 ~ 학업 + 친구 + BMI
폭력 ~ 학업 + 친구 + BMI
학업 ~~ 친구 + BMI
친구 ~~ BMI
정서 ~~ 폭력
"

#검토모형의 모형 추정결과
dsk.out_path <- lavaan(model = dsk_model_path, data = dsk, auto.var = TRUE)
summary(dsk.out_path, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

