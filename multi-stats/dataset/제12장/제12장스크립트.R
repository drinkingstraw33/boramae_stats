#계층적 군집분석
#데이터의 입력, 데이터프레임의 확인
ysi <- read.csv("야채의 영양.csv", row.names = 1) #데이터의 입력
head(ysi)

#비유사도 행렬의 산출
D0 <- dist(ysi, method = "euclidean")
D <- (1/2) * D0 ^ 2

#덴드로그램 작성을 위한 사전 처리
ysi.out <- hclust(d = D, method="ward.D")

#덴드로그램의 작성
plot(as.dendrogram(ysi.out), xlim = c(300000000, 0), xlab = "비유사도", horiz = TRUE)
plot(as.dendrogram(ysi.out), ylim = c(0, 12000), ylab = "비유사도", horiz = FALSE, nodePar = list(lab.cex = 0.8, angle = 180, pch = NA))

#군집수의 타당성을 확인
CNvalidity <- function(dat, clusters){
	CNvalidity0 <- function(dat, clusters, index = NULL){ #CH, H, KL의 값을 개별적으로 반환하는 함수 CNvalidity0
	# index의 지정 오류 처리
	if(!is.element(index, c("CH", "H", "KL"))){
		stop("index의 지정이 바르지 않습니다")
	}
	N <- ncol(clusters) - 2 #수치를 내는 클러스터의 수(처음과 끝 열은 대상 제외)
	d <- ncol(dat) #변수의 수
	value <- data.frame(cluster.n = names(clusters)[2:(ncol(clusters)-1)], numeric(N)) #지표 값을 모으기 위한 객체
	colnames(value)[2] <- index #지표 이름(CH, H, KL 중 어느 하나)을 벡터의 열 이름에 부여한다
	W <- function(X){ #제곱합 합제곱 행렬을 반환하는 함수W
		X <- as.matrix(X)
		V <- X-(rep(1, nrow(X)) %*% t(colMeans(X))) #평균편차화 데이터
		W <- t(V) %*% V
		return(W)
	}
	from <- as.numeric(names(clusters)[2]) #지표 값을 산출하는 최초의 클러스터 수
	to <- as.numeric(names(clusters)[(ncol(clusters) - 1)]) #지표 값을 산출하는 최후의 클러스터 수
	for(i in from:to){　#i는 지표를 산출하는 클러스터 수
		# dat0-dat에 클러스터 수 i일 때의 클러스터, 클러스터 수 i+1일 때의 클러스터, 클러스터 수 i-1일 때의 클러스터를 열로 추가한 데이터 프레임
		dat0 <- data.frame(dat, cluster1 = factor(clusters[, names(clusters) == as.character(i)]), 
			cluster2 = factor(clusters[, names(clusters) == as.character(i + 1)]), 
			cluster3 = factor(clusters[, names(clusters) == as.character(i - 1)]))
		Ws1 <- by(data = dat0[, 1:d], INDICES = dat0$cluster1, FUN = W) #클러스터 수 i일 때의 각 클러스터의 제곱합 곱합 행렬의 리스트
		Ww1 <- Reduce(f = "+", Ws1) #클러스터 수 i일 때의 군내 제곱합 곱제곱 행렬(각 클러스터의 제곱합 곱제곱 행렬의 합
		if(index == "CH"){
			indexname <- "Calinski & Harabasz index"
			Z  <- Reduce(f = rbind, by(data = dat0[, 1:d],INDICES = dat0$cluster1, FUN = colMeans)) #클러스터 수 i일 때의 각 클러스터 평균을 각 행에 가지는 행렬(i×d)
			ZZ <- Z - rep(1, i) %*% t(colMeans(dat0[, 1:d])) #Z에서 전체 평균을 뺀 행렬(i×d)
			Nc <- diag(table(dat0$cluster1))#클러스터 수 i일 때의 각 클러스터 대상수를 대각요소로 하는 대각행렬
			Wb <- t(ZZ) %*% Nc %*% ZZ #클러스터 수 i일 때의 군간 제곱합 곱합 행렬
			indexvalue <- (sum(diag((Wb))) / (i - 1))/(sum(diag((Ww1))) / (nrow(dat0) - i)) #CHの値
		}else if(index == "H"){
			indexname <- "Hartigan index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W) #클러스터 수 i+1 때 각 클러스터의 제곱화 곱합 행렬 리스트
			Ww2 <- Reduce(f = "+", Ws2) #클러스터 수 i+1일 때의 군내 제곱합 곱합 행렬(각 클러스터의 제곱합 곱합 행렬의 합)
			indexvalue <- (sum(diag((Ww1))) / sum(diag((Ww2))) - 1) * (nrow(dat0) - i - 1) #H의 값
			if(i == from){ #diffH를 산출하기 위한 처리(최초 클러스터 수-1 때의 H 값)
				Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)#클러스터 수 i-1일 때의 각 클러스터의 제곱합 곱합 행렬 리스트
				Ww3 <- Reduce(f = "+", Ws3) #클러스터 수 i-1일 때의 군내 제곱합 곱합 행렬(각 클러스터의 제곱합 곱합 행렬의 합)		
				indexvalue_sub <- (sum(diag((Ww3))) / sum(diag((Ww1))) - 1 ) * (nrow(dat0) - (i - 1) - 1) #diffH의 값
			}
		}else if(index == "KL"){
			indexname <- "Krzanowski & Lai index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W)　#클러스터 수 i+1일 때의 각 클러스터의 제곱합 곱합 행렬 리스트
			Ww2 <- Reduce(f = "+", Ws2) #클러스터 수 i+1일 때의 군내 제곱합 곱합 행렬(각 클러스터의 제곱합 곱합 행렬의 합)
			Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)　#클러스터 수 i-1일 때의 각 클러스터의 제곱합 곱합 행렬 리스트
			Ww3 <- Reduce(f = "+", Ws3) #클러스터 수 i-1일 때의 군내 제곱합 곱합 행렬(각 클러스터의 제곱합 곱합 행렬의 합)
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
clabel <- function(x){factor(cutree(ysi.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
head(clusters)
CNvalidity(dat = ysi, clusters = clusters)

#할당된 군집 확인
(cluster <- factor(cutree(ysi.out, k = 3))) #3개의 군집으로 분류

#각 군집의 평균
by(ysi, INDICES = cluster, FUN = function(x){apply(x, 2, mean)}) #FUN에 자작함수를 지정하여, 한 번에 출력

#z점수화 데이터의 분석（덴드로그램의 작성까지）
ysi.stdz <- scale(ysi)
D0.stdz <- dist(ysi.stdz, method = "euclidean")
D.stdz <- (1 / 2) * D0.stdz ^ 2
ysi.stdz.out <- hclust(d = D.stdz, method = "ward.D")#Ward-input은 유클리드 제곱 거리
plot(as.dendrogram(ysi.stdz.out), xlim = c(120, 0), xlab = "비유사도", horiz = TRUE)


#비계층적 군집분석
#군집의 형성과 성과의 확인
INTP.KM <- function(dat, ncluster){ #군집 수에 따라 각 군집의 초기값이 되는 값을 반환하는 함수
	M <- nrow(dat) #전체의 대상수
	center.all <- apply(dat, 2, mean) #전체평균
	DVATmat <- as.matrix((dat - rep(1, M) %*% t(center.all))) #평균편차화 데이터
	DTC0 <- diag(DVATmat %*% t(DVATmat)) #각 대상과 전체평균과의 제곱 유클리드 거리
	INTP <- matrix(0, ncluster, ncol(dat)) #각 행에 각 군집의 초기 값을 담기 위한 객체
	dat.ordered <- dat[order(DTC0), ] #제곱 유클리드 거리 순으로 원래의 데이터프레임 정렬
	for(i in 1:ncluster){
		INTP[i, ]<-unlist(dat.ordered[1 + (i - 1) * floor(M / ncluster), , drop = FALSE]) #초기값이 되는 대상의 관측값 대입
	}
	return(INTP)
}
INTP <- INTP.KM(dat = ysi, ncluster = 3)
(ysi.out2 <- kmeans(x = ysi, centers = INTP))

#군집수의 타당성을 확인
from <- 1; to <- 11
clabel <- function(x){factor(kmeans(x = ysi, centers = INTP.KM(dat = ysi, ncluster = x))$cluster)}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = ysi, clusters = clusters)

#z점수화 데이터 분석（군집의 형성과 결과의 확인）
INTP.stdz <- INTP.KM(dat = ysi.stdz, ncluster = 3)
(ysi.stdz.out2 <- kmeans(x = ysi.stdz, centers = INTP.stdz))


#문제와 해답
#문1 해답
tsks <- read.csv("도시의 기상.csv", row.names = 1)

#문2 해답
tsks.stdz <- scale(tsks)

#문3 해답
D0.stdz <- dist(tsks.stdz, method = "euclidean")
D.stdz<-(1 / 2) * D0.stdz ^ 2
tsks.stdz.out <- hclust(d = D.stdz, method = "ward.D")
plot(as.dendrogram(tsks.stdz.out), xlim = c(100, 0), xlab = "비유사도", horiz = TRUE)

#문4 해답
from <- 1; to <- 11
clabel <- function(x){factor(cutree(tsks.stdz.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = tsks.stdz, clusters = clusters)

#문5 해답
(cluster <- factor(cutree(tsks.stdz.out, k = 5))) #5개의 군집으로 분류
by(tsks.stdz, INDICES = cluster, FUN = function(x){apply(x, 2, mean)}) #FUN에 자작함수를 지정하여, 한 번에 출력

