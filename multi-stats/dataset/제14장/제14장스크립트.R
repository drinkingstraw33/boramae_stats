#데이터의 입력，데이터프레임의 확인
ks <- read.csv("9도시의 기상.csv", colClasses = c(rep("factor", 6), rep("numeric", 3))) #데이터의 입력
head(ks)

#범주형 변수의 수준을 확인
str(ks)

#범주형 변수의 수준을 설정
ks$도시 <- factor(ks$도시, levels = c("삿포로", "센다이", "도쿄", "니가타", "나고야", "오사카", "히로시마", "후쿠오카", "나하"))
ks$년 <- factor(ks$년, levels = as.character(2012:2016))
ks$월 <- factor(ks$월, levels = as.character(1:12))
ks$일 <- factor(ks$일, levels = as.character(1:31))
ks$계절 <- factor(ks$계절, levels = c("봄", "여름", "가을", "겨울"))
ks$날씨 <- factor(ks$날씨, levels = c("쾌청", "맑음", "다소흐림", "흐림", "옅은안개", "안개", "안개비", "비", "진눈깨비", "눈", "우박", "천둥번개"))

#막대그래프 그리기
library(ggplot2)
P1_0 <- ggplot(data = ks, mapping = aes(x = 날씨))
(P1_1 <- P1_0 + geom_bar())

#막대에 대응하는 값을 취득
ggplot_build(P1_1)$data[[1]]

#막대그래프 그리기（도시별）
(P1_2 <- P1_1 + facet_wrap( ~ 도시, ncol = 3))

#막대그래프 그리기（도시와 계절별）
(P1_3 <- P1_1 + facet_grid(도시 ~ 계절))

#히스토그램 그리기
P2_0 <- ggplot(data = ks,mapping = aes(x = 풍속))
(P2_1 <- P2_0 + geom_histogram(breaks = seq(0, 25, 0.5)))

#히스토그램 그리기（도시별）
(P2_2 <- P2_1 + facet_wrap( ~ 도시, ncol = 3))

#분포의 평균과 불편분산에 기초한 표준편차（도시별）
library(dplyr)
wind_MeanSD_city <- summarise(group_by(ks, 도시), Mean = mean(풍속), SD = sd(풍속))
print(wind_MeanSD_city)

#히스트그램 그리기 및 분포의 평균과 불편분산에 기초한 표준편차（도시와 계절별）
(P2_3 <- P2_1 + facet_grid(도시 ~ 계절))
wind_MeanSD_cityseason <- summarise(group_by(ks, 도시, 계절), Mean = mean(풍속), SD = sd(풍속))
print(wind_MeanSD_cityseason, n = 50)

#비만 포함한 데이터프레임 작성
ks_rain <- filter(ks, 날씨 == "비")

#1월부터 12월까지 비가 내린 날의 수를 꺾은 선 그래프로 그리기
P3_0 <- ggplot(data = ks_rain, mapping = aes(x = 월))
P3_1 <- P3_0 + geom_line(aes(group = 1), stat = "count")
(P3_2 <- P3_1 + geom_point(aes(group = 1), stat = "count"))

#1월부터 12월까지 비가 내린 날의 수를 꺾은 선 그래프로 그리기（도시별）
(P3_3 <- P3_2 + facet_grid(. ~ 도시))

#1월부터 12월까지 평균강수량의 꺾은 선 그래프 그리기
P3_3 <- ggplot(data = ks, mapping = aes(x = 월, y = 강수량))
P3_4 <- P3_3 + stat_summary(aes(group = 1), fun = mean, geom = "line")
(P3_5 <- P3_4 + stat_summary(aes(group = 1), fun = mean, geom = "point"))

#1월부터 12월까지 평균강수량의 꺾은 선 그래프 그리기（도시별）
(P3_6 <- P3_5 + facet_grid(. ~ 도시))

#2년치의 기온 데이터로 조정
library(tidyr)
fixname <- function(x){ #변수명 문자열의 인코딩을 원래대로 원래대로 되돌리는 함수
	names(x)<-enc2native(names(x)) #데이터 객체 변수명의 인코딩 재조정
	return(x)
}
ks_temp <- 
	ks %>% 
	filter(년 == "2014" | 년 == "2015") %>% #조건에 합치하는 대상을 발췌
	select(도시, 월, 일, 년, 계절, 기온) %>% #특정 변수의 추출
	spread(key = 년, value = 기온, sep = "") %>% #와이드 포맷으로 변경
	rename(기온2014 = 년2014, 기온2015 = 년2015) %>% #변수명을 다시 붙임
	fixname() #데이터 객체에 대해 함수 group_by 적용 시 오류에 대처하기 위해 필요（Windows OS）

#산점도 그리기
P4_0 <- ggplot(data = ks_temp, mapping = aes(x = 기온2014, y = 기온2015))
(P4_1 <- P4_0 + geom_point())

#산점도 그리기（도시별）
(P4_2 <- P4_1 + facet_wrap( ~ 도시))

#산점도 그리기（도시와 계절별）
(P4_3 <- P4_1 + facet_grid(도시 ~ 계절))

#상관계수（도시와 계절별）
ks_temp1415 <- summarize(group_by(ks_temp, 도시, 계절), Cor = cor(기온2014, 기온2015))
print(ks_temp1415, n = 50)

#색칠을 위한 변수 매핑
P5_0 <- ggplot(data = ks, mapping = aes(x = 날씨))
(P5_1 <- P5_0 + geom_bar(aes(fill = 계절), position = "stack"))
# (P5_1 <- P5_0 + geom_bar(aes(fill = 계절), position = position_stack(reverse = TRUE))) #역순
# (P5_1 <- P5_0 + geom_bar(aes(fill = 계절, group = factor(계절, levels = c("가을", "겨울", "봄", "여름"))))) #임의의 순서
(P5_2 <- P5_0 + geom_bar(aes(fill = 계절), position = "fill"))
(P5_3 <- P5_0 + geom_bar(aes(fill = 계절), position = "dodge"))
(P5_4 <- P5_0 + geom_bar(aes(fill = 계절), position = "identity"))

#막대 색상 변경(매핑 아님)
 P5_0 <- ggplot(data = ks, mapping = aes(x = 날씨))
(P5_1 <- P5_0 + geom_bar(fill = "red"))

#선의 색과 선의 종류 매핑
P6_0 <- ggplot(data = ks_rain, mapping = aes(x = 월))
(P6_1 <- P6_0 + geom_line(aes(group = 도시, color = 도시), stat="count") + geom_point(aes(group = 도시, color = 도시), stat = "count"))	
(P6_2 <- P6_0 + geom_line(aes(group = 도시, color = 도시, linetype = 도시), stat = "count") + geom_point(aes(group = 도시, color = 도시), stat = "count"))	

#점의 색과 점의 종류 매핑
P7_0 <- ggplot(data = ks_temp,mapping = aes(x = 기온2014, y = 기온2015))
(P7_1 <- P7_0 + geom_point(aes(color = 계절, shape = 계절)))

#축에 관한 설정
P8_0 <- ggplot(data = ks, mapping = aes(x = 날씨)) + geom_bar(aes(fill = 계절))
P8_1 <- P8_0 + scale_y_continuous(limits = c(0,6000), breaks = seq(0, 6000, 1000))
P8_2 <- P8_1 + labs(x = "날씨의 종류", y = "도수")
(P8_3 <- P8_2 + theme(axis.text.x = element_text(size = 15), axis.title.y = element_text(size = 20)))

#범례에 관한 설정
keys <- c("봄", "여름", "가을", "겨울")
mycolor <- c("plum", "tomato", "wheat", "lemonchiffon"); names(mycolor) <- keys
P8_4 <- P8_3 + scale_fill_manual(values = mycolor)
# P8_4<-P8_3 + scale_fill_manual(values = mycolor,limits = rev(keys)) #범례항목 역순
P8_5 <- P8_4 + theme(legend.position = "bottom")
P8_6 <- P8_5 + labs(fill = "사계절")
(P8_7 <- P8_6 + guides(fill = guide_legend(nrow = 1, byrow = TRUE)))
	
#집계 데이터로부터의 막대그래프
ks_bar <- ks %>% 
	group_by(계절, 날씨) %>% 
	summarise(도수 = n()) %>% 
	complete(계절, 날씨, fill = list(도수 = 0)) %>% 
	as.data.frame() #결과를 쉽게 볼 수 있도록 데이터프레임화
(P9_0 <- ggplot(ks_bar, aes(x = 날씨, y = 도수)) + geom_bar(aes(fill = 계절), stat = "identity"))

#집계 데이터로부터의 꺾은 선 그래프
ks_line <- ks %>% 
	group_by(월, 도시) %>% 
	summarise(평균강수량 = mean(강수량)) %>% 
	as.data.frame()
P9_1 <- ggplot(ks_line, aes(x = 월, y = 평균강수량))
P9_2 <- P9_1 + geom_line(aes(group = 도시, color = 도시, linetype = 도시), stat = "identity")
(P9_3 <- P9_2 + geom_point(aes(group = 도시, color = 도시), stat = "identity"))
	
#문자정보를 부가한다（geom_text）
P10_1 <- ggplot(data = ks,mapping = aes(x = 날씨)) + geom_bar()
(P10_2 <- P10_1 + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5))

#분포의 모양을 알아본다（geom_density，geom_vline）
ks_mean_temp <- ks %>% 
	group_by(계절,도시) %>% 
	summarise(평균기온 = mean(기온)) %>% 
	as.data.frame()
P10_3 <- ggplot(data = ks, mapping = aes(x = 기온)) + geom_density(aes(linetype = 계절, color = 계절))
P10_4 <- P10_3 + geom_vline(data = ks_mean_temp, aes(xintercept = 평균기온, color = 계절), linetype = "twodash") #linetype의 다른 종류는"solid","longdash","dotted","dotdash","dashed","blank"
(P10_5 <- P10_4 + facet_wrap( ~ 도시))

#데이터의 산포도를 상세하게 조사한다（geom_jitter）
P10_6 <- ggplot(data = ks,mapping = aes(x = 도시, y = 풍속)) + geom_jitter(aes(color = 계절, group = 계절), position = position_jitterdodge(dodge.width = 0.6), alpha = 1 / 5) #position_jitterdodge(dodge.width = 0.6)로 점의 산포도 폭을 지정
(P10_7 <- P10_6 + stat_summary(aes(x = 도시, y = 풍속, group = 계절), color = "white", fun = median, geom = "point", shape = 4, position = position_dodge(width = 0.6))) #position_dodge(width = 0.6)로 dodge의 폭을 지정


#연습과 해답
#문1 해답
rtks <- read.csv("6도시의 기상.csv", colClasses = c(rep("factor", 6), rep("numeric", 3)))
rtks$도시 <- factor(rtks$도시, levels = c("가고시마", "다카마쓰", "가나자와", "나가노", "요코하마", "아오모리"))
rtks$월  <- factor(rtks$월, levels = as.character(1:12))
rtks$일  <- factor(rtks$일, levels = as.character(1:31))
rtks$계절 <- factor(rtks$계절, levels = c("봄", "여름", "가을", "겨울"))
rtks$오전날씨 <- factor(rtks$오전날씨, levels = c("쾌청", "맑음", "다소흐림", "흐림", "옅은안개", "안개", "안개비", "비", "진눈깨비", "눈", "우박", "천둥번개"))
rtks$오후날씨 <- factor(rtks$오후날씨, levels = c("쾌청", "맑음", "다소흐림", "흐림", "옅은안개", "안개", "안개비", "비", "진눈깨비", "눈", "우박", "천둥번개"))

#문2 해답
library(ggplot2)
ggplot(data = rtks, aes(x = 오전날씨)) + geom_bar()

#문3 해답
ggplot(data = rtks, aes(x = 계절, group = 1)) + stat_summary(aes(y = 기온), fun = mean, geom = "line") + stat_summary(aes(y = 기온), fun = mean, geom = "point")

#문4 해답
library(dplyr)
rtks2 <- rtks %>% 
	group_by(계절) %>% 
	summarise(평균기온 = mean(기온)) %>% 
	as.data.frame()

#문5 해답
ggplot(data = rtks2, aes(x = 계절, y = 평균기온, group = 1)) + geom_line(stat = "identity") + geom_point(stat = "identity")

#문6 해답
ggplot(data = rtks, aes(x = 오후날씨)) + geom_bar(aes(fill = 계절)) + facet_grid( ~ 도시)

