#남녀별 교차집계표의 작성
bdat <- read.csv("자전거데이터.csv")#데이터의 입력

#연령과 제조사의 교차집계표
bdat$연령 <- as.character(bdat$연령)
tmpm <- table(bdat$연령,bdat$제조사)

mm <- matrix(bdat$도수[1:9],ncol=3,nrow=3) #남성의 교차집계표 작성
colnames(mm) <- colnames(tmpm)
rownames(mm) <- rownames(tmpm)
mm

fm <- matrix(bdat$도수[10:18],ncol=3,nrow=3) #여성의 교차집계표 작성 
colnames(fm) <- colnames(tmpm)
rownames(fm) <- rownames(tmpm)
fm


#함수glm을 이용한 포화모형의 분석
fullmodel <- glm(도수~연령*성별*제조사,data=bdat,family="poisson")

#포화모형의 출력
summary(fullmodel)

#함수glm을 이용한 독립모형의 분석
idmodel <- glm(도수~연령+성별+제조사,data=bdat,family="poisson")

#독립모형의 출력
summary(idmodel)

#독립모형의 분산분석표
anova(idmodel,fullmodel,test="Chisq")

#포화모형과 독립모형의 AIC와 BIC
extractAIC(fullmodel) #포화모형 AIC
extractAIC(idmodel) #독립모형 AIC
extractAIC(fullmodel,k=log(sum(bdat$도수))) #포화모형 BIC
extractAIC(idmodel,k=log(sum(bdat$도수))) #독립모형 BIC

#함수glm을 이용한 제안모형의 분석
bestmodel <- glm(도수~연령+성별+제조사+(연령:성별)+(연령:제조사),data=bdat,family="poisson")

#함수glm을 이용한 제안모형의 출력
summary(bestmodel)

#포화모형과 제안모형의 우도비검정
anova(bestmodel,fullmodel,test="Chisq")


#최량모형의 기대도수행렬
xtabs(bestmodel$fitted.values~bdat$연령+bdat$제조사+bdat$성별)



#포화모형을 기초로 한 기대도수행렬
xtabs(fullmodel$fitted.values~bdat$연령+bdat$제조사+bdat$성별)


#기준 셀의 설정
bdat$연령 <-factor(bdat$연령,levels=c("30대","20대","40대"))
bdat$성별 <-factor(bdat$성별,levels=c("M","F"))
bdat$제조사 <-factor(bdat$제조사,levels=c("코렉스","삼천리","알톤"))



#연습과 해답

#문1 해답
bdat2 <- read.csv("자전거데이터연습1.csv") 
bdat2$연령<- factor(bdat2$연령,levels=c("30대","20대","40대"))
bdat2$성별<-factor(bdat2$성별,levels=c("M","F"))
bdat2$제조사<-factor(bdat2$제조사,levels=c("엘파마","디엠","스마트"))


#문2 해답
fullmodel2 <- glm(도수~연령*성별*제조사,data=bdat2,family="poisson")
indmodel2 <- glm(도수~연령+성별+제조사,data=bdat2,family="poisson")

#문3 해답
anova(indmodel2,fullmodel2,test="Chisq")

#문4 해답
summary(fullmodel2)

#문5 해답
xtabs(fullmodel2$fitted.values~연령+제조사+성별,data=bdat2)

#문6 해답
(frate <- ((837/1266)/(649/442)))#여성의 비율
(mrate <- ((744/888)/(626/432)))#남성의 비율
log(frate/mrate)

