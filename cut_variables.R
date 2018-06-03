mydata <- read.csv(file = "~/desktop/upmc/MYHAT_executive.csv")

names <- c(6,8:15,21,23:25)
mydata[,names] <- lapply(mydata[,names],cut,breaks=c(-0.1,0.1,2),labels = c("low","high"))
#str(mydata)

mydata$SEXF.1 <- as.factor(mydata$SEXF.1)

mydata$BL_Age.1<-cut(mydata$BL_Age.1, c(64,74.5,84.5,200), labels=c("y","m","o"))
mydata$EDU_YEARS.1 <- cut(mydata$EDU_YEARS.1, c(-0.1,11.5,12.5,200), labels=c("low","mid","high"))
mydata$mednum_c1 <- cut(mydata$mednum_c1,c(-0.1,2.9,200), labels=c("low","high"))
mydata$IADLscore_c1 <- cut(mydata$IADLscore_c1,c(-0.1,0.1,200), labels=c("low","high"))
mydata$smc_c1<-cut(mydata$smc_c1,c(-0.1,0.1,200), labels=c("low","high"))
mydata$mCESDscore.1 <- cut(mydata$mCESDscore.1,c(-0.1,4.9,200), labels=c("low","high"))

#sleep <- subset(mydata,select=c(fallasl1:sleepday1))
mydata$fallasl1 <- cut(mydata$fallasl1,c(-0.1,1.5,200), labels=c("low","high"))
mydata$backasl1 <- cut(mydata$backasl1,c(-0.1,1.5,200), labels=c("low","high"))
mydata$wakearl1 <- cut(mydata$wakearl1,c(-0.1,1.5,200), labels=c("low","high"))
mydata$sleepday1 <- cut(mydata$sleepday1,c(-0.1,1.5,200), labels=c("low","high"))

mydata$smokeyr1 <- cut(mydata$smokeyr1,c(-100,0.1,200), labels=c("low","high"))
mydata$smokenow1 <- cut(mydata$smokenow1,c(-100,0.1,200), labels=c("low","high"))

mydata$DRINK <- cut(mydata$DRINK,c(-100,0.1,1.1,3), labels=c("low","mid","high"))

mydata$subjhealth_c1[mydata$subjhealth_c1<1] <- NA
mydata$subjhealth_c1 <- cut(mydata$subjhealth_c1,c(-0.1,1.1,2.1,3.1,4.1,5.1), labels=c("1","2","3","4","5"))
#mydata$DRINK <- cut(mydata$DRINK,c(-10,0.1,1.1,2.1,3.1,4.1,5.1), labels=c("0","1","2"))

mydata<-na.omit(mydata)
mydata2 <- subset(mydata,select = -c(SEXF.1,BL_Age.1,EDU_YEARS.1))
str(mydata2)

###### Without sleeping fields; if with 3 baselines, please use first two lines; else last two lines.
#mydatanosleep <- subset(mydata,select = -c(18:24))
#str(mydatanosleep)
mydatanosleep2 <- subset(mydata,select = -c(SEXF.1,BL_Age.1,EDU_YEARS.1,18:24))
str(mydatanosleep2)

###### if you want to select sex age edu into the analysis, use mydata on x; else use mydata2 on x; also apply for the nosleep area (for sleep/sleep2)
is.fact <- sapply(mydata2,is.factor)
factors.df <- mydata2[,is.fact]


xfactors<-model.matrix(mydata$CYCLE~ ., data = factors.df)[,-1]

isnot.fact <- mydata[c(27:31,35:36)]
x <- as.matrix(data.frame(isnot.fact,xfactors))

library(glmnet)
fitlasso <- cv.glmnet(x,mydata2$CYCLE,alpha=1)
plot(fitlasso)
bestlam<-cv.glmnet(x,mydata2$CYCLE,alpha=1)$lambda.min*1.5
lasso.coef <- coef(fitlasso,s=bestlam)
lasso.coef
sink('sink-closing.txt')
min <- lm(CYCLE~SEXF.1+EDU_YEARS.1+BL_Age.1,data=mydata)
max <- formula(lm(CYCLE~.,data=mydata))
step.forward.model<-step(min,direction = "forward",scope = max)
summary(step.forward.model)
sink()
write.csv(mydata,file = "~/desktop/upmc/MYHAT_executive_level.csv", row.names = FALSE)

