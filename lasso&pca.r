library(lars)
AZ=read.csv("AZ.csv")
AZ.info=read.csv("AZ.info.csv")
AZdata=data.frame(AZ$CLTCB[1:50], AZ.info$GDPrate[1:50], AZ.info$Crude.oil.production[1:50], 
         AZ.info$CLTCD[1:50], AZ.info$TETCB[1:50])
AZdata=as.matrix(AZdata)
x=AZdata[,2:5]
y=AZdata[,1]
laa=lars(x, y) ##matrix
plot(laa)
summary(laa)
cva=cv.lars(x, y, K=10)
best=cva$index[which.min(cva$cv)]
coef=coef.lars(laa, mode="fraction", s=best)
min(laa$Cp)
coef1=coef.lars(laa, mode="step", s=5)


library(msgps)
al=msgps(x, y, penalty = "alasso", gamma=1, lambda = 0)
summary(al)





######PCA
library(psych)
fa.parallel(x, fa = "pc", n.iter = 100, 
            show.legend = FALSE, main = "Scree plot with parallel analysis")
mydata=as.data.frame(cbind(y, x))
pr1=princomp(~., data=mydata[,-1], cor=T)
summary(pr1,loadings = T)

#### principal components regression ##############
pre=predict(pr1)
mydata$F1=pre[,1]
lm.sol=lm(y~F1, data=mydata)
summary(lm.sol)

#### transformation ###############################
beta=coef(lm.sol)
A=loadings(pr1)
x.bar=pr1$center
x.sd=pr1$scale
if(length(beta)==2){
    coef=(beta[2]*A[,1])/x.sd
}
if(length(beta)>=3){
    coef=beta[2]*A[,1]
    for(i in 2:(length(beta)-1)){
        coef=coef + beta[i+1]*A[,i]
    }
    coef=coef/x.sd
}
beta0=beta[1]-sum(x.bar*coef)
cc=c(beta0, coef)
pred1=cbind(1, AZ.info$GDPrate[51:91], AZ.info$Crude.oil.production[51:91], 
                  AZ.info$CLTCD[51:91], AZ.info$TETCB[51:91])
pred2=c(y,pred1 %*% cc)
plot(pred2)
plot(y)
(pred1 %*% cc)/209.57935
