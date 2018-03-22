AZ=read.csv("AZ.csv")
CA=read.csv("CA.csv")
NM=read.csv("NM.csv")
TX=read.csv("TX.csv")
AZ.info=read.csv("AZ.info.csv")
CA.info=read.csv("CA.info.csv")
NM.info=read.csv("NM.info.csv")
TX.info=read.csv("TX.info.csv")
coe.CA=matrix(0,2,17)

index=11:50
#调前后数字 第二个
i=11
#i从1到11

y=CA[index,i+7]
p=CA.info[index,i+6]/CA.info[index,24]
plot(p, y)
text(p, y)
ly=log(1/y)
lp=log(p)
fit1=lm(ly~lp)
summary(fit1)
beta=coef(fit1)
c(exp(-beta[1]), beta[2])

nls1=nls(y~a/(p^b), start=list(a=3.603447e+04,b=2.323972e-01),
         algorithm="port", control=nls.control(maxiter=1000,tol=1e-1000))
summary(nls1)
SE=deviance(nls1)
SST=sum((y-mean(y))^2)
Rsquare=1-SE/SST
Rsquare


coe.CA[,i]=c(3.730e+04, 2.284e-01)



write.csv(coe.CA, "coe.CA.csv")

#kk=seq(0.001,0.003,0.0001)
#a=3.730e+04
#b=2.284e-01
#points(kk,a/(kk^b), col="red")
