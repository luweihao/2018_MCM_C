TX=read.csv("TX.csv")
TX.info=read.csv("TX.info.csv")
coe.TX=read.csv("coe.TX.csv")
coe.TX=coe.TX[,-1]

index=37:50
#调前后数字 第二个
i=6
#i从1到6

y=TX[index,i+1]
p=TX.info[index,i+17]/TX.info[index,24]
plot(p, y)
text(p, y)
ly=log(1/y)
lp=log(p)
fit1=lm(ly~lp)
summary(fit1)
beta=coef(fit1)
c(exp(-beta[1]), beta[2])

nls1=nls(y~a/(p^b), start=list(a=1.031515e-12 ,b=4.731557e+00),
         algorithm="port", control=nls.control(maxiter=1000,tol=1e-1000))
summary(nls1)
SE=deviance(nls1)
SST=sum((y-mean(y))^2)
Rsquare=1-SE/SST
Rsquare


coe.TX[,i+11]=c(1.031515e-12 ,4.731557e+00)



write.csv(coe.TX, "coe.TX.csv")

kk=seq(2e-04,1e-03,1e-04)
a=3.404e+04
b=2.824e-01
lines(kk,a/(kk^b), col="red")

