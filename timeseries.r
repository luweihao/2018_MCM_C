TX=read.csv("TX.csv")
TX.info=read.csv("TX.info.csv")

TX.info=TX.info[1:91,1:23]
x=1:50
GDP=TX.info[x,2]
k=TX.info[x,5]
l=TX.info[x,6]

ly=log(GDP)
lk=log(k)
ll=log(l)
fit1=lm(ly~lk+ll)
summary(fit1)
beta=coef(fit1)
c(exp(beta[1]), beta[2], beta[3])
#Approximate coeffcient for iteration

nls1=nls(GDP~A*((k^a)*(l^b)), start=list(A=3.123709e-05,a=5.905826e-01,
         b=1.447383e+00), lower=c(0,0,0),upper=c(1000,100,100),
         algorithm="port", control=nls.control(maxiter=1000,tol=1e-1000))
summary(nls1)
SE=deviance(nls1)
SST=sum((GDP-mean(GDP))^2)
Rsquare=1-SE/SST
Rsquare

yy=c(GDP, predict(nls1,list(k=TX.info[51:91,5], l=TX.info[51:91,6])))
plot(yy)
TX.info[,2]=yy





