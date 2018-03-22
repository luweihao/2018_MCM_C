AZ=read.csv("AZ.csv")
AZ.info=read.csv("AZ.info.csv")
alpha=read.csv("alpha.csv", header = FALSE)
rownames(alpha)=c("CO2", "CO", "score")
colnames(alpha)=c(colnames(AZ[,-1]))

coefAZ=as.data.frame(matrix(0, 50, 18))
rownames(coefAZ)=1960:2009
colnames(coefAZ)=c(colnames(AZ[,-1]), "freedom")
for(i in 1:6){
    coefAZ[,i]=AZ.info[,17+i]/alpha[3,i]
}
for(i in 7:17){
    coefAZ[,i]=alpha[1,i]*alpha[2,i]*AZ.info[,i]/alpha[3,i]
}
for(j in 1:50){
    coefAZ[j,18]=6-sum(AZ.info[j,18:23]==0)-(11-sum(AZ.info[j,7:17]==0))
}

outputAZ=rep(0,50)
for(k in 1:50){
    outputAZ[k]=as.matrix(coefAZ[k,1:17]) %*% as.matrix(t(AZ[k,2:18]))
}
beta=0.25
inputAZ=1-beta+beta*(AZ.info[,2]/1000000) * (AZ.info[,4]/100)
CR.AZ=log(outputAZ)/inputAZ-coefAZ[,18]

