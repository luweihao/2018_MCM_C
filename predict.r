AZ=read.csv("AZ.csv")
CA=read.csv("CA.csv")
NM=read.csv("NM.csv")
TX=read.csv("TX.csv")
AZ.info=read.csv("AZ.info.csv")
CA.info=read.csv("CA.info.csv")
NM.info=read.csv("NM.info.csv")
TX.info=read.csv("TX.info.csv")

alpha=read.csv("alpha.csv", header = FALSE)
rownames(alpha)=c("CO2", "CO", "score")
colnames(alpha)=c(colnames(AZ[,-1]))

coefAZ=as.data.frame(matrix(0, 91, 18))
rownames(coefAZ)=1960:2050
colnames(coefAZ)=c(colnames(AZ[,-1]), "freedom")
for(i in 1:6){
    coefAZ[,i]=AZ.info[,17+i]/alpha[3,i]
}
for(i in 7:17){
    coefAZ[,i]=alpha[1,i]*alpha[2,i]*AZ.info[,i]/alpha[3,i]
}
for(j in 1:91){
    coefAZ[j,18]=6-sum(AZ.info[j,18:23]==0)-(11-sum(AZ.info[j,7:17]==0))
}


coefCA=as.data.frame(matrix(0, 91, 18))
rownames(coefCA)=1960:2050
colnames(coefCA)=c(colnames(CA[,-1]), "freedom")
for(i in 1:6){
    coefCA[,i]=CA.info[,17+i]/alpha[3,i]
}
for(i in 7:17){
    coefCA[,i]=alpha[1,i]*alpha[2,i]*CA.info[,i]/alpha[3,i]
}
for(j in 1:91){
    coefCA[j,18]=6-sum(CA.info[j,18:23]==0)-(11-sum(CA.info[j,7:17]==0))
}


coefNM=as.data.frame(matrix(0, 91, 18))
rownames(coefNM)=1960:2050
colnames(coefNM)=c(colnames(NM[,-1]), "freedom")
for(i in 1:6){
    coefNM[,i]=NM.info[,17+i]/alpha[3,i]
}
for(i in 7:17){
    coefNM[,i]=alpha[1,i]*alpha[2,i]*NM.info[,i]/alpha[3,i]
}
for(j in 1:91){
    coefNM[j,18]=6-sum(NM.info[j,18:23]==0)-(11-sum(NM.info[j,7:17]==0))
}


coefTX=as.data.frame(matrix(0, 91, 18))
rownames(coefTX)=1960:2050
colnames(coefTX)=c(colnames(TX[,-1]), "freedom")
for(i in 1:6){
    coefTX[,i]=TX.info[,17+i]/alpha[3,i]
}
for(i in 7:17){
    coefTX[,i]=alpha[1,i]*alpha[2,i]*TX.info[,i]/alpha[3,i]
}
for(j in 1:91){
    coefTX[j,18]=6-sum(TX.info[j,18:23]==0)-(11-sum(TX.info[j,7:17]==0))
}


outputAZ=rep(0,91)
for(k in 1:91){
    outputAZ[k]=as.matrix(coefAZ[k,1:17]) %*% as.matrix(t(AZ[k,2:18]))
}
beta=0.25
inputAZ=1-beta+beta*(AZ.info[,2]/1000000) * (AZ.info[,4]/100)
CR.AZ=log(outputAZ)/inputAZ-coefAZ[,18]


outputCA=rep(0,91)
for(k in 1:91){
    outputCA[k]=as.matrix(coefCA[k,1:17]) %*% as.matrix(t(CA[k,2:18]))
}
beta=0.25
inputCA=1-beta+beta*(CA.info[,2]/1000000) * (CA.info[,4]/100)
CR.CA=log(outputCA)/inputCA-coefCA[,18]


outputNM=rep(0,91)
for(k in 1:91){
    outputNM[k]=as.matrix(coefNM[k,1:17]) %*% as.matrix(t(NM[k,2:18]))
}
beta=0.25
inputNM=1-beta+beta*(NM.info[,2]/1000000) * (NM.info[,4]/100)
CR.NM=log(outputNM)/inputNM-coefNM[,18]


outputTX=rep(0,91)
for(k in 1:91){
    outputTX[k]=as.matrix(coefTX[k,1:17]) %*% as.matrix(t(TX[k,2:18]))
}
beta=0.25
inputTX=1-beta+beta*(TX.info[,2]/1000000) * (TX.info[,4]/100)
CR.TX=log(outputTX)/inputTX-coefTX[,18]


write.csv(cbind(CR.AZ,CR.CA,CR.NM,CR.TX), "CR.pre.csv")