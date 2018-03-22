AZ0=read.csv("AZ0.csv")
CA0=read.csv("CA0.csv")
NM0=read.csv("NM0.csv")
TX0=read.csv("TX0.csv")
rownames(AZ0)=1960:2009
rownames(CA0)=1960:2009
rownames(NM0)=1960:2009
rownames(TX0)=1960:2009
AZ=AZ0[,c(28,198,208,383,530,606,8,20,78,111,216,244,270,315,361,507,594)]
AZ.info=AZ0[,c(192,384,539,549,564,9,21,79,112,217,245,271,316,362,508,595)]
CA=CA0[,c(28,198,208,383,530,606,8,20,78,111,216,244,270,315,361,507,594)]
CA.info=CA0[,c(192,384,539,549,564,9,21,79,112,217,245,271,316,362,508,595)]
NM=NM0[,c(28,198,208,383,530,606,8,20,78,111,216,244,270,315,361,507,594)]
NM.info=NM0[,c(192,384,539,549,564,9,21,79,112,217,245,271,316,362,508,595)]
TX=TX0[,c(28,198,208,383,530,606,8,20,78,111,216,244,270,315,361,507,594)]
TX.info=TX0[,c(192,384,539,549,564,9,21,79,112,217,245,271,316,362,508,595)]


for(i in 40:50){
    if(!AZ$RFTCB[i]){
        AZ$RFTCB[i]=mean(AZ$RFTCB[max(0,i-5):min(50,i+5)])
    }
}
for(i in 40:50){
    if(!AZ.info$RFTCD[i]){
        AZ.info$RFTCD[i]=mean(AZ.info$RFTCD[max(0,i-5):min(50,i+5)])
    }
}

write.csv(AZ, "AZ.csv")
write.csv(CA, "CA.csv")
write.csv(NM, "NM.csv")
write.csv(TX, "TX.csv")
write.csv(AZ.info, "AZ.info.csv")
write.csv(CA.info, "CA.info.csv")
write.csv(NM.info, "NM.info.csv")
write.csv(TX.info, "TX.info.csv")
