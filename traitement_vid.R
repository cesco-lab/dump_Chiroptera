#library(rpart)
library(data.table)
args="C:/Users/Yves Bas/Documents/chiroptera/Donnees/"
sub=c("10.10.16.1/stream1" ,"10.10.16.1/stream2" ,"10.10.16.6/stream1" 
      ,"10.10.16.6/stream2" )

extrtime <- function(x) 
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- substr(as.character(x), 1, nchar(as.character(x))-1)
  strptime(pretemps, "%Y%m%dT%H%M%OS")
}

angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}


for (h in 1:length(sub))
{


Logdir=paste0(args,sub[h])

#listing files 
Loglist=list.files(Logdir,pattern=".csv$",full.names=T,recursive=T)

#concatenating  tables
Sys.time()
my.data <- list()
for (i in 1:length(Loglist)){
  my.data[[i]] <- fread(Loglist[[i]],h=F,fill=T)
  my.data[[i]]$file=Loglist[[i]]
}

Sys.time()
traj=as.data.frame(rbindlist(my.data,fill=T))
Sys.time()

colnames(traj)=c("Time","C","Inc","Mode","Inc2","Inc3","Inc4","TimeBeg","TimeEnd","Coord","file")

MinRow=1
#MaxRow=50000
MaxRow=nrow(traj)

NbPosit=vector(length=MaxRow-MinRow)
Disttot=vector(length=MaxRow-MinRow)
Distdir=vector(length=MaxRow-MinRow) #distance between first and last point
SdDist=vector(length=MaxRow-MinRow)
AnglTot=vector(length=MaxRow-MinRow) #direction between first and last point
DecTA=vector(length=MaxRow-MinRow)#cumul de décalage d'angle
PasMax=vector() 
PasMed=vector()
XMax=vector()
XMin=vector()
XMean=vector()
XMed=vector()
YMax=vector()
YMin=vector()
YMean=vector()
YMed=vector()


Sys.time()



for (i in MinRow:MaxRow)
{
  CoordTemp=strsplit(as.character(traj$Coord[i]),split=",")
  j=0
  Xtemp=vector()
  Ytemp=vector()
  while (j<length(CoordTemp[[1]]))
    {
      j=j+2
      Xtemp=c(Xtemp,CoordTemp[[1]][j-1])
      Ytemp=c(Ytemp,CoordTemp[[1]][j])
  }
  Xtemp=as.numeric(Xtemp)
  Ytemp=as.numeric(Ytemp)
  if(i%%1000==0){
    print(paste(i,traj$Time[i],Sys.time()))
    plot(Xtemp,Ytemp,xlim=c(-1.5,1.5),ylim=c(-1,1),type="o",main=traj$Time[i])
    
    }
  XMax[i-MinRow+1]=max(Xtemp)
  XMin[i-MinRow+1]=min(Xtemp)
  XMean[i-MinRow+1]=mean(Xtemp)
  XMed[i-MinRow+1]=quantile(Xtemp,0.5)
  YMax[i-MinRow+1]=max(Ytemp)
  YMin[i-MinRow+1]=min(Ytemp)
  YMean[i-MinRow+1]=mean(Ytemp)
  YMed[i-MinRow+1]=quantile(Ytemp,0.5)
  
  
  
  NbPosit[i-MinRow+1]=length(Xtemp)
  Pas=vector()
  AnglInst=vector()
  DecTA[i-MinRow+1]=0
  for (k in 2:length(Xtemp))
  {
    Pas=c(Pas,((Xtemp[k]-Xtemp[k-1])^2+(Ytemp[k]-Ytemp[k-1])^2)^0.5)
    AnglInst=c(AnglInst,angle(t(as.matrix(c(Xtemp[k-1],Xtemp[k])))
                        ,as.matrix(c(Ytemp[k-1],Ytemp[k]))))
    if (k>2)
      {DecTA[i-MinRow+1]=DecTA[i-MinRow+1]+min(abs(AnglInst[k-1]-AnglInst[k-2])
                             ,abs(AnglInst[k-1]+pi-AnglInst[k-2])
                             ,abs(AnglInst[k-1]-AnglInst[k-2]-pi))}
  }
  PasMax[i-MinRow+1]=max(Pas)
  PasMed[i-MinRow+1]=quantile(Pas,0.5)
  AnglTot[i-MinRow+1]=angle(t(as.matrix(c(Xtemp[1],Xtemp[length(Xtemp)])))
                   ,as.matrix(c(Ytemp[1],Ytemp[length(Xtemp)])))
  
  Disttot[i-MinRow+1]=sum(Pas)
  Distdir[i-MinRow+1]=((Xtemp[length(Xtemp)]-Xtemp[1])^2+(Ytemp[length(Xtemp)]-Ytemp[1])^2)^0.5
  SdDist[i-MinRow+1]=sd(Pas)
}
Sys.time()

Dur=log(as.numeric(extrtime(traj$TimeEnd[MinRow:MaxRow]))-as.numeric(extrtime(traj$TimeBeg[MinRow:MaxRow]))+1)
Vit=Disttot/Dur
Dens=NbPosit/Dur
DateT=extrtime(traj$TimeBeg[MinRow:MaxRow])
Day=DateT$mday
Hour=DateT$hour
Month=DateT$mon

Straight=Distdir/Disttot
AnglMoy=DecTA/NbPosit

TrajData=as.data.frame(cbind(Sub=sub[h],TimeB=traj$TimeBeg,TimeE=traj$TimeEnd,NbPosit,Disttot,Distdir,SdDist,AnglTot,DecTA
                             ,PasMax,PasMed,XMax,XMin,XMean,XMed,YMax,YMin,YMean,YMed
                                   ,Dur,Vit,Dens,Day,Hour,Month,Straight,AnglMoy,file=traj$file))

fwrite(TrajData,paste0(args,sub[h],"_TrajData.csv"))

}



####JUNK######


TEB=subset(TrajData,XMed>(-0.7)&XMed<0.7)

plot(TEB$Distdir,TEB$SdDist)
TEB=subset(TEB,TEB$Distdir>0.8)
for (k in 1:ncol(TEB))
{
  boxplot(TEB[,k]~TEB$Hour,main=colnames(TEB)[k])
  
}

TEBo=subset(TEB,TEB$Hour>5&TEB$Hour<18)
TEBc=subset(TEB,(TEB$Hour>20)|(TEB$Hour<5))
Chiro=((TEB$Hour>19)|(TEB$Hour<6))

for (k in 1:ncol(TEB))
{
TTT=t.test(TEBo[,k],TEBc[,k])
#print(paste(colnames(TEB)[k],TTT$p.value))
}

TEBos=TEBo[sample(1000,replace=F),]
TEBcs=TEBc[sample(1000,replace=F),]


plot(TEBcs$AnglMoy,TEBcs$SdDist)
points(TEBos$AnglMoy,TEBos$SdDist,col=2)


plot(TEBcs$Vit,TEBcs$SdDist)
points(TEBos$Vit,TEBos$SdDist,col=2)



####

Arbre=rpart(Chiro~TEB$NbPosit+TEB$Disttot+TEB$Distdir+TEB$SdDist+TEB$AnglTot+TEB$DecTA
            +TEB$PasMax+TEB$PasMed+TEB$XMax+TEB$XMin+TEB$XMean+TEB$XMed+TEB$YMax+TEB$YMin+TEB$YMean+TEB$YMed
            +TEB$Dur+TEB$Vit+TEB$Dens+TEB$Sin+TEB$AnglMoy
            ,parms = list(prior = c(.25,.75))
            ,control = rpart.control(cp = 0.005))

par(mfrow = c(1,1), xpd = NA) # otherwise on some devices the text is clipped
plot(Arbre)
text(Arbre, use.n = TRUE,cex=1)


