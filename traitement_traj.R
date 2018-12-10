library(data.table)
TrajDir="C:/Users/Yves Bas/Documents/chiroptera/Donnees"
TrajFiles=list.files(TrajDir,pattern="_TrajData.csv",recursive=T,full.names=T)
VarToConsider=c("NbPosit", "Disttot", "Distdir", "SdDist",  "AnglTot"
                ,"Vit"   ,  "Dens"  ,   "Straight" ,    "AnglMoy")

SampleY=2015
SampleS=c("10.10.16.1/stream1","10.10.16.1/stream2")

my.data=list()
for (i in 1:length(TrajFiles))
{
  my.data[[i]]=fread(TrajFiles[i])
}

TrajTot=rbindlist(my.data)
TrajS1=subset(TrajTot,(!is.na(TrajTot$AnglMoy))&(!is.na(TrajTot$Dens))
                 &(!is.na(TrajTot$AnglTot))&(!is.na(TrajTot$Sub)))
TrajS1$Year=substr(TrajS1$TimeB,1,4)

TrajVar=subset(TrajS1,select=VarToConsider)
TrajVar=scale(TrajVar)

fit <- kmeans(TrajVar,10)
barplot(table(TrajPurge$Year,fit$cluster))

barplot(table(fit$cluster,TrajPurge$Month))
barplot(table(fit$cluster,TrajPurge$Hour))
barplot(table(TrajPurge$Sub,fit$cluster),las=2)

for (i in 1:length(VarToConsider))
{
  VarSel=subset(TrajPurge,select=VarToConsider[i])
  boxplot(as.data.frame(VarSel)[,1]~fit$cluster,main=VarToConsider[i])
}

TrajPurge$clust1=fit$cluster

fwrite(TrajPurge,paste0("TrajPurge",substr(Sys.time(),1,10),".csv"))

TrajSamp=subset(TrajPurge,(TrajPurge$Year==SampleY)&(TrajPurge$Sub %in% SampleS))

TrajSel=TrajSamp[0,]

for (i in 1:nlevels(as.factor(TrajSamp$clust1)))
{
  TrajC=subset(TrajSamp,TrajSamp$clust1==levels(as.factor(TrajSamp$clust1))[i])
  SampleSize=min(3,nrow(TrajC))
  TrajCS=TrajC[sample(nrow(TrajC),SampleSize),]
  TrajSel=rbind(TrajSel,TrajCS)
}

TrajSel=TrajSel[order(TrajSel$TimeB),]

for (i in 1:nrow(TrajSel))
{
  DataCoord=fread(TrajSel$file[i])
  DataCoordT=subset(DataCoord,(DataCoord$V8==TrajSel$TimeB[i])&((DataCoord$V9==TrajSel$TimeE[i])))
  if(nrow(DataCoordT)>1) stop("pb doublon")
  CoordTemp=strsplit(as.character(DataCoordT$V10),split=",")
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
  plot(Xtemp,Ytemp,xlim=c(-1.5,1.5),ylim=c(-1,1),type="o"
       ,main=paste(DataCoordT$V8,TrajSel$Sub[i],TrajSel$clust1[i]))
    
  }

fwrite(TrajSel,paste0(TrajDir,"/TrajSel.csv"))

ClustSel=9

TrajS1=subset(TrajPurge,TrajPurge$clust1 %in% ClustSel)


TrajVar=subset(TrajS1,select=VarToConsider)
TrajVar=scale(TrajVar)

fit <- kmeans(TrajVar,6)
barplot(table(TrajS1$Year,fit$cluster))

barplot(table(fit$cluster,TrajS1$Month))
barplot(table(fit$cluster,TrajS1$Hour))
barplot(table(TrajS1$Sub,fit$cluster),las=2)

for (i in 1:length(VarToConsider))
{
  VarSel=subset(TrajS1,select=VarToConsider[i])
  boxplot(as.data.frame(VarSel)[,1]~fit$cluster,main=VarToConsider[i])
}

TrajS1$clust2=fit$cluster

fwrite(TrajS1,paste0("TrajS1",substr(Sys.time(),1,10),".csv"))

TrajSamp=subset(TrajS1,(TrajS1$Year==SampleY)&(TrajS1$Sub %in% SampleS))

TrajSel=TrajSamp[0,]

for (i in 1:nlevels(as.factor(TrajSamp$clust2)))
{
  TrajC=subset(TrajSamp,TrajSamp$clust2==levels(as.factor(TrajSamp$clust2))[i])
  SampleSize=min(3,nrow(TrajC))
  TrajCS=TrajC[sample(nrow(TrajC),SampleSize),]
  TrajSel=rbind(TrajSel,TrajCS)
}

TrajSel=TrajSel[order(TrajSel$TimeB),]

for (i in 1:nrow(TrajSel))
{
  DataCoord=fread(TrajSel$file[i])
  DataCoordT=subset(DataCoord,(DataCoord$V8==TrajSel$TimeB[i])&((DataCoord$V9==TrajSel$TimeE[i])))
  if(nrow(DataCoordT)>1) stop("pb doublon")
  CoordTemp=strsplit(as.character(DataCoordT$V10),split=",")
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
  plot(Xtemp,Ytemp,xlim=c(-1.5,1.5),ylim=c(-1,1),type="o"
       ,main=paste(DataCoordT$V8,TrajSel$Sub[i],TrajSel$clust2[i]))
  
}

fwrite(TrajSel,paste0(TrajDir,"/TrajSel2.csv"))
