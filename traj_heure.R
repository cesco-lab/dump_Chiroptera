library(data.table)
library(ggeffects)
library(ggplot2)
library(StreamMetabolism)
library(glmmTMB)

TrajDir="C:/Users/Yves Bas/Documents/chiroptera/Donnees"
TrajS1=fread("TrajS12018-10-08.csv")
TrajTot=fread("TrajPurge2018-10-08.csv")
DatePb=c("2014-06-05")
Meteo=fread("C:/Users/Yves Bas/Documents/chiroptera/Meteo.csv")
Meteo=subset(Meteo,Meteo$`EPN1-REPO0043`<45)

f2p <- function(x) #get date-time data
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- substr(x,1,19)
  strptime(pretemps, "%Y%m%dT%H%M%OS",tz="UTC")-0
}

f2pM <- function(x) #get date-time data
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  strptime(x, "%d/%m/%Y %H:%M",tz="UTC")-0
}


TrajTot$Date=substr(TrajTot$TimeB,1,8)
TrajS1$Date=substr(TrajS1$TimeB,1,8)
TrajS1$TimeBd=sapply(TrajS1$TimeB,FUN=f2p) #30 sec
pourDateNuit=TrajS1$TimeBd-12*3600 #bricolage-décalage de 12 heures pour ramener à la date du début de nuit
Sys.time()
TrajS1$DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night
Sys.time()
TrajS1$DateJour=as.Date.POSIXct(TrajS1$TimeBd) # date (UTC+0)
TrajS1$DateLP=as.factor(TrajS1$DateJour)

#DateEch=levels(as.factor(TrajTot$Date))

#DateCamEch=aggregate(TrajTot$NbPosit
 #                    ,by=c(list(TrajTot$Sub),list(TrajTot$Date))
  #                                               ,FUN=length)

#test=aggregate(DateCamEch$Group.1,by=list(DateCamEch$Group.2),FUN=length)

DateCamEch=aggregate(TrajS1$NbPosit
                     ,by=c(list(TrajS1$Sub),list(TrajS1$DateJour))
                     ,FUN=length)

DateCamEch=subset(DateCamEch,!(as.character(DateCamEch$Group.2) %in% DatePb))

DateEch=levels(as.factor(TrajS1$DateJour))
DateEch=subset(DateEch,!(DateEch %in% DatePb))

Sys.time()
Srst=mapply(sunrise.set,48.51,5.61,DateEch) #3 sec
Sys.time()
SrstD=as.data.frame(t(Srst))
DataSrst=as.data.frame(cbind(DateEch,SrstD))

TrajS1sun=merge(TrajS1,DataSrst,by.x="DateLP",by.y="DateEch")

Sys.time()
TrajS1sun$Decst=TrajS1sun$TimeBd-as.numeric(TrajS1sun$sunset)
Sys.time()
#recale par rapport au coucher de soleil du bon jour
TrajS1sun$DecstP=TrajS1sun$Decst+3600*24*(TrajS1sun$Decst<(-6*3600))  # 2 min

Sys.time()
TrajS1sun$Decsr=TrajS1sun$TimeBd-as.numeric(TrajS1sun$sunrise)
Sys.time()
#recale par rapport au coucher de soleil du bon jour
TrajS1sun$DecsrP=TrajS1sun$Decsr+3600*24*(TrajS1sun$Decsr<(-6*3600))  # 2 min

TrajS1sun$Nuit=((TrajS1sun$Decsr<0)|(TrajS1sun$Decst>0))

#fit Date
Hour=c(0:23)

Date_1=vector()
Hour_1=vector()
NbTraj_1=vector()
Cam_1=vector()
for (i in 1:nrow(DateCamEch))
{
  TrajD=subset(TrajS1sun,(TrajS1sun$DateJour==DateCamEch$Group.2[i])&
                 (TrajS1sun$Sub==DateCamEch$Group.1[i]))
  for (j in 1:length(Hour))
  {
    Date_1=c(Date_1,DateCamEch$Group.2[i])
    Hour_1=c(Hour_1,Hour[j])
    testH=(TrajD$Hour==Hour[j])
    TrajDH=subset(TrajD,testH)
    NbTraj_1=c(NbTraj_1,nrow(TrajDH))  
    Cam_1=c(Cam_1,DateCamEch$Group.1[i])
  }
print(DateCamEch$Group.2[i])
}  

DataH=data.frame(cbind(Date_1,Hour_1,NbTraj_1,Cam_1))
boxplot(as.numeric(as.character(DataH$NbTraj_1))~
          as.numeric(as.character(DataH$Hour_1)))
#subset(DataH,as.numeric(as.character(DataH$NbTraj_1))>600)
#test=subset(TrajS1sun,TrajS1sun$Date=="20160719")

DataH$NbTraj_1N=as.numeric(as.character(DataH$NbTraj_1))
DataH$Hour_1N=as.numeric(as.character(DataH$Hour_1))


m1=glm(NbTraj_1N~poly(Hour_1N,9)
                  ,data=DataH)

pr1 <- ggpredict(m1, c(terms = "Hour_1N"),pretty = FALSE)

print(ggplot(pr1, aes(x, predicted)) +
        geom_line()  +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Hour") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


Date_1=vector()
NbTraj_1=vector()
JJulien_1=vector()
Nuit_1=vector()
Cam_1=vector()
for (i in 1:nrow(DateCamEch))
{
  TrajD=subset(TrajS1sun,(TrajS1sun$DateJour==DateCamEch$Group.2[i])&
                 (TrajS1sun$Sub==DateCamEch$Group.1[i]))
  for (j in 0:1)
  {
    Date_1=c(Date_1,DateCamEch$Group.2[i])
    testH=(TrajD$Nuit==j)
    TrajDH=subset(TrajD,testH)
    NbTraj_1=c(NbTraj_1,nrow(TrajDH))  
    JJulien_1=c(JJulien_1,yday(DateCamEch$Group.2[i]))
    Nuit_1=c(Nuit_1,j)
    Cam_1=c(Cam_1,DateCamEch$Group.1[i])
  }
  print(DateCamEch$Group.2[i])
}  

DataH=data.frame(cbind(Date_1,NbTraj_1,JJulien_1,Nuit_1,Cam_1))

DataH$NbTraj_1N=as.numeric(as.character(DataH$NbTraj_1))
DataH$JJulien_1N=as.numeric(as.character(DataH$JJulien_1))
DataH$JJulien_1Nscale=scale(DataH$JJulien_1N)
DataH$DateP=format(as.Date(Date_1))


m1=glmmTMB(NbTraj_1N~JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
             I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
             I(JJulien_1Nscale^7)
       ,data=DataH,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale"),full.data=T)

pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line()  +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

m1=glmmTMB(NbTraj_1N~(JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
             I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
             I(JJulien_1Nscale^7))*Nuit_1
       ,data=DataH,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale","Nuit_1"),full.data=T)
pr1$Nuit=pr1$group
pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = Nuit),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

#date by cameras
DataHN=subset(DataH,DataH$Nuit==1)

m1=glmmTMB(NbTraj_1N~(JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
                        I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
                        I(JJulien_1Nscale^7))*Cam_1
           ,data=DataHN,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale","Cam_1"),full.data=T)
pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

#date by year
DataHN$Year=substr(DataHN$DateP,1,4)

m1=glmmTMB(NbTraj_1N~(JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
                        I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
                        I(JJulien_1Nscale^7))*Year
           ,data=DataHN,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale","Year"),full.data=T)
pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

#date and speed
TrajS1N=subset(TrajS1sun,TrajS1sun$Nuit==1)
SpeedQ=quantile(TrajS1N$Vit,c(0,0.25,0.5,0.75,1))

Date_1=vector()
NbTraj_1=vector()
JJulien_1=vector()
Cam_1=vector()
SpeedC_1=vector()
for (i in 1:nrow(DateCamEch))
{
  TrajD=subset(TrajS1N,(TrajS1N$DateJour==DateCamEch$Group.2[i])&
                 (TrajS1N$Sub==DateCamEch$Group.1[i]))
  for (j in 1:4)
  {
    Date_1=c(Date_1,DateCamEch$Group.2[i])
    testH=((TrajD$Vit>=SpeedQ[j])&(TrajD$Vit<(SpeedQ[j+1]+0.001)))
    TrajDH=subset(TrajD,testH)
    NbTraj_1=c(NbTraj_1,nrow(TrajDH))  
    JJulien_1=c(JJulien_1,yday(DateCamEch$Group.2[i]))
    Cam_1=c(Cam_1,DateCamEch$Group.1[i])
    SpeedC_1=c(SpeedC_1,j)
    }
  print(DateCamEch$Group.2[i])
}  

DataH=data.frame(cbind(Date_1,NbTraj_1,JJulien_1,Cam_1,SpeedC_1))

DataH$NbTraj_1N=as.numeric(as.character(DataH$NbTraj_1))
DataH$JJulien_1N=as.numeric(as.character(DataH$JJulien_1))
DataH$JJulien_1Nscale=scale(DataH$JJulien_1N)
DataH$DateP=format(as.Date(Date_1))

m1=glmmTMB(NbTraj_1N~(JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
                        I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
                        I(JJulien_1Nscale^7))*SpeedC_1
           ,data=DataH,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale","SpeedC_1"),full.data=T)
pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

#date and straightness
StraightQ=quantile(TrajS1N$Straight,c(0,0.25,0.5,0.75,1))

Date_1=vector()
NbTraj_1=vector()
JJulien_1=vector()
Cam_1=vector()
StraightC_1=vector()
for (i in 1:nrow(DateCamEch))
{
  TrajD=subset(TrajS1N,(TrajS1N$DateJour==DateCamEch$Group.2[i])&
                 (TrajS1N$Sub==DateCamEch$Group.1[i]))
  for (j in 1:4)
  {
    Date_1=c(Date_1,DateCamEch$Group.2[i])
    testH=((TrajD$Straight>=StraightQ[j])&(TrajD$Straight<(StraightQ[j+1]+0.0001)))
    TrajDH=subset(TrajD,testH)
    NbTraj_1=c(NbTraj_1,nrow(TrajDH))  
    JJulien_1=c(JJulien_1,yday(DateCamEch$Group.2[i]))
    Cam_1=c(Cam_1,DateCamEch$Group.1[i])
    StraightC_1=c(StraightC_1,j)
  }
  print(DateCamEch$Group.2[i])
}  

DataH=data.frame(cbind(Date_1,NbTraj_1,JJulien_1,Cam_1,StraightC_1))

DataH$NbTraj_1N=as.numeric(as.character(DataH$NbTraj_1))
DataH$JJulien_1N=as.numeric(as.character(DataH$JJulien_1))
DataH$JJulien_1Nscale=scale(DataH$JJulien_1N)
DataH$DateP=format(as.Date(Date_1))

m1=glmmTMB(NbTraj_1N~(JJulien_1Nscale+I(JJulien_1Nscale^2)+I(JJulien_1Nscale^3)+
                        I(JJulien_1Nscale^4)+I(JJulien_1Nscale^5)+I(JJulien_1Nscale^6)+
                        I(JJulien_1Nscale^7))*StraightC_1
           ,data=DataH,family=nbinom2)

pr1 <- ggpredict(m1, c(terms = "JJulien_1Nscale","StraightC_1"),full.data=T)
pr1$x=pr1$x*sd(DataH$JJulien_1N)+mean(DataH$JJulien_1N)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Date") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#temperature
Meteo$DateP=sapply(Meteo$DATE,FUN=f2pM) #? sec
Meteo$DateJour=as.Date.POSIXct(Meteo$DateP) # date (UTC+0)
Meteo$DateLP=as.factor(Meteo$DateJour)
Meteo$Jour=yday(Meteo$DateJour)
Meteo$Year=year(Meteo$DateJour)
Meteo$Month=month(Meteo$DateJour)
boxplot(Meteo$`EPN1-REPO0043`~(Meteo$Year+Meteo$Month),las=2)
boxplot(Meteo$`EPN1-REPO0038`~(Meteo$Year+Meteo$Month),las=2)

DateM=levels(as.factor(Meteo$DateJour))

Sys.time()
Srst=mapply(sunrise.set,48.51,5.61,DateM) #4 sec
Sys.time()
SrstD=as.data.frame(t(Srst))
DataSrst=as.data.frame(cbind(DateM,SrstD))

Meteosun=merge(Meteo,DataSrst,by.x="DateLP",by.y="DateM")

Sys.time()
Meteosun$Decst=Meteosun$DateP-as.numeric(Meteosun$sunset)
Sys.time()
Meteosun$Decsr=Meteosun$DateP-as.numeric(Meteosun$sunrise)
Sys.time()
#recale par rapport au coucher de soleil du bon jour


Meteosun$Nuit=((Meteosun$Decsr<0)|(Meteosun$Decst>0))

MeteoNuit=subset(Meteosun,Meteosun$Nuit)

#calcul anomalie température
MeteoNuit$Mois=substr(MeteoNuit$DATE,4,5)
MeteoNuit$Hour=substr(MeteoNuit$DATE,12,13)

NormTemp=aggregate(MeteoNuit$`EPN1-REPO0043`
                   ,by=c(list(MeteoNuit$Mois),list(MeteoNuit$Hour))
                   ,FUN=mean)

MeteoNuitN=merge(MeteoNuit,NormTemp
                 ,by.x=c("Mois","Hour"),by.y=c("Group.1","Group.2"))

MeteoNuitE=subset(MeteoNuitN,MeteoNuitN$DateJour %in% DateCamEch$Group.2)
MeteoNuitE$Time10m=paste0(substr(MeteoNuitE$DATE,7,10)
                         ,substr(MeteoNuitE$DATE,4,5)
                         ,substr(MeteoNuitE$DATE,1,2),"T"
                         ,substr(MeteoNuitE$DATE,12,13)
                         ,substr(MeteoNuitE$DATE,15,15))
TrajS1$Time10m=substr(TrajS1$TimeB,1,12)

NbTraj10m=aggregate(TrajS1$TimeB,by=c(list(TrajS1$Sub),list(TrajS1$Time10m)),FUN=length)

TrajMeteo=merge(MeteoNuitE,NbTraj10m,by.x="Time10m",by.y="Group.2",all.x=T)
TrajMeteo$x.y[is.na(TrajMeteo$x.y)]=0
TrajMeteo$AnomTemp=TrajMeteo$`EPN1-REPO0043`-TrajMeteo$x.x
TrajMeteo$AnomTempscale=scale(TrajMeteo$AnomTemp)




#temp
TrajMeteo$Temp=TrajMeteo$`EPN1-REPO0043`
TrajMeteo$Tempscale=scale(TrajMeteo$Temp)
m1=glmmTMB(x.y~(Tempscale+I(Tempscale^2)+I(Tempscale^3)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "Tempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = "Tempscale [all]" ))

pr1$x=pr1$x*sd(TrajMeteo$Temp)+mean(TrajMeteo$Temp)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line()+
        #       geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Temperature") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)



#anomalie temp
m1=glmmTMB(x.y~(AnomTempscale+I(AnomTempscale^2)+I(AnomTempscale^3)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "AnomTempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = "AnomTempscale [all]"))

pr1$x=pr1$x*sd(TrajMeteo$AnomTemp)+mean(TrajMeteo$AnomTemp)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line()+
         #       geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Anomalie") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#wind
TrajMeteo$Wind=TrajMeteo$`EPN1-REPO0038`
TrajMeteo$Windscale=scale(TrajMeteo$Wind)
m1=glmmTMB(x.y~(Windscale+I(Windscale^2)+I(Windscale^3)+I(Windscale^4)
                +I(Windscale^5)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "Windscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = "Windscale [all]" ))

pr1$x=pr1$x*sd(TrajMeteo$Wind)+mean(TrajMeteo$Wind)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line()+
        #       geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Wind") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)

#wind direction
TrajMeteo$WindDir=TrajMeteo$`EPN1-REPO0039`
TrajMeteo$WindDirscale=scale(TrajMeteo$WindDir)
m1=glmmTMB(x.y~(WindDirscale+I(WindDirscale^2)+I(WindDirscale^3)+I(WindDirscale^4)
                +I(WindDirscale^5)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "WindDirscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = "WindDirscale [all]" ))

pr1$x=pr1$x*sd(TrajMeteo$WindDir)+mean(TrajMeteo$WindDir)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line()+
        #       geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("WindDir") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#wind direction * wind
m1=glmmTMB(x.y~(WindDirscale+I(WindDirscale^2)+I(WindDirscale^3)+I(WindDirscale^4)
                +I(WindDirscale^5))*(Windscale+I(Windscale^2)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "WindDirscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("WindDirscale [all]","Windscale" )))

pr1$x=pr1$x*sd(TrajMeteo$WindDir)+mean(TrajMeteo$WindDir)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("WindDir") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#wind direction * temperature
m1=glmmTMB(x.y~(WindDirscale+I(WindDirscale^2)+I(WindDirscale^3)+I(WindDirscale^4)
                +I(WindDirscale^5))*(AnomTempscale+I(AnomTempscale^2)),data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "WindDirscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("WindDirscale [all]","AnomTempscale" )))

pr1$x=pr1$x*sd(TrajMeteo$WindDir)+mean(TrajMeteo$WindDir)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="blue",mid="green",high="red") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("WindDir") + 
        ylab("NbTraj") +
        scale_fill_discrete()+
        theme_bw(base_size = 13)
)


#speed
TrajS1$Slow=(TrajS1$Vit<quantile(TrajS1$Vit,0.25))
TrajS1$SlowF=as.factor(as.numeric(TrajS1$Slow))
NbTraj10m=aggregate(TrajS1$TimeB,by=c(list(TrajS1$Sub),list(TrajS1$Time10m),list(TrajS1$SlowF)),FUN=length)

TrajMeteo=merge(MeteoNuitE,NbTraj10m,by.x="Time10m",by.y="Group.2",all.x=T)
TrajMeteo$x.y[is.na(TrajMeteo$x.y)]=0
TrajMeteo$AnomTemp=TrajMeteo$`EPN1-REPO0043`-TrajMeteo$x.x
TrajMeteo$AnomTempscale=scale(TrajMeteo$AnomTemp)

#temp*speed
TrajMeteo$Temp=TrajMeteo$`EPN1-REPO0043`
TrajMeteo$Tempscale=scale(TrajMeteo$Temp)

m1=glmmTMB(x.y~(Tempscale+I(Tempscale^2)+I(Tempscale^3))*Group.3
           ,data=TrajMeteo,family=nbinom2)


#pr1 <- ggpredict(m1, c(terms = "Tempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("Tempscale [all]","Group.3" )))

pr1$x=pr1$x*sd(TrajMeteo$Temp)+mean(TrajMeteo$Temp)


print(ggplot(pr1, aes(x, predicted)) +
         geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Temperature") + 
        ylab("NbTraj") +
        scale_x_continuous(limits = c(12, 33)) +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)



#anomalie temp * speed
m1=glmmTMB(x.y~(AnomTempscale+I(AnomTempscale^2)
                +I(AnomTempscale^3))*
             Group.3
,data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "AnomTempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("AnomTempscale [all]","Group.3")))

pr1$x=pr1$x*sd(TrajMeteo$AnomTemp)+mean(TrajMeteo$AnomTemp)


print(ggplot(pr1, aes(x, predicted)) +
               geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Anomalie") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#wind * speed
TrajMeteo$Wind=TrajMeteo$`EPN1-REPO0038`
TrajMeteo$Windscale=scale(TrajMeteo$Wind)
m1=glmmTMB(x.y~(Windscale+I(Windscale^2)+I(Windscale^3)+I(Windscale^4)
                +I(Windscale^5)+I(Windscale^6)+I(Windscale^7))*Group.3,data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "Windscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("Windscale [all]","Group.3" )))

pr1$x=pr1$x*sd(TrajMeteo$Wind)+mean(TrajMeteo$Wind)


print(ggplot(pr1, aes(x, predicted)) +
        #geom_line()+
               geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Wind") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#sinuosity
TrajS1$Sinuous=(TrajS1$Straight<quantile(TrajS1$Straight,0.75))
TrajS1$SinuousF=as.factor(as.numeric(TrajS1$Sinuous))
NbTraj10m=aggregate(TrajS1$TimeB,by=c(list(TrajS1$Sub),list(TrajS1$Time10m),list(TrajS1$SinuousF)),FUN=length)

TrajMeteo=merge(MeteoNuitE,NbTraj10m,by.x="Time10m",by.y="Group.2",all.x=T)
TrajMeteo$x.y[is.na(TrajMeteo$x.y)]=0
TrajMeteo$AnomTemp=TrajMeteo$`EPN1-REPO0043`-TrajMeteo$x.x
TrajMeteo$AnomTempscale=scale(TrajMeteo$AnomTemp)

#temp*sinuosity
TrajMeteo$Temp=TrajMeteo$`EPN1-REPO0043`
TrajMeteo$Tempscale=scale(TrajMeteo$Temp)

m1=glmmTMB(x.y~(Tempscale+I(Tempscale^2)+I(Tempscale^3)+
                  I(Tempscale^4)+I(Tempscale^5))*Group.3
           ,data=TrajMeteo,family=nbinom2)


#pr1 <- ggpredict(m1, c(terms = "Tempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("Tempscale [all]","Group.3" )))

pr1$x=pr1$x*sd(TrajMeteo$Temp)+mean(TrajMeteo$Temp)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Temperature") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)



#anomalie temp * sinuosity
m1=glmmTMB(x.y~(AnomTempscale+I(AnomTempscale^2)
                +I(AnomTempscale^3))*
             Group.3
           ,data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "AnomTempscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("AnomTempscale [all]","Group.3")))

pr1$x=pr1$x*sd(TrajMeteo$AnomTemp)+mean(TrajMeteo$AnomTemp)


print(ggplot(pr1, aes(x, predicted)) +
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Anomalie") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


#wind * sinuosity
TrajMeteo$Wind=TrajMeteo$`EPN1-REPO0038`
TrajMeteo$Windscale=scale(TrajMeteo$Wind)
m1=glmmTMB(x.y~(Windscale+I(Windscale^2)+I(Windscale^3)
                )*Group.3,data=TrajMeteo,family=nbinom2)

#pr1 <- ggpredict(m1, c(terms = "Windscale"),full.data=T)
pr1 <- ggpredict(m1, c(terms = c("Windscale [all]","Group.3" )))

pr1$x=pr1$x*sd(TrajMeteo$Wind)+mean(TrajMeteo$Wind)


print(ggplot(pr1, aes(x, predicted)) +
        #geom_line()+
        geom_line(aes(color = group),size=1)  +
        #scale_color_gradient2(low="red",high="blue") +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        xlab("Wind") + 
        ylab("NbTraj") +
        scale_fill_discrete(guide=FALSE)+
        theme_bw(base_size = 13)
)


